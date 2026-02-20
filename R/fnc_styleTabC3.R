#' @title styleTabC3.
#' @description Prepare Tab C3 for HTML.
#' @param x Object `materialtabelle`.
#' @param apm apm to determine selected rounding precision
#' @param selected_row should a specific row be selected.
#' @param output Return either a formatted Datatable (DT) or flextable (ft) object.
#' @examples
#' x <- shiny::isolate(eCerto::getValue(eCerto:::test_rv(type = "SR3"), c("General", "materialtabelle")))
#' eCerto:::styleTabC3(x)
#' eCerto:::styleTabC3(x, output="flextable")
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC3 <- function(x, apm = NULL, selected_row = 1, output = c("DT", "ft")) {
  output <- match.arg(output)
  e_msg("Styling Tab.C3")
  precision_U <- 4
  prec_exp <- NULL
  if (!is.null(apm)) {
    # apply analyte specific precision for mean and sd
    prec <- try(sapply(apm, function(x) {
      x[["precision"]]
    }))
    if (!inherits(prec, "try-error") && is.numeric(prec) && all(is.finite(prec)) && length(prec) == nrow(x)) {
      x[, "mean"] <- sapply(1:nrow(x), function(i) {
        round(x[i, "mean"], prec[i])
      })
      x[, "sd"] <- sapply(1:nrow(x), function(i) {
        round(x[i, "sd"], prec[i])
      })
    }
    # apply analyte specific precision for U_abs and cert_val
    prec_exp <- try(sapply(apm, function(x) {
      x[["precision_export"]]
    }))
    if (!inherits(prec_exp, "try-error") && is.numeric(prec_exp) && all(is.finite(prec_exp)) && length(prec_exp) == nrow(x)) {
      # determine number of decimal places required according to DIN1333
      x[, "cert_val"] <- round_DIN1333(x = x[, "cert_val"], n = prec_exp)
      # ***Note!*** U_abs is always rounded up; number of decimal places according to DIN1333
      x[, "U_abs"] <- round_up(x = x[, "U_abs"], n = prec_exp)
    }
  }
  u_cols <- get_UF_cols(x, "U_round")
  cc <- attr(x, "col_code")
  non_edit_cols <- list(columns = which(!(colnames(x) %in% c("k", cc[, "Name"]))) - 1)
  # rename column header for temporary display
  if (nrow(cc) >= 1 && !all(cc[, "Name"] %in% colnames(x))) {
    for (k in 1:nrow(cc)) {
      colnames(x)[colnames(x) == cc[k, "ID"]] <- cc[k, "Name"]
    }
  }

  if (output == "DT") {
    colnames(x) <- gsub("^U_abs$", "U<sub>abs</sub>", colnames(x))
    colnames(x) <- gsub("^u_char$", "u<sub>char</sub>", colnames(x))
    colnames(x) <- gsub("^u_com$", "u<sub>com</sub>", colnames(x))
    colnames(x) <- gsub("^cert_val$", "\u00B5<sub>c</sub>", colnames(x))
    dt <- DT::datatable(
      data = x,
      editable = list(
        target = "cell",
        disable = non_edit_cols
      ),
      options = list(
        dom = "t", paging = FALSE, scrollX = TRUE, ordering = FALSE,
        columnDefs = list(
          list("width" = paste0(max(c(60, nchar(as.character(x[, "analyte"])) * 9)), "px"), "targets" = which(colnames(x) %in% c("analyte")) - 1),
          list("width" = "80px", "targets" = which(!(colnames(x) %in% c("analyte", "n", "k", "unit"))) - 1),
          list("width" = "30px", "targets" = which(colnames(x) %in% c("n", "k")) - 1)
        )
      ),
      rownames = NULL, escape = FALSE, selection = list(mode = "single", target = "row", selected = selected_row)
    )
    dt <- DT::formatCurrency(table = dt, columns = u_cols, currency = "", digits = precision_U)
    # if (!is.null(prec_exp)) dt <- DT::formatCurrency(table = dt, columns = "cert_val", currency = "", digits = prec_exp)
    if (!is.null(prec_exp)) {
      #dt <- DT::formatCurrency(table = dt, columns = "\u00B5<sub>c</sub>", currency = "", digits = prec_exp)
    }
    return(dt)
  } else {
    # return flextable for Word report
    for (idx in u_cols) {
      #x[,idx] <- sprintf("%.04f", round(x, precision_U))
      if (any(is.finite(x[,idx]))) x[is.finite(x[,idx]),idx] <- sprintf("%.04f", round(x[is.finite(x[,idx]),idx], precision_U))
    }
    x[,"cert_val"] <- as.character(x[,"cert_val"])
    x[,"U_abs"] <- as.character(x[,"U_abs"])
    x[,"mean"] <- as.character(x[,"mean"])
    x[,"sd"] <- as.character(x[,"sd"])
    eCerto_flextable_defaults()
    ft <- flextable::flextable(x)
    ft <- flextable::align(ft, j = c("analyte","unit"), align = "left", part = "all")
    ft <- flextable::align(ft, j = !(colnames(x) %in% c("analyte","unit")), align = "right", part = "all")
    ft <- flextable::compose(x = ft, j = which(colnames(x)=="cert_val"), value = flextable::as_paragraph("\u00B5", flextable::as_sub("c")), part = "header")
    ft <- flextable::compose(x = ft, j = which(colnames(x)=="U_abs"), value = flextable::as_paragraph("U", flextable::as_sub("abs")), part = "header")
    ft <- flextable::compose(x = ft, j = which(colnames(x)=="u_char"), value = flextable::as_paragraph("u", flextable::as_sub("char")), part = "header")
    ft <- flextable::compose(x = ft, j = which(colnames(x)=="u_com"), value = flextable::as_paragraph("u", flextable::as_sub("com")), part = "header")
    # exchange further HTML commands against flextable format
    for (j in grep("<.+>.+</.+>", colnames(x))) {
      ft <- flextable::compose(x = ft, j = j, value = HTML2ft(colnames(x)[j]), part = "header")
    }
    fw <- 16 # full page width
    ft <- flextable::width(ft, j = "unit", width = 1, unit = "cm")
    ft <- flextable::width(ft, j = c("n","k"), width = 0.6, unit = "cm")
    ft <- flextable::width(ft, j = !(colnames(x) %in% c("analyte","n","k","unit")), width = 1.2, unit = "cm")
    ft <- flextable::width(ft, j = "analyte", width = fw-2*0.6-0.9-sum(!(colnames(x) %in% c("analyte","n","k","unit")))*1.2, unit = "cm")
    ft <- eCerto_flextable_defaults(ft = ft)
    ft <- flextable::set_caption(ft, caption = flextable::as_paragraph(flextable::as_b("Tab.C3"), " Material properties and accociated uncertainties"))
    return(ft)
  }
}
