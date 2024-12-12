#' @title styleTabC3.
#' @description Prepare Tab C3 for HTML.
#' @param x Object `materialtabelle`.
#' @param apm apm to determine selected rounding precision
#' @param selected_row should a specific row be selected.
#' @examples
#' x <- shiny::isolate(eCerto::getValue(eCerto:::test_rv(type = "SR3"), c("General", "materialtabelle")))
#' eCerto:::styleTabC3(x)
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC3 <- function(x, apm = NULL, selected_row = 1) {
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
}
