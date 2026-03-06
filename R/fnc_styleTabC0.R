#' @title styleTabC0.
#' @description Prepare Tab C0 for HTML.
#' @param x Object `overview_stats_pre()`.
#' @param ap Analyte specific parameter list.
#' @param type Either 'compact' or 'standard'.
#' @param output Return either a formatted Datatable (DT) or flextable (ft) object.
#' @examples
#' rv <- eCerto:::test_rv(type = "SR3")
#' fd <- rv$c_fltData()
#' ap <- shiny::isolate(eCerto::getValue(rv, c("General", "apm"))[[fd[1, "analyte"]]])
#' eCerto:::styleTabC0(x = fd, ap = ap, type = c("compact", "standard")[2])
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC0 <- function(x, ap, type = c("compact", "standard"), output = c("DT", "ft", "ft_HTML")) {
  type <- match.arg(type)
  output <- match.arg(output)
  if (type == "compact") {
    #browser()
    idx <- attr(x, "id_idx")
    if (output %in% c("ft", "ft_HTML")) {
      eCerto_flextable_defaults(output = output)
      ft <- flextable::flextable(x)
      ft <- ft_set_formatter(ft, 5, ft_formatter_fixed_digits, ap[["precision"]])
      if (!is.null(ap[["lab_filter"]])) {
        ap[["sample_filter"]] <- unique(c(ap[["sample_filter"]], unname(unlist(idx[idx[,"Lab"] %in% ap[["lab_filter"]],-1]))))
      }
      if (!is.null(ap[["sample_filter"]])) {
        for (s_idx in ap[["sample_filter"]]) {
          icol <- which(apply(idx, 2, function(cc) {any(cc == s_idx)}))
          irow <- which(apply(idx, 1, function(cc) {any(cc == s_idx)}))
          ft <- flextable::color(ft, i = irow, j = icol, color = "red", part = "body")
        }
      }
      ft <- eCerto_flextable_defaults(ft = ft, output = output)
      ft <- flextable::set_caption(ft, caption = flextable::as_paragraph(flextable::as_b("Tab.C0"), " Imported analyte data from inter laboratory trial"))
      return(ft)
    } else {
      if (!("File" %in% colnames(x))) x <- cbind(x, data.frame(" " = " ", check.names = FALSE))
      dt <- DT::datatable(
        data = x,
        extensions = "Buttons",
        options = list(
          dom = "Bft", paging = FALSE, searching = FALSE, ordering = FALSE,
          buttons = c("copy", "excel"),
          columnDefs = list(
            list("width" = "80px", "targets" = which(!(colnames(x) %in% c("Lab", " ", "File"))) - 1),
            list("width" = "30px", "targets" = which(colnames(x) %in% c("Lab")) - 1),
            list(className = "dt-right", targets = (1:(ncol(x) - 1)) - 1),
            list(className = "dt-left", targets = ncol(x) - 1)
          )
        ),
        rownames = NULL,
      )
      if (!is.null(ap[["sample_filter"]])) {
        for (s_idx in ap[["sample_filter"]]) {
          coln <- colnames(idx)[ceiling(which(idx == s_idx) / nrow(idx))]
          cval <- unlist(x)[which(idx == s_idx)]
          dt <- DT::formatStyle(
            table = dt, columns = coln,
            color = DT::styleEqual(levels = cval, values = "red"),
            fontWeight = DT::styleEqual(levels = cval, values = "bold")
          )
        }
      }
      if (!is.null(ap[["lab_filter"]])) {
        for (l_idx in ap[["lab_filter"]]) {
          rown <- which(idx[, "Lab"] == ap[["lab_filter"]])
          dt <- DT::formatStyle(
            table = dt, target = "cell", columns = 2:ncol(idx), rows = rown,
            color = DT::styleRow(rows = rown, values = "red"),
            fontWeight = DT::styleRow(rows = rown, values = "bold")
          )
        }
      }
      # round all replicate measurement values with input precision
      dt <- DT::formatCurrency(table = dt, columns = which(!(colnames(x) %in% c("Lab", "mean", "sd", "File", " "))), currency = "", digits = ap[["precision"]])
      # round with output precision (JL: currently the same; adjust and remove comment if requested by users)
      dt <- DT::formatCurrency(table = dt, columns = which(colnames(x) %in% c("mean", "sd")), currency = "", digits = ap[["precision"]])
      return(dt)
    }
  }
  if (type == "standard") {
    if (output %in% c("ft", "ft_HTML")) {
      eCerto_flextable_defaults(output = output)
      ft <- flextable::flextable(x)
      ft <- ft_set_formatter(ft, 5, ft_formatter_fixed_digits, ap[["precision"]])
      if (!is.null(ap[["sample_filter"]])) {
        ft <- flextable::color(ft, i = which(x[,"ID"] %in% ap[["sample_filter"]]), j = 1, color = "red", part = "body")
      }
      if (!is.null(ap[["lab_filter"]])) {
        ft <- flextable::color(ft, i = which(x[,"Lab"] %in% ap[["lab_filter"]]), j = 2, color = "red", part = "body")
      }
      ft <- eCerto_flextable_defaults(ft = ft, output = output)
      ft <- flextable::set_caption(ft, caption = flextable::as_paragraph(flextable::as_b("Tab.C0"), " Imported analyte data from inter laboratory trial"))
      return(ft)
    } else {
      if (!("File" %in% colnames(x))) x <- cbind(x, data.frame(" " = " ", check.names = FALSE))
      dt <- DT::datatable(
        data = x,
        options = list(
          dom = "t", paging = FALSE, searching = FALSE,
          scrollY = "250px", pageLength = -1,
          columnDefs = list(
            list("width" = "80px", "targets" = which(colnames(x) %in% c("value")) - 1),
            list("width" = "30px", "targets" = which(colnames(x) %in% c("ID", "Lab", "unit", "replicate")) - 1),
            list(className = "dt-right", targets = 0:4)
          )
        ), rownames = NULL
      )
      dt <- DT::formatCurrency(table = dt, columns = 3, currency = "", digits = ap[["precision"]])
      if (!is.null(ap[["sample_filter"]])) {
        dt <- DT::formatStyle(
          table = dt, columns = "ID",
          color = DT::styleEqual(levels = ap[["sample_filter"]], values = "red"),
          fontWeight = DT::styleEqual(levels = ap[["sample_filter"]], values = "bold")
        )
      }
      if (!is.null(ap[["lab_filter"]])) {
        dt <- DT::formatStyle(
          table = dt, columns = "Lab",
          color = DT::styleEqual(levels = ap[["lab_filter"]], values = "red"),
          fontWeight = DT::styleEqual(levels = ap[["lab_filter"]], values = "bold")
        )
      }
    }
    return(dt)
  }
}
