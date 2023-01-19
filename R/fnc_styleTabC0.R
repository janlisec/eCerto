#' @title styleTabC0.
#' @description Prepare Tab C0 for HTML.
#' @param x Object `overview_stats_pre()`.
#' @param ap Analyte specific parameter list.
#' @type Either kompakt or standard.
#' @examples
#' x <- eCerto:::test_Certification_Excel()
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC0 <- function(x, ap, type=c("kompakt", "standard")) {

  if (type == "kompakt") {
    dt <- DT::datatable(
      data = x,
      options = list(
        dom = "t", paging = FALSE, searching = FALSE
      ),
      rownames = NULL,
    )
    idx <- attr(x, "id_idx")
    if (!is.null(ap[["sample_filter"]])) {
      for (s_idx in ap[["sample_filter"]]) {
        coln <- colnames(idx)[ceiling(which(idx==s_idx)/nrow(idx))]
        cval <- unlist(x)[which(idx==s_idx)]
        dt <- DT::formatStyle(
          table = dt, columns = coln,
          color = DT::styleEqual(levels = cval, values = "red"),
          fontWeight = DT::styleEqual(levels = cval, values = "bold")
        )
      }
    }
    if (!is.null(ap[["lab_filter"]])) {
      for (l_idx in ap[["lab_filter"]]) {
        rown <- which(idx[,"Lab"]==ap[["lab_filter"]])
        dt <- DT::formatStyle(
          table = dt, target = 'row', columns = 2:ncol(idx), rows = rown,
          color = DT::styleRow(rows = rown, values = "red"),
          fontWeight = DT::styleRow(rows = rown, values = "bold")
        )
      }
    }
    # round with input precision
    dt <- DT::formatCurrency(table = dt, columns = 2:(ncol(x)-2), currency = "", digits = ap[["precision"]])
    # round with output precision (JL: currently the same; adjust and remove comment if requested by users)
    dt <- DT::formatCurrency(table = dt, columns = (ncol(x)-1):ncol(x), currency = "", digits = ap[["precision"]])
  }

  if (type == "standard") {
    dt <- DT::datatable(
      data = x,
      options = list(
        dom = "t", paging = FALSE, searching = FALSE,
        scrollY = "250px", pageLength = -1,
        columnDefs = list(
          list("width"= "80px", targets=2),
          list("width"= "40px", targets=c(0,1,3,4)),
          list(className = 'dt-right', targets = 0:4)
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