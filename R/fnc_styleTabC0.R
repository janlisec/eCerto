#' @title styleTabC0.
#' @description Prepare Tab C0 for HTML.
#' @param x Object `overview_stats_pre()`.
#' @param ap Analyte specific parameter list.
#' @param type Either 'kompakt' or 'standard'.
#' @examples
# rv <- eCerto:::test_rv(type = "SR3")
# fd <- rv$c_fltData()
# ap <- shiny::isolate(eCerto::getValue(rv, c("General", "apm"))[[rv$cur_an]])
# styleTabC0(x = fd, ap = ap, type=c("kompakt", "standard")[1])
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC0 <- function(x, ap, type=c("kompakt", "standard")) {
  type <- match.arg(type)
  if (type == "kompakt") {
    idx <- attr(x, "id_idx")
    if (!("File" %in% colnames(x))) x <- cbind(x, data.frame(" "=" ", check.names = FALSE))
    dt <- DT::datatable(
      data = x,
      extensions = "Buttons",
      options = list(
        dom = "Bft", paging = FALSE, searching = FALSE, ordering = FALSE,
        buttons = c('copy', 'excel'),
        columnDefs = list(
          list("width"= "80px", "targets" = which(!(colnames(x) %in% c("Lab", " ", "File")))-1),
          list("width"= "30px", "targets" = which(colnames(x) %in% c("Lab"))-1),
          list(className = 'dt-right', targets = "_all")
        )
      ),
      rownames = NULL,
    )
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
          table = dt, target = 'cell', columns = 2:ncol(idx), rows = rown,
          color = DT::styleRow(rows = rown, values = "red"),
          fontWeight = DT::styleRow(rows = rown, values = "bold")
        )
      }
    }
    # round all replicate measurement values with input precision
    dt <- DT::formatCurrency(table = dt, columns = which(!(colnames(x) %in% c("Lab","mean","sd","File"," "))), currency = "", digits = ap[["precision"]])
    # round with output precision (JL: currently the same; adjust and remove comment if requested by users)
    dt <- DT::formatCurrency(table = dt, columns = which(colnames(x) %in% c("mean","sd")), currency = "", digits = ap[["precision"]])
  }
  if (type == "standard") {
    if (!("File" %in% colnames(x))) x <- cbind(x, data.frame(" "=" ", check.names = FALSE))
    dt <- DT::datatable(
      data = x,
      options = list(
        dom = "t", paging = FALSE, searching = FALSE,
        scrollY = "250px", pageLength = -1,
        columnDefs = list(
          #list("width"= "80px", "targets" = which(!(colnames(x) %in% c("Lab", " ", "File")))-1),
          list("width"= "80px", "targets" = which(colnames(x) %in% c("value"))-1),
          list("width"= "30px", "targets" = which(colnames(x) %in% c("ID","Lab","unit","replicate"))-1),
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