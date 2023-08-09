#' @title styleTabC1.
#' @description Prepare Tab C1 for HTML.
#' @param x Object `overview_stats_pre()`.
#' @param n Rounding precision for mean/sd columns.
#' @param fmt Output value depicted in the table.
#' @examples
#' rv <- eCerto:::test_rv(type = "SR3")
#' shiny::isolate(dat <- rv$c_fltData())
#' shiny::isolate(lab_means <- rv$c_lab_means(data = dat))
#' x <- eCerto:::prepTabC1(dat = dat, lab_means = lab_means)
#' eCerto:::styleTabC1(x = x)
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC1 <- function(x, n = 4, fmt = c("alpha", "pval", "cval", "cval05", "cval01")) {
  fmt <- match.arg(fmt)
  if (fmt %in% c("pval", "cval", "cval05", "cval01")) {
    cns <- c("Dixon", "Grubbs1", "Grubbs2", "Cochran")
    cns <- cns[cns %in% colnames(x)]
    if (length(cns)>=1) x[,cns] <- round(x[,cns], 6)
  }
  nc <- ncol(x)
  colnames(x) <- gsub("_01", "<sub>.01</sub>", colnames(x))
  colnames(x) <- gsub("_05", "<sub>.05</sub>", colnames(x))
  colnames(x) <- gsub("1$", "<sub>1</sub>", colnames(x))
  colnames(x) <- gsub("2$", "<sub>2</sub>", colnames(x))
  dt <- DT::datatable(
    # add a fake column to the table to allow horizontal fill
    data = cbind(x, data.frame(" "=" ", check.names = FALSE)),
    options = list(
      dom = "t", pageLength=-1, scrollX = TRUE, ordering = FALSE,
      columnDefs = list(
        list("width"= "80px", targets=c(1:2,4:(nc-1))),
        list("width"= "30px", targets=c(0,3)),
        list("orderable"= "true", targets=0:2),
        list(className = 'dt-right', targets = "_all")
      )
    ),
    selection='none', rownames = NULL, escape = FALSE
  )
  dt <- DT::formatCurrency(
    table = dt, columns = c(2,3), currency = "", digits = n
  )
  return(dt)
}