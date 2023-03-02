#' @title styleTabC1.
#' @description Prepare Tab C1 for HTML.
#' @param x Object `overview_stats_pre()`.
#' @param n Rounding precision for specific columns..
#' @examples
#' rv <- eCerto:::test_rv(type = "SR3")
#' x <- eCerto:::prepTabC1(dat = shiny::isolate(dat <- rv$c_fltData()))
#' eCerto:::styleTabC1(x = x)
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC1 <- function(x, n=4) {
  nc <- ncol(x)
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
    selection='none',
    rownames = NULL
  )
  dt <- DT::formatCurrency(
    table = dt, columns = c(2,3), currency = "", digits = n
  )
  return(dt)
}