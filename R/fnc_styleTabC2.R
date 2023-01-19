#' @title styleTabC2.
#' @description Prepare Tab.C2 as data table object.
#' @param x Output of function `prepTabC2`.
#' @param n Rounding precision for specific columns..
#' @examples
#' x <- eCerto:::test_Certification_Excel()
#' x <- eCerto:::prepTabC2(data = x)
#' eCerto:::styleTabC2(x = x)
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC2 <- function(x, n=4) {
  dt <- DT::datatable(
    # add a fake column to the table to allow horizontal fill
    data = cbind(x, data.frame(" "=" ", check.names = FALSE)),
    options = list(
      dom = "t", pageLength=1, ordering=FALSE, scrollX = TRUE,
      columnDefs = list(
        list("width"= "80px", targets=0:10),
        list(className = 'dt-right', targets = "_all")
      )
    ),
    selection = 'none',
    rownames = NULL
  )
  dt <- DT::formatCurrency(
    table = dt, columns = c(1,2,3), currency = "", digits = n
  )
  return(dt)
}