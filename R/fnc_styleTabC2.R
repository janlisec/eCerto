#' @title styleTabC2.
#' @description Prepare Tab.C2 as data table object.
#' @param x Output of function `prepTabC2`.
#' @param n Rounding precision for analyte specific columns.
#' @param precision Rounding precision for P-value columns.
#' @examples
#' x <- eCerto:::test_Certification_Excel()
#' x <- eCerto:::prepTabC2(dat = x)
#' eCerto:::styleTabC2(x = x)
#' x[,"KS_p"] <- 10^-6
#' eCerto:::styleTabC2(x = x)
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC2 <- function(x, n = 3, precision = 4) {
  p_cols <- grep("_p", colnames(x))
  colnames(x)[p_cols] <- gsub("_p", "<sub>p</sub>", colnames(x)[p_cols])
  fmt_p_cols <- p_cols[which(x[,p_cols]<10^-precision)]
  if (length(fmt_p_cols)>=1) {
    p_cols <- p_cols[!(p_cols %in% fmt_p_cols)]
    for (i in fmt_p_cols) {
      x[,i] <- formatC(x[,i], format="E", digits=2)
    }
  }
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
    selection = 'none', rownames = NULL, escape = FALSE
  )
  dt <- DT::formatCurrency(
    table = dt, columns = c(1,2,3,4), currency = "", digits = n
  )
  dt <- DT::formatCurrency(
    table = dt, columns = p_cols, currency = "", digits = precision
  )
  dt <- DT::formatCurrency(
    table = dt, columns = c(8, 10), currency = "", digits = 2
  )
  dt <- DT::formatStyle(
    table = dt,
    columns = c(fmt_p_cols, p_cols),
    target = "cell",
    color = DT::styleInterval(cuts = 0.05, values = c("red","")),
    fontWeight = DT::styleInterval(cuts = 0.05, values = c("bold","normal"))
  )
  return(dt)
}