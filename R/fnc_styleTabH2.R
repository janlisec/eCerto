#' @title fnc_styleTabH2.
#' @description \code{styleTabH2} will style Tab.H2 for pretty output.
#' @details tbd.
#' @param x data.frame.
#' @param precision Rounding precision.
#' @examples
#' \donttest{
#' x <- data.frame("Item" = LETTERS[1:3], "mean" = 1:3, "sd" = rnorm(3), "n" = 4:6)
#' eCerto:::styleTabH2(x = x)
#' eCerto:::styleTabH2(x = x, precision = 2)
#' }
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabH2 <- function(x, precision = 4) {
  message("[styleTabH2] styling Tab.H2")
  cols <- which(colnames(x) %in% c("mean", "sd"))
  for (i in cols) x[, i] <- pn(x[, i], precision)
  # attach a blank column at the end
  x <- cbind(x, data.frame(" " = " ", check.names = FALSE))
  dt <- DT::datatable(
    data = x,
    options = list(
      dom = "t",
      pageLength = -1,
      order = list(list(0, "asc")),
      columnDefs = list(
        list("orderable" = "false", "targets" = 4),
        list("width" = "60px", "targets" = which(!(colnames(x) %in% c(" ", "n"))) - 1),
        list("width" = "30px", "targets" = which(colnames(x) %in% c("n")) - 1),
        list(className = "dt-right", targets = "_all")
      )
    ),
    rownames = NULL, selection = "none"
  )
  return(dt)
}
