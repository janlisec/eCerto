#' @title fnc_styleTabS1.
#' @description \code{styleTabS1} will style Tab.S1 for pretty output.
#' @details tbd.
#' @param x The S data from an session R6 object.
#' @param mt The mt from an session R6 object.
#' @param sr Currently selected row of table.
#' @examples
#' @return A datatable object.
#' @keywords internal

styleTabS1 <- function(x, mt = NULL, sr = 1) {
  message("[styleTabS1] styling Tab.S1")
  for (i in c("slope", "SE_slope", "u_stab", "P")) {
    x[, i] <- pn(x[, i], 4)
  }
  if (!is.null(mt)) {
    x[, "style_analyte"] <- sapply(x[, "analyte"], function(x) {
      ifelse(x %in% mt[, "analyte"], "black", "red")
    })
  } else {
    x[, "style_analyte"] <- "red"
  }
  dt <- DT::datatable(
    data = x,
    options = list(
      dom = "t", pageLength = -1,
      columnDefs = list(
        list(visible = FALSE, targets = 6),
        list(className = "dt-right", targets = "_all")
      )
    ),
    selection = list(mode = "single", target = "row", selected = sr),
    rownames = NULL
  )
  dt <- DT::formatStyle(table = dt, columns = "analyte", valueColumns = "style_analyte", target = "cell", color = DT::styleValue())
  return(dt)
}
