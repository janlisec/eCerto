#' @title fnc_styleTabS1.
#' @description \code{styleTabS1} will style Tab.S1 for pretty output.
#' @details tbd.
#' @param x The S data from an session R6 object.
#' @param mt The mt from an session R6 object.
#' @param sr Currently selected row of table.
#' @examples
#' x <- eCerto:::prepTabS1(x = eCerto:::test_Stability_Excel())
#' eCerto:::styleTabS1(x = x, mt = NULL, sr = 1)
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabS1 <- function(x, mt = NULL, sr = 1) {
  message("[styleTabS1] styling Tab.S1")
  for (i in c("slope", "SE_slope", "u_stab", "P")) {
    x[, i] <- pn(x[, i], 4)
  }
  if (!is.null(mt)) {
    x[, "style_analyte"] <- sapply(x[, "analyte"], function(x) {
      ifelse(x %in% mt[, "analyte"], "", "red")
    })
  } else {
    x[, "style_analyte"] <- "red"
  }
  # use sub text in header
  colnames(x) <- gsub("_diff", "<sub>diff</sub>", colnames(x))
  colnames(x) <- gsub("_slope", "<sub>slope</sub>", colnames(x))
  colnames(x) <- gsub("_stab", "<sub>stab</sub>", colnames(x))
  colnames(x) <- gsub("_cert", "<sub>cert</sub>", colnames(x))
  inv_cols <- grep("style_", colnames(x)) - 1
  # attach a blank column at the end
  x <- cbind(x, data.frame(" " = " ", check.names = FALSE))
  # set up the DT object
  dt <- DT::datatable(
    data = x,
    options = list(
      dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE,
      columnDefs = list(
        list("width" = paste0(max(c(60, nchar(as.character(x[, "analyte"])) * 9)), "px"), "targets" = which(colnames(x) %in% c("analyte")) - 1),
        list("width" = "60px", "targets" = which(!(colnames(x) %in% c("analyte", " "))) - 1),
        # list("width"= "30px", "targets" = which(colnames(x) %in% c("n", "N"))-1),
        list(visible = FALSE, targets = inv_cols),
        list(className = "dt-right", targets = which(!(colnames(x) %in% c("analyte"))) - 1),
        list(className = "dt-left", targets = which(colnames(x) %in% c("analyte")) - 1)
      )
    ),
    selection = list(mode = "single", target = "row", selected = sr),
    rownames = NULL, escape = FALSE
  )
  dt <- DT::formatStyle(table = dt, columns = "analyte", valueColumns = "style_analyte", target = "cell", color = DT::styleValue())
  return(dt)
}
