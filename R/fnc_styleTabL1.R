#' @title fnc_styleTabL1.
#' @description \code{styleTabL1} will style Tab.L1 for pretty output.
#' @details tbd.
#' @param x The LTS data for a material property.
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabL1 <- function(x) {
  message("[styleTabL1] styling Tab.L1")
  # use sub text in header
  # colnames(x) <- gsub("_diff", "<sub>diff</sub>", colnames(x))
  colnames(x) <- gsub("_slope", "<sub>slope</sub>", colnames(x))
  colnames(x) <- gsub("_stab", "<sub>stab</sub>", colnames(x))
  colnames(x) <- gsub("_cert", "<sub>cert</sub>", colnames(x))
  # setting cols invisible
  # inv_cols <- grep("style_", colnames(x))-1
  # attach a blank column at the end
  # x <- cbind(x, data.frame(" "=" ", check.names = FALSE))
  # set up the DT object
  dt <- DT::datatable(
    data = x,
    options = list(
      paging = TRUE, pageLength = 25, searching = FALSE, stateSave = TRUE,
      columnDefs = list(
        list("width" = "60px", "targets" = colnames(x) %in% c("Value", "Date"))
        # list(visible = FALSE, targets = inv_cols),
        # list(className = 'dt-right', targets = which(!(colnames(x) %in% c("analyte")))-1),
        # list(className = 'dt-left', targets = which(colnames(x) %in% c("analyte"))-1)
      )
    ),
    selection = list(mode = "single", target = "row"),
    rownames = NULL, escape = FALSE
  )
  # dt <- DT::formatStyle(table = dt, columns = "analyte", valueColumns = "style_analyte", target = "cell", color = DT::styleValue())
  return(dt)
}
