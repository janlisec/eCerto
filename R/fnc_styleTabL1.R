#' @title fnc_styleTabL1.
#' @description \code{styleTabL1} will style Tab.L1 for pretty output.
#' @details tbd.
#' @param x The LTS data for a material property.
#' @examples
#' x <- data.frame("Value"=1, "Date"=Sys.Date(), "File"="XX.smp")
#' eCerto:::styleTabL1(x=x)
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabL1 <- function(x) {
  e_msg("styling Tab.L1")
  # set up the DT object
  dt <- DT::datatable(
    data = x,
    options = list(
      paging = TRUE, pageLength = 10, searching = FALSE, stateSave = TRUE,
      columnDefs = list(
        list("width" = "60px", "targets" = colnames(x) %in% c("Value", "Date"))
      )
    ),
    selection = list(mode = "single", target = "row"),
    rownames = NULL, escape = FALSE
  )
  return(dt)
}
