#' @title fnc_styleTabD1.
#' @description \code{styleTabD1} will style Tab.D1 for pretty output.
#' @details tbd.
#' @param df The data.frame of values.
#' @param selected Currently selected row.
#' @examples
#' df <- data.frame(
#'   path = paste0("Path", 1:5),
#'   idx = c("1_1_1_1","1_1_2_1","1_2_1_1","1_2_2_1","1_3_1_1"),
#'   value = paste0("Value", 1:5)
#' )
#' eCerto:::styleTabD1(df = tab, selected = NULL)
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabD1 <- function(df, selected = 1) {
  e_msg("Styling Tab.D1 for HTML output")

  # hide first column ("path")
  df <- df[,-1, drop=FALSE]

  # convert column 'idx' for better display
  df[,"idx"] <- format_hierarchy(df[,"idx"])

  # create DT object
  dt <- DT::datatable(
    data = df, rownames = FALSE, escape = FALSE,
    options = list(
      dom = "t", pageLength = -1, ordering = FALSE
    ),
    selection = list(mode = "single", selected = selected, target = 'row')
  )

  dt <- DT::formatStyle(dt, 'idx', fontFamily = 'monospace')

  return(dt)
}
