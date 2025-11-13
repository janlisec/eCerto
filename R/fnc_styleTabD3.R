#' @title fnc_styleTabD3.
#' @description \code{styleTabD3} will style Tab.D3 for pretty output.
#' @details tbd.
#' @param df The data.frame of values.
#' @examples
#' fl <- system.file("extdata", "drmd", "BAM-M375a.xml", package = "eCerto")
#' lst <- eCerto:::read_drmd_xml(fl)
#' tab <- eCerto:::flatten_list_to_df(lst)
#' out_dt <- eCerto:::styleTabD3(df = tab)
#' out_dt
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabD3 <- function(df, interact_ele = TRUE) {
  e_msg("Styling Tab.D3 for HTML output")

  # hide first column ("path")
  df <- df[,-1, drop=FALSE]

  # convert column 'idx' for better display
  df[,"idx"] <- format_hierarchy(df[,"idx"])

  # create DT object
  dt <- DT::datatable(
    data = df, rownames = FALSE, escape = FALSE, filter = 'top',
    options = list(
      dom = "fti", pageLength = -1, ordering = FALSE, search = list(regex = TRUE)
    ),
    selection = 'none'
  )

  dt <- DT::formatStyle(dt, 'idx', fontFamily = 'monospace')

  return(dt)
}
