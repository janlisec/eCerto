#' @title fnc_styleTabD3.
#' @description \code{styleTabD3} will style Tab.D3 for pretty output.
#' @details tbd.
#' @param df The data.frame of values.
#' @examples
#' inp <- "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/DRMD/drmc-007.xml"
#' tab <- eCerto:::read_drmd_xml(inp)
#' tab <- eCerto:::xml2df(tab, type = "full")
#' out_dt <- eCerto:::styleTabD3(df = tab)
#' out_dt
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabD3 <- function(df, interact_ele = TRUE) {
  e_msg("Styling Tab.D3 for HTML output")

  # create DT object
  dt <- DT::datatable(
    data = df[,-1], rownames = FALSE, escape = FALSE, filter = 'top',
    options = list(
      dom = "fti", pageLength = -1, ordering = FALSE, search = list(regex = TRUE)
    ),
    selection = 'none'
  )
  return(dt)
}
