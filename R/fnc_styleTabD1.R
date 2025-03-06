#' @title fnc_styleTabD1.
#' @description \code{styleTabD1} will style Tab.D1 for pretty output.
#' @details tbd.
#' @param df The data.frame of values.
#' @param selected Currently selected row.
#' @examples
#' inp <- "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/DRMD/drmc-007.xml"
#' tab <- eCerto:::read_drmd_xml(inp)
#' tab <- eCerto:::xml2df(tab, type = "admin")
#' out_dt <- eCerto:::style_tabD1(df = tab, selected = NULL)
#' out_dt
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabD1 <- function(df, selected = 1) {
  e_msg("Styling Tab.D1 for HTML output")
  # ====
  # old Version
  # df <- plyr::ldply(1:nrow(df), function(i) {
  #   x <- rev(stats::na.omit(unlist(df[i,])))
  #   data.frame("Last_level" = x[2], "value" = x[1])
  # })
  # new version
  df <- df[,-1, drop=FALSE]
  # ====

  # create DT object
  dt <- DT::datatable(
    data = df, rownames = FALSE, escape = FALSE,
    options = list(
      dom = "t", pageLength = -1, ordering = FALSE
    ),
    selection = list(mode = "single", selected = selected, target = 'row')
  )

  return(dt)
}
