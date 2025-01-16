#' @title fnc_styleTabD1.
#' @description \code{styleTabD1} will style Tab.D1 for pretty output.
#' @details tbd.
#' @param df The data.frame of values.
#' @param selected Currently selected row.
#' @param interact_ele Show interactive elements (ordering and buttons), respectively use FALSE to hide them for Word export.
#' @param font.size Specify table font.size explicitly.
#' @examples
#' inp <- "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/DRMD/drmc-007.xml"
#' tab <- eCerto:::read_drmd_xml(inp)
#' tab <- eCerto:::xml2df(tab, type = "admin")
#' out_dt <- eCerto:::style_tabD1(df = tab, selected = NULL, interact_ele = FALSE)
#' out_dt
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabD1 <- function(df, selected = 1, interact_ele = TRUE, font.size = NA) {
  e_msg("Styling Tab.D1 for HTML output")

  # # modify column names, using HTML formatting
  # colnames(df) <- gsub("^value$", "value", colnames(df))

  # if (!is.null(selected) && selected %in% 1:nrow(df)) {
  #   tab_cap <- rev(na.omit(df[selected,]))[-1]
  # } else {
  #   tab_cap <- NULL
  # }

  df <- plyr::ldply(1:nrow(df), function(i) {
    x <- rev(stats::na.omit(unlist(df[i,])))
    data.frame("Last_level" = x[2], "value" = x[1])
  })

  # create DT object
  dt <- DT::datatable(
    data = df, rownames = FALSE, extensions = "Buttons", escape = FALSE,
    options = list(
      dom = ifelse(interact_ele, "Bt", "t"), pageLength = -1, ordering = FALSE,
      buttons = if (interact_ele)  { list(list(extend = "excel", text = "Excel", title = NULL)) },
      initComplete = if (!is.na(font.size)) {DT::JS(
        "function(settings, json) {",
        paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
        "}"
      )}
    ),
    selection = list(mode = "single", selected = selected, target = 'row')#,
    # caption = if (length(tab_cap)>=1) { shiny::tags$caption(
    #   style = 'caption-side: bottom; text-align: left;',
    #   'Tab.D1 bla: ', paste(tab_cap, collapse=" | "), "."
    # )}
  )

  # column formaters
  # round_cols <- c("b<sub>0</sub>", "b<sub>1</sub>", "P<sub>KS,e</sub>", "P<sub>Neu,e</sub>", "P<sub>Mandel</sub>", "LOD", "LOQ", "s<sub>y,x</sub>", "s<sub>x0</sub>", "V<sub>x0</sub>")
  # round_cols <- round_cols[round_cols %in% colnames(df)]
  # dt <- DT::formatCurrency(table = dt, columns = round_cols, currency = "", digits = precision)
  #
  # pval_cols <- c("P<sub>KS,e</sub>", "P<sub>Neu,e</sub>", "P<sub>Mandel</sub>")
  # pval_cols <- pval_cols[pval_cols %in% colnames(df)]
  # dt <- DT::formatStyle(
  #   table = dt,
  #   columns = pval_cols,
  #   target = "cell",
  #   color = DT::styleInterval(cuts = c(0.01, 0.05), values = c("red", "orange", "")),
  #   fontWeight = DT::styleInterval(cuts = c(0.01, 0.05), values = c("bold", "normal", "normal"))
  # )
  # if ("r" %in% colnames(df)) {
  #   dt <- DT::formatCurrency(table = dt, columns = "r", currency = "", digits = 4)
  #   dt <- DT::formatStyle(
  #     table = dt,
  #     columns = "r",
  #     target = "cell",
  #     color = DT::styleInterval(cuts = c(0.995, 0.999), values = c("red", "orange", "")),
  #     fontWeight = DT::styleInterval(cuts = c(0.995, 0.999), values = c("bold", "normal", "normal"))
  #   )
  # }
  return(dt)
}
