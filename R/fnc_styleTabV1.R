#' @title fnc_styleTabV1.
#' @description \code{styleTabV1} will style Tab.V1 for pretty output.
#' @details tbd.
#' @param df The data.frame of values.
#' @param precision Number of digits to display.
#' @param selected Currently selected row.
#' @param show_colgroups Character.
#' @examples
#' inp <- system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx")
#' tab <- eCerto:::read_Vdata(file = inp)
#' out <- plyr::ldply(levels(tab[,"Analyte"]), function(a) {
#'     eCerto:::prepTabV1(tab = tab, a = a)
#' })
#' eCerto:::style_tabV1(df = out, selected = NULL, ordering = FALSE)
#' @return A datatable object.
#' @keywords internal
#' @noRd
style_tabV1 <- function(df, precision = 3, selected = 1, ordering = TRUE, font.size = "100%", show_colgroups = c("lm", "wr", "lo")) {
  e_msg("Styling Tab.V1 for HTML output")

  # check for columns with consistent values, which can be better stored in a table caption
  tab_cap <- NULL
  if ("lm" %in% show_colgroups) {
    for (unique_val_col in c("alpha", "k", "N", "n")) {
      if (length(unique(df[,unique_val_col]))==1) {
        tab_cap <- c(tab_cap, paste(unique_val_col, "=", unique(df[,unique_val_col])))
        df <- df[,!(colnames(df) %in% unique_val_col)]
      }
    }
  } else {
    df <- df[,!colnames(df) %in% c("alpha", "k", "N", "n")]
    tab_cap <- rep("", 4)
  }

  # filter columns from display
  if (!("lm" %in% show_colgroups)) df <- df[,!colnames(df) %in% c("b0", "b1", "r", "s_yx", "s_x0", "V_x0", "P_KS_Res", "P_Neu_Res", "P_Mandel", "F_Test")]
  if (!("wr" %in% show_colgroups)) df <- df[,!colnames(df) %in% c("c_WR_min", "c_WR_max", "c_WR_min2", "c_WR_max2", "unit_cali", "unit_smpl", "conv_fac")]
  if (!("lo" %in% show_colgroups)) df <- df[,!colnames(df) %in% c("LOD", "LOQ")]

  # modify column names, using HTML formatting
  colnames(df) <- gsub("^P_KS_Res$", "P<sub>KS,e</sub>", colnames(df))
  colnames(df) <- gsub("^P_Neu_Res$", "P<sub>Neu,e</sub>", colnames(df))
  colnames(df) <- gsub("^P_Mandel$", "P<sub>Mandel</sub>", colnames(df))
  colnames(df) <- gsub("^F_Test$", "Out<sub>F</sub>", colnames(df))
  colnames(df) <- gsub("^s_yx$", "s<sub>y,x</sub>", colnames(df))
  colnames(df) <- gsub("^s_x0$", "s<sub>x0</sub>", colnames(df))
  colnames(df) <- gsub("^V_x0$", "V<sub>x0</sub>", colnames(df))
  colnames(df) <- gsub("^b0$", "b<sub>0</sub>", colnames(df))
  colnames(df) <- gsub("^b1$", "b<sub>1</sub>", colnames(df))
  colnames(df) <- gsub("^c_WR_min$", "c<sub><i>min</i></sub>", colnames(df))
  colnames(df) <- gsub("^c_WR_max$", "c<sub><i>max</i></sub>", colnames(df))
  colnames(df) <- gsub("^c_WR_min2$", "c<sub><i>min,s</i></sub>", colnames(df))
  colnames(df) <- gsub("^c_WR_max2$", "c<sub><i>max,s</i></sub>", colnames(df))
  colnames(df) <- gsub("^conv_fac$", "conv<sub>fac</sub>", colnames(df))
  colnames(df) <- gsub("^unit_cali$", "unit<sub>cali</sub>", colnames(df))
  colnames(df) <- gsub("^unit_smpl$", "unit<sub>smpl</sub>", colnames(df))

  # modify table head
  second_header_row <- tags$table(
    class = 'display',
    tags$thead(
      tags$tr(
        th(colspan = 6-length(tab_cap), ''),
        if ("lm" %in% show_colgroups) tags$th(style="background-color:#D8D8D8; text-align:center; font-style:italic", colspan = 10, 'Linear model parameters and residuals evaluation'),
        if ("lo" %in% show_colgroups) tags$th(colspan = 2, ''),
        if ("wr" %in% show_colgroups) tags$th(style="background-color:#D8D8D8; text-align:center; font-style:italic", colspan = 7, 'Working range')
      ),
      tags$tr(
        lapply(colnames(df), function(x) { tags$th(shiny::HTML(x)) })
      )
    )
  )

  # create DT object
  dt <- DT::datatable(
    data = df, rownames = FALSE, extensions = "Buttons", escape = FALSE,
    options = list(
      dom="Bt", pageLength = -1, ordering = ordering,
      buttons = list(list(extend = "excel", text = "Excel", title = NULL)),
      initComplete = htmlwidgets::JS(
        "function(settings, json) {",
        paste0("$(this.api().table().container()).css({'font-size': '", font.size, "'});"),
        "}"
      )
    ),
    selection = list(mode = "single", selected = selected, target = 'row'),
    caption = if ("lm" %in% show_colgroups && length(tab_cap)>=1) { htmltools::tags$caption(
      style = 'caption-side: bottom; text-align: left;',
      'Tab.V1 These values are consistent for all rows of the table: ', paste(tab_cap, collapse=", "), "."
    )},
    container = second_header_row
  )

  # column formaters
  round_cols <- c("b<sub>0</sub>", "b<sub>1</sub>", "P<sub>KS,e</sub>", "P<sub>Neu,e</sub>", "P<sub>Mandel</sub>", "LOD", "LOQ", "s<sub>y,x</sub>", "s<sub>x0</sub>", "V<sub>x0</sub>")
  round_cols <- round_cols[round_cols %in% colnames(df)]
  dt <- DT::formatCurrency(table = dt, columns = round_cols, currency = "", digits = precision)

  pval_cols <- c("P<sub>KS,e</sub>", "P<sub>Neu,e</sub>", "P<sub>Mandel</sub>")
  pval_cols <- pval_cols[pval_cols %in% colnames(df)]
  dt <- DT::formatStyle(
    table = dt,
    columns = pval_cols,
    target = "cell",
    color = DT::styleInterval(cuts = c(0.01, 0.05), values = c("red", "orange", "")),
    fontWeight = DT::styleInterval(cuts = c(0.01, 0.05), values = c("bold", "normal", "normal"))
  )
  if ("r" %in% colnames(df)) {
    dt <- DT::formatCurrency(table = dt, columns = "r", currency = "", digits = 4)
    dt <- DT::formatStyle(
      table = dt,
      columns = "r",
      target = "cell",
      color = DT::styleInterval(cuts = c(0.995, 0.999), values = c("red", "orange", "")),
      fontWeight = DT::styleInterval(cuts = c(0.995, 0.999), values = c("bold", "normal", "normal"))
    )
  }
  return(dt)
}