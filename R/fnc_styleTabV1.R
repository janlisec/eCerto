#' @title fnc_styleTabV1.
#' @description \code{styleTabV1} will style Tab.V1 for pretty output.
#' @details tbd.
#' @param df The data.frame of values.
#' @param precision Number of digits to display.
#' @param selected Currently selected row.
#' @return A datatable object.
#' @keywords internal
#' @noRd
style_tabV1 <- function(df, precision = 3, selected = 1) {
  colnames(df) <- gsub("^P_KS_Res$", "P<sub>KS,Res</sub>", colnames(df))
  colnames(df) <- gsub("^P_Neu_Res$", "P<sub>Neu,Res</sub>", colnames(df))
  colnames(df) <- gsub("^P_Mandel$", "P<sub>Mandel</sub>", colnames(df))
  colnames(df) <- gsub("^F_Test$", "Out<sub>F</sub>", colnames(df))
  colnames(df) <- gsub("^s_yx$", "s<sub>y,x</sub>", colnames(df))
  colnames(df) <- gsub("^s_x0$", "s<sub>x0</sub>", colnames(df))
  colnames(df) <- gsub("^V_x0$", "V<sub>x0</sub>", colnames(df))
  colnames(df) <- gsub("^b0$", "b<sub>0</sub>", colnames(df))
  colnames(df) <- gsub("^b1$", "b<sub>1</sub>", colnames(df))
  dt <- DT::datatable(data = df, options = list(dom="t", pageLength = -1), rownames = FALSE, escape = FALSE, selection = list(mode = "single", selected = selected, target = 'row'))
  round_cols <- c("b<sub>0</sub>", "b<sub>1</sub>", "P<sub>KS,Res</sub>", "P<sub>Neu,Res</sub>", "P<sub>Mandel</sub>", "LOD", "LOQ", "s<sub>y,x</sub>", "s<sub>x0</sub>", "V<sub>x0</sub>")
  dt <- DT::formatCurrency(table = dt, columns = round_cols, currency = "", digits = precision)
  pval_cols <- c("P<sub>KS,Res</sub>", "P<sub>Neu,Res</sub>", "P<sub>Mandel</sub>")
  dt <- DT::formatStyle(
    table = dt,
    columns = pval_cols,
    target = "cell",
    color = DT::styleInterval(cuts = c(0.01, 0.05), values = c("red", "orange", "")),
    fontWeight = DT::styleInterval(cuts = c(0.01, 0.05), values = c("bold", "normal", "normal"))
  )
  return(dt)
}