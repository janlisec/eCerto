#' @title styleTabC3.
#' @description Prepare Tab C3 for HTML.
#' @param x Object `overview_stats_pre()`.
#' @param n Rounding precision for specific columns..
#' @examples
#' x <- eCerto:::test_Certification_Excel()
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC3 <- function(x, precision_U = 4, selected_row = 1, currency_cols = 1) {
  dt <- DT::datatable(
    data = x,
    editable = list(
      target = "cell",
      disable = list(columns = which(!(colnames(x) %in% c("k", attr(x, "col_code")[,"Name"])))-1)
    ),
    options = list(
      dom = "t", paging = FALSE, scrollX = TRUE, ordering = FALSE,
      columnDefs = list(
        list("width"= "80px", "targets" = which(!(colnames(x) %in% c("analyte","n","k","unit")))-1),
        list("width"= "30px", "targets" = which(colnames(x) %in% c("n","k"))-1)
      )
    ),
    rownames = NULL, selection = list(mode="single", target="row", selected=selected_row)
  )
  dt <- DT::formatCurrency(table = dt, columns = get_UF_cols(x, "U_round"), currency = "", digits = precision_U)
  return(dt)

}