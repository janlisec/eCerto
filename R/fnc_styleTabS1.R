#' @title fnc_styleTabS1.
#' @description \code{styleTabS1} will style Tab.S1 for pretty output.
#' @details tbd.
#' @param x The S data from a session R6 object.
#' @param mt The mt from a session R6 object.
#' @param sr Currently selected row of table.
#' @param output Return either a formatted Datatable (DT) or flextable (ft) object.
#' @examples
#' x <- eCerto:::prepTabS1(x = eCerto:::test_Stability_Excel())
#' eCerto:::styleTabS1(x = x, mt = NULL, sr = 1)
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabS1 <- function(x, mt = NULL, sr = 1, output = c("DT", "ft")) {
  e_msg("styling Tab.S1")
  output <- match.arg(output)
  P_col <- ifelse("P" %in% colnames(x), "P", "P_adj")
  p_col_idx <- which(colnames(x)==P_col)
  p_cols_sign <- x[,P_col]<0.05
  for (i in c("slope", "SE_slope", "mean", "u_stab", P_col)) {
    if (i %in% colnames(x)) x[, i] <- pn(x[, i], 4)
  }
  if (!is.null(mt)) {
    x[, "style_analyte"] <- sapply(x[, "analyte"], function(x) {
      ifelse(x %in% mt[, "analyte"], "", "red")
    })
  } else {
    x[, "style_analyte"] <- "red"
  }
  # use sub text in header
  colnames(x) <- gsub("_diff", "<sub>diff</sub>", colnames(x))
  colnames(x) <- gsub("^slope$", "b<sub>1</sub>", colnames(x))
  colnames(x) <- gsub("^SE_slope$", "s(b<sub>1</sub>)", colnames(x))
  colnames(x) <- gsub("_stab", "<sub>stab</sub>", colnames(x))
  colnames(x) <- gsub("_cert", "<sub>cert</sub>", colnames(x))
  colnames(x) <- gsub("mean", "\u00B5<sub>s</sub>", colnames(x))
  colnames(x) <- gsub("^P$", "P<sub>b1</sub>", colnames(x))
  colnames(x) <- gsub("^P_adj$", "P<sub>adj,b1</sub>", colnames(x))
  if (output == "ft") {
    #eCerto:::ft_default(x, caption = "Analyte stabilities and accociated uncertainties", id = "Tab.S1")
    eCerto_flextable_defaults()
    ft <- flextable::flextable(x[,colnames(x)!="style_analyte"])
    for (j in grep("<.+>.+</.+>", colnames(x))) {
      ft <- flextable::compose(x = ft, j = j, value = HTML2ft(colnames(x)[j]), part = "header")
    }
    ft <- flextable::align(ft, j = which(!colnames(x) %in% c("analyte","style_analyte")), align = "right", part = "all")
    if (any(x[,"style_analyte"]=="red")) for (i in which(x[,"style_analyte"]=="red")) ft <- flextable::color(ft, i = i, j = "analyte", color = "red", part = "body")
    if (any(p_cols_sign)) for (i in which(p_cols_sign)) ft <- flextable::color(ft, i = i, j = p_col_idx, color = "red", part = "body")
    ft <- eCerto_flextable_defaults(ft = ft)
    ft <- flextable::set_caption(ft, caption = flextable::as_paragraph(flextable::as_b("Tab.S1"), " Analyte stabilities and accociated uncertainties"))
    return(ft)

  } else {
    inv_cols <- grep("style_", colnames(x)) - 1
    # attach a blank column at the end
    x <- cbind(x, data.frame(" " = " ", check.names = FALSE))
    # set up the DT object
    dt <- DT::datatable(
      data = x,
      options = list(
        dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE,
        columnDefs = list(
          list("width" = paste0(max(c(60, nchar(as.character(x[, "analyte"])) * 9)), "px"), "targets" = which(colnames(x) %in% c("analyte")) - 1),
          list("width" = "60px", "targets" = which(!(colnames(x) %in% c("analyte", " "))) - 1),
          # list("width"= "30px", "targets" = which(colnames(x) %in% c("n", "N"))-1),
          list(visible = FALSE, targets = inv_cols),
          list(className = "dt-right", targets = which(!(colnames(x) %in% c("analyte"))) - 1),
          list(className = "dt-left", targets = which(colnames(x) %in% c("analyte")) - 1)
        )
      ),
      selection = list(mode = "single", target = "row", selected = sr),
      rownames = NULL, escape = FALSE
    )
    dt <- DT::formatStyle(table = dt, columns = "analyte", valueColumns = "style_analyte", target = "cell", color = DT::styleValue())
    dt <- DT::formatStyle(
      table = dt,
      columns = which(colnames(x) %in% c("P<sub>b1</sub>", "P<sub>adj,b1</sub>")),
      target = "cell",
      color = DT::styleInterval(cuts = 0.05, values = c("red", "")),
      fontWeight = DT::styleInterval(cuts = 0.05, values = c("bold", "normal"))
    )
    return(dt)
  }
}
