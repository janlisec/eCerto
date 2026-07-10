#' @title fnc_styleTabS1.
#' @description \code{styleTabS1} will style Tab.S1 for pretty output.
#' @details tbd.
#' @param x The S data from a session R6 object.
#' @param mt The mt from a session R6 object.
#' @param optimize_u_stab Subtract the uncertainty defined in `mt` from u_stab calculated for the desired t_cert. Modify t_cert until non-negative values for u_stab appear in TabS1.
#' @param sr Currently selected row of table.
#' @param output Return either a formatted Datatable (DT) or flextable (ft) object.
#' @examples
#' x <- eCerto:::prepTabS1(x = eCerto:::test_Stability_Excel())
#' eCerto:::styleTabS1(x = x)
#' mt <- data.frame("analyte"="Mn", "cert_val"=100, "k"=2, "U_abs"=5)
#' eCerto:::styleTabS1(x = x, mt = mt)
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabS1 <- function(x, mt = NULL, optimize_u_stab = FALSE, sr = 1, output = c("DT", "ft", "ft_HTML")) {
  e_msg("styling Tab.S1")
  output <- match.arg(output)
  P_col <- ifelse("P" %in% colnames(x), "P", "P_adj")
  p_col_idx <- which(colnames(x)==P_col)
  p_cols_sign <- x[,P_col]<0.05
  if (TRUE) {
    # add a column for U_stab (the absolute uncertainty values for stability), just upon output generation
    x[,"U<sub>stab</sub>"] <- pn(x[,"u_stab"]*x[,"mean"], 4)
  }
  if (optimize_u_stab) {
    # add a column for t_max (the maximum t_cert before u_stab exceeds U_abs_new/3), just upon output generation
    # browser()
    # u_rest <- sapply(x[,"analyte"], function(a) {
    #   i <- which(mt[,"analyte"]==a)
    #   if (length(i)==1) { mt[i,"u_com"] } else { NA }
    # })
    # x[,"t<sub>max</sub>"] <- round((x[,"mean"]*u_rest)/(sqrt(8)*x[,"SE_slope"]))
    tmp <- sapply(x[,"analyte"], function(a) {
      i <- which(mt[,"analyte"]==a)
      if (length(i)==1) {
        t_max <- 0:60
        u_stab <- x[x[,"analyte"]==a,"SE_slope"]*t_max/x[x[,"analyte"]==a,"mean"]
        U_abs_3 <- mt[i,"k"]*mt[i,"cert_val"]*sqrt(mt[i,"u_com"]^2+u_stab^2)/3
        max(which((u_stab*x[x[,"analyte"]==a,"mean"])<U_abs_3))
      } else {
        NA
      }
    })
    x[,"t<sub>max</sub>"] <- tmp
  }
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
  if (output %in% c("ft", "ft_HTML")) {
    #eCerto:::ft_default(x, caption = "Analyte stabilities and accociated uncertainties", id = "Tab.S1")
    eCerto_flextable_defaults(output = output)
    ft <- flextable::flextable(x[,colnames(x)!="style_analyte"])
    for (j in grep("<.+>.+</.+>", colnames(x))) {
      ft <- flextable::compose(x = ft, j = j, value = HTML2ft(colnames(x)[j]), part = "header")
    }
    ft <- flextable::align(ft, j = which(!colnames(x) %in% c("analyte","style_analyte")), align = "right", part = "all")
    if (any(x[,"style_analyte"]=="red")) for (i in which(x[,"style_analyte"]=="red")) ft <- flextable::color(ft, i = i, j = "analyte", color = "red", part = "body")
    if (any(p_cols_sign)) for (i in which(p_cols_sign)) ft <- flextable::color(ft, i = i, j = p_col_idx, color = "red", part = "body")
    ft <- eCerto_flextable_defaults(ft = ft, output = output)
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
