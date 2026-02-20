#' @title styleTabC2.
#' @description Prepare Tab.C2 as data table object.
#' @param x Output of function `prepTabC2`.
#' @param n Rounding precision for analyte specific columns.
#' @param precision Rounding precision for P-value columns.
#' @param output Return either a formatted Datatable (DT) or flextable (ft) object.
#' @examples
#' x <- eCerto:::test_Certification_Excel()
#' x <- eCerto:::prepTabC2(dat = x)
#' eCerto:::styleTabC2(x = x)
#' x[, "KS_p"] <- 10^-6
#' eCerto:::styleTabC2(x = x)
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC2 <- function(x, n = 3, precision = 4, output = c("DT", "ft")) {
  output <- match.arg(output)
  p_cols <- grep("_p", colnames(x))
  p_cols_sign <- p_cols[which(x[,p_cols]<0.05)]
  colnames(x)[p_cols] <- gsub("_p", "<sub>p</sub>", colnames(x)[p_cols])
  fmt_p_cols <- p_cols[which(x[, p_cols] < 10^-precision)]
  if (length(fmt_p_cols) >= 1) {
    p_cols <- p_cols[!(p_cols %in% fmt_p_cols)]
    for (i in fmt_p_cols) {
      x[, i] <- formatC(x[, i], format = "E", digits = 2)
    }
  }
  if (output == "ft") {
    eCerto_flextable_defaults()
    ft <- flextable::flextable(x)
    for (j in grep("<.+>.+</.+>", colnames(x))) {
      ft <- flextable::compose(x = ft, j = j, value = HTML2ft(colnames(x)[j]), part = "header")
    }
    ft <- ft_set_formatter(ft, 1:4, ft_formatter_fixed_digits, n)
    ft <- ft_set_formatter(ft, c(8,10), ft_formatter_fixed_digits, 2)
    ft <- ft_set_formatter(ft, p_cols, ft_formatter_fixed_digits, precision)
    if (length(p_cols_sign)>=1) for (j in p_cols_sign) ft <- flextable::color(ft, i = 1, j = j, color = "red", part = "body")
    ft <- eCerto_flextable_defaults(ft = ft)
    ft <- flextable::set_caption(ft, caption = flextable::as_paragraph(flextable::as_b("Tab.C2"), " Statistics regarding overall mean distribution and variance testing"))
    return(ft)
  } else {
    dt <- DT::datatable(
      # add a fake column to the table to allow horizontal fill
      data = cbind(x, data.frame(" " = " ", check.names = FALSE)),
      options = list(
        dom = "t", pageLength = 1, ordering = FALSE, scrollX = TRUE,
        columnDefs = list(
          list("width" = "80px", targets = 0:10),
          list(className = "dt-right", targets = "_all")
        )
      ),
      selection = "none", rownames = NULL, escape = FALSE
    )
    dt <- DT::formatCurrency(
      table = dt, columns = c(1, 2, 3, 4), currency = "", digits = n
    )
    dt <- DT::formatCurrency(
      table = dt, columns = p_cols, currency = "", digits = precision
    )
    dt <- DT::formatCurrency(
      table = dt, columns = c(8, 10), currency = "", digits = 2
    )
    dt <- DT::formatStyle(
      table = dt,
      columns = c(fmt_p_cols, p_cols),
      target = "cell",
      color = DT::styleInterval(cuts = 0.05, values = c("red", "")),
      fontWeight = DT::styleInterval(cuts = 0.05, values = c("bold", "normal"))
    )
    return(dt)
  }
}
