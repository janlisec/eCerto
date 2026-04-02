#' @title fnc_styleTabV2_3.
#' @description \code{styleTabV2_3} will style Tab.V2_3.
#' @details tabulate relevant repeatability values.
#' @param x The calculated repeatabilities of the imported data from validation2 module.
#' @return A data frame.
#' @examples
#' inp <- eCerto:::init_V2_data()
#' mns <- eCerto:::V2_calc_stats(inp = inp)
#' df <- eCerto:::prepTabV2_3(mns = mns)
#' eCerto:::styleTabV2_3(x = df)
#' @keywords internal
#' @noRd
styleTabV2_3 <- function(x, prec=3, ...) {
  colnames(x)[1] <- endmod(colnames(x)[1], type = "*", fmt = "html", spacer = " ")
  colnames(x)[2:5] <- endmod(colnames(x)[2:5], type = "~", fmt = "html")
  ft <- ft_default(df = x, ...)
  ft <- ft_set_formatter(ft = ft, j_idx = 3:5, fmt = ft_formatter_fixed_digits, digits = prec)
  means <- sprintf(paste0("%.", prec, "f"), colMeans(x[, 4:5], na.rm = TRUE))
  footer_values <- stats::setNames(as.list(c("mean", "", "", means)), colnames(x))
  ft <- flextable::add_footer(ft, values = footer_values)
  ft <- flextable::merge_at(x = ft, part = "footer", i = 1, j = 1:3)
  ft <- flextable::align(x = ft, align = "right", part = "footer", i = 1, j = 1:3)
  ft <- flextable::italic(x = ft, part = "footer", i = 1, j = 1:3)
  ft <- flextable::bold(x = ft, part = "footer", i = 1, j = 4:5)
  ft <- flextable::bg(ft, bg = grDevices::grey(0.85), part = "footer")
  ft <- flextable::hline_bottom(ft, border = list("width"=2, "color"="black", "style"="double"), part = "footer")
  return(ft)
}
