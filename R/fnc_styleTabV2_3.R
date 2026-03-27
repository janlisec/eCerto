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
  colnames(x) <- endmod(colnames(x), type = "~", fmt = "html")
  ft <- ft_default(df = x, ...)
  ft <- ft_set_formatter(ft = ft, j_idx = 2:4, fmt = ft_formatter_fixed_digits, digits = prec)
  return(ft)
}
