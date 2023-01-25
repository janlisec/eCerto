#' @title fnc_prepTabS1.
#' @description \code{prepTabS1} will perform statistics on imported homogeneity
#'    data.
#' @details tbd.
#' @param x The imported S data from an session R6 object.
#' @examples
#' x <- eCerto:::test_Stability_Excel()
#' eCerto:::prepTabS1(x = x)
#' @return A data frame.
#' @keywords internal
#' @noRd
prepTabS1 <- function(x) {
  message("[prepTabS1] perform statistics on imported stability data")
  stopifnot(all(c("analyte", "Value", "Date") %in% colnames(x)))
  plyr::ldply(split(x, x[,"analyte"]), function(x) {
    x_lm <- stats::lm(Value ~ Date, data=x)
    mon_diff <- max(calc_time_diff(x[,"Date"], type = "mon"))
    x_slope <- summary(x_lm)$coefficients[2,1:2]
    # according to B.3.4 from ISO Guide 35
    p_val <- 2 * stats::pt(abs(x_slope[1]/x_slope[2]), df = stats::df.residual(x_lm), lower.tail = FALSE)
    data.frame(
      "mon_diff"=mon_diff,
      "slope"=x_slope[1],
      "SE_slope"=x_slope[2],
      "u_stab"=abs(x_slope[1]*x_slope[2]),
      "P"=p_val)
  }, .id="analyte")
}