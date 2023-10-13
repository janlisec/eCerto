#' @title fnc_prepTabS1.
#' @description \code{prepTabS1} will perform statistics on imported homogeneity
#'    data.
#' @details tbd.
#' @param x The imported S data from an session R6 object.
#' @param time_fmt The lm was calculated using day as time basis until 20230927 and was changed to month afterwards.
#' @examples
#' x <- eCerto:::test_Stability_Excel()
#' eCerto:::prepTabS1(x = x)
#' @return A data frame.
#' @keywords internal
#' @noRd
prepTabS1 <- function(x, time_fmt = c("mon", "day"), t_cert = 60) {
  message("[prepTabS1] perform statistics on imported stability data")
  time_fmt <- match.arg(time_fmt)
  stopifnot(all(c("analyte", "Value", "Date") %in% colnames(x)))
  if (!is.numeric(t_cert) | (is.numeric(t_cert) && !(t_cert>0))) t_cert <- as.numeric(NA)
  plyr::ldply(split(x, x[,"analyte"]), function(x) {
    if (time_fmt == "day") {
      x_lm <- stats::lm(Value ~ Date, data=x)
      mon_diff <- max(calc_time_diff(x[,"Date"], type = "mon"))
      x_coef <- unname(summary(x_lm)$coefficients["Date",])
    }
    if (time_fmt == "mon") {
      x[,"Date"] <- calc_time_diff(x[,"Date"], type = "mon", exact=TRUE)
      x_lm <- stats::lm(Value ~ Date, data=x)
      mon_diff <- round(max(x[,"Date"]), 1)
      x_coef <- unname(summary(x_lm)$coefficients["Date",])
    }
    # according to B.3.4 from ISO Guide 35 which is similar to summary(lm))coef[4]
    #p_val <- 2 * stats::pt(abs(x_coef[1]/x_coef[2]), df = stats::df.residual(x_lm), lower.tail = FALSE)
    #p_val <- x_coef[4]
    #browser()
    data.frame(
      "mon_diff"=mon_diff,
      "slope"=x_coef[1],
      "SE_slope"=x_coef[2],
      "u_stab"=abs(t_cert*x_coef[2]),
      "P"=x_coef[4]
    )
  }, .id="analyte")
}