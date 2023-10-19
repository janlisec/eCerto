#' @title fnc_prepTabS1.
#' @description \code{prepTabS1} will perform statistics on imported homogeneity
#'    data.
#' @details tbd.
#' @param x The imported S data from an session R6 object.
#' @param time_fmt The lm was calculated using day as time basis until 20230927 and was changed to month afterwards.
#' @param t_cert The time of certified shelf life + time until first PCM measurements
#' @param slope_of_means Average replicate measurements (same Date) before computing linear model and SE of slope.
#' @examples
#' x <- eCerto:::test_Stability_Excel()
#' eCerto:::prepTabS1(x = x)
#' @return A data frame.
#' @keywords internal
#' @noRd
prepTabS1 <- function(x, time_fmt = c("mon", "day"), t_cert = 60, slope_of_means = FALSE) {
  message("[prepTabS1] perform statistics on imported stability data")
  time_fmt <- match.arg(time_fmt)
  stopifnot(all(c("analyte", "Value", "Date") %in% colnames(x)))
  if (!is.numeric(t_cert) | (is.numeric(t_cert) && !(t_cert>0))) t_cert <- as.numeric(NA)
  plyr::ldply(split(x, x[,"analyte"]), function(x) {
    if (slope_of_means) {
      # compute mean values by Date
      x <- plyr::ldply(split(x, x[,"Date"]), function(y) {
        data.frame(
          "analyte" = y[1,"analyte"],
          "Value" = mean(y[,"Value"]),
          "Date" = y[1,"Date"]
        )
      }, .id = NULL)
    }
    if (time_fmt == "day") {
      mon_diff <- max(calc_time_diff(x[,"Date"], type = "mon"))
    }
    if (time_fmt == "mon") {
      x[,"Date"] <- calc_time_diff(x[,"Date"], type = "mon", exact=TRUE)
      mon_diff <- round(max(x[,"Date"]), 1)
    }
    x_lm <- stats::lm(Value ~ Date, data=x)
    x_coef <- unname(summary(x_lm)$coefficients["Date",])
    # according to B.3.4 from ISO Guide 35 which is similar to summary(lm))coef[4]
    #p_val <- 2 * stats::pt(abs(x_coef[1]/x_coef[2]), df = stats::df.residual(x_lm), lower.tail = FALSE)
    #p_val <- x_coef[4]
    #browser()
    # according to B.3.2 [B16] and [B17] from ISO Guide 35 which is similar to summary(lm))coef[2]
    # d_in <- read.table("clipboard", sep="\t", dec=",")
    # d_means <- apply(d_in, 2, function(x) {c(mean(x), sd(x))})
    # x <- rep(1:3, each=1)
    # x <- c(1, 3, 6, 12)
    # summary(lm(d_means[1,] ~ x))$coef
    # summary(lm(unlist(d_in) ~ rep(1:3, each=2)))$coef
    # x <- rep(1:3, each=2)
    # y <- unlist(d_in)
    # b0 <- lm(y ~ x)$coef[1]
    # b1 <- lm(y ~ x)$coef[2]
    # s <- sqrt(sum((y-b0-b1*x)^2)/(length(y)-2))
    # SE <- s / sqrt(sum((x - mean(x))^2))
    data.frame(
      "mon_diff" = mon_diff,
      "slope" = x_coef[1],
      "SE_slope" = x_coef[2],
      "t_cert" = t_cert,
      "u_stab" = abs(t_cert*x_coef[2]),
      "P" = x_coef[4]
    )
  }, .id="analyte")
}