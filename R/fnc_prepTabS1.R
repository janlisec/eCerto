#' @title fnc_prepTabS1.
#' @description \code{prepTabS1} will perform statistics on imported stability
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
prepTabS1 <- function(x, time_fmt = c("mon", "day"), t_cert = 60, slope_of_means = FALSE, mt = NULL, optimize_u_stab = FALSE, adjust = FALSE) {
  e_msg("perform statistics on imported stability data")
  time_fmt <- match.arg(time_fmt)
  stopifnot(all(c("analyte", "Value", "Date") %in% colnames(x)))
  if (!is.numeric(t_cert) | (is.numeric(t_cert) && !(t_cert > 0))) t_cert <- as.numeric(NA)
  out <- ldply_base(split(x, x[, "analyte"]), function(x) {
    if (slope_of_means) {
      # compute mean values by Date
      x <- ldply_base(split(x, x[, "Date"]), function(y) {
        data.frame(
          "analyte" = y[1, "analyte"],
          "Value" = mean(y[, "Value"]),
          "Date" = y[1, "Date"]
        )
      }, .id = NULL)
    }
    if (time_fmt == "day") {
      mon_diff <- max(calc_time_diff(x[, "Date"], type = "mon"))
    }
    if (time_fmt == "mon") {
      x[, "Date"] <- calc_time_diff(x[, "Date"], type = "mon", exact = TRUE)
      mon_diff <- round(max(x[, "Date"]), 1)
    }
    if (mon_diff == 0) {
      e_msg(paste("Check", x[1,"analyte"]))
      x_coef <- rep(0,4)
      x_mean <- 0
    } else {
      x_lm <- stats::lm(Value ~ Date, data = x)
      x_coef <- unname(summary(x_lm)$coefficients["Date", ])
      x_mean <- mean(x[,"Value"], na.rm=TRUE)
    }
    # according to B.3.4 from ISO Guide 35 which is similar to summary(lm))coef[4]
    # p_val <- 2 * stats::pt(abs(x_coef[1]/x_coef[2]), df = stats::df.residual(x_lm), lower.tail = FALSE)
    # p_val <- x_coef[4]
    # according to B.3.2 [B16] and [B17] from ISO Guide 35 which is similar to summary(lm))coef[2]
    # d_in <- read.table("clipboard", sep="\t", dec=",")
    # d_means <- apply(d_in, 2, function(x) {c(mean(x), stats::sd(x))})
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

    if (optimize_u_stab) {
      # this can be used to optimize u_stab to reach a total U_abs which will fit the expected shelf life
      U_abs <- mt[mt[,"analyte"]==x[1,"analyte"],"U_abs"]
      mu_c <- mt[mt[,"analyte"]==x[1,"analyte"],"cert_val"]
      u_stab <- (abs(t_cert * x_coef[2])-ifelse(is.null(U_abs), 0, U_abs))/mu_c
      #u_stab <- (abs(t_cert * x_coef[2])-ifelse(is.null(U_abs), 0, U_abs))/x_mean
      u_stab <- ifelse(u_stab<0, 0, u_stab)
    } else {
      # this is the classic version
      u_stab <- abs(t_cert * x_coef[2])/x_mean
    }

    data.frame(
      "mon_diff" = mon_diff,
      "slope" = x_coef[1],
      "SE_slope" = x_coef[2],
      "t_cert" = t_cert,
      "mean" = x_mean,
      "u_stab" = u_stab,
      "P" = x_coef[4]
    )
  }, .id = "analyte")
  if (!is.null(adjust) && adjust) {
    out[, "P"] <- stats::p.adjust(p = out[, "P"], method = "bonferroni")
    colnames(out) <- gsub("^P$", "P_adj", colnames(out))
  }
  return(out)
}
