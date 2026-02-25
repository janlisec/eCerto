#' @title fnc_prepTabS2c.
#' @description \code{prepTabS2c} will calculate statistics on arrhenius data.
#' @details tbd.
#' @param x A dataframe.
#' @examples
#' x <- eCerto:::test_Stability_Arrhenius()
#' x <- eCerto:::prepTabS2a(x = x)
#' y <- eCerto:::prepTabS2b(x = x)
#' eCerto:::prepTabS2c(x = x, y = y)
#' @return A data frame.
#' @keywords internal
#' @noRd
prepTabS2c <- function(x, y) {
  e_msg("perform arrhenius statistics for Tab S2b")
  stopifnot(all(c("1/K", "log(-k_eff)") %in% colnames(x)))
  stopifnot(all(c("u(i)", "u(s)", "cov") %in% colnames(y)))
  ce <- stats::coef(stats::lm(x[, "log(-k_eff)"] ~ x[, "1/K"]))
  a <- ce[2]
  b <- ce[1]
  out <- x
  out[, "log(k)_calc"] <- a * x[, "1/K"] + b
  out[, "CI_upper"] <- sqrt(y[, "u(i)"]^2 + y[, "u(s)"]^2 * x[, "1/K"]^2 + 2 * y[, "cov"] * x[, "1/K"]) + out[, "log(k)_calc"]
  out[, "CI_lower"] <- 2 * out[, "log(k)_calc"] - out[, "CI_upper"]
  return(out)
}
