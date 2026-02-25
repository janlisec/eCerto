#' @title fnc_prepTabS2b.
#' @description \code{prepTabS2b} will calculate statistics on arrhenius data.
#' @details tbd.
#' @param x A dataframe.
#' @examples
#' x <- eCerto:::test_Stability_Arrhenius()
#' x <- eCerto:::prepTabS2a(x = x)
#' eCerto:::prepTabS2b(x = x)
#' @return A data frame.
#' @keywords internal
#' @noRd
prepTabS2b <- function(x, s_opt_FigS2 = TRUE) {
  e_msg("perform arrhenius statistics for Tab S2b")
  stopifnot(all(c("1/K", "log(-k_eff)") %in% colnames(x)))
  tab1 <- x
  s <- sum(tab1[, "1/K"])
  s2 <- sum(tab1[, "1/K"]^2)
  n <- nrow(tab1)
  se <- steyx(x = tab1[, "1/K"], y = tab1[, "log(-k_eff)"])
  out <- data.frame(
    "sum_x" = s,
    "sum_x2" = s2,
    "n" = n,
    "steyx" = se,
    "u(i)" = sqrt(se^2 * s2 / (s2 * n - s^2)),
    "u(s)" = sqrt(se^2 * n / (s2 * n - s^2)),
    "cov" = -1 * (se^2 * s / (s2 * n - s^2)),
    check.names = FALSE
  )
  return(out)
}
