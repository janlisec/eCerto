#' @title fnc_prepTabS2a.
#' @description \code{prepTabS2a} will calculate statistics on arrhenius data.
#' @details tbd.
#' @param x A dataframe.
#' @examples
#' x <- eCerto:::test_Stability_Arrhenius()
#' eCerto:::prepTabS2a(x = x)
#' @return A data frame.
#' @keywords internal
#' @noRd
prepTabS2a <- function(x, s_opt_FigS2 = TRUE) {
  e_msg("perform arrhenius statistics for Tab S2a")
  stopifnot(all(c("Temp", "time", "Value") %in% colnames(x)))
  tmp <- x
  tf <- factor(tmp[, "Temp"])
  if ("round_time" %in% s_opt_FigS2) {
    # the version for compatibility with Bremser (round to 1/4 month precision)
    time <- tmp[, "time"] * 12 / 365
    time <- round(round(4 * time) / 4, 2)
  } else {
    # the day wise precise version
    time <- round(tmp[, "time"] * 12 / 365, 2)
  }
  val <- log(tmp[, "Value"])
  out <- ldply_base(levels(tf)[-1], function(k) {
    # the linear model shall include the reference data
    flt <- tmp[, "Temp"] == k | tmp[, "Temp"] == levels(tf)[1]
    a <- stats::coef(stats::lm(val[flt] ~ time[flt]))[2]
    # Rec and RSD are calculated without reference data
    flt <- tmp[, "Temp"] == k
    return(data.frame(
      "dummy" = k,
      "Rec" = paste0(round(100 * mean(tmp[flt, "Value"], na.rm = T), 1), "%"),
      "RSD" = paste0(round(100 * stats::sd(tmp[flt, "Value"], na.rm = T) / mean(tmp[flt, "Value"], na.rm = T), 1), "%"),
      "1/K" = 1 / (273.15 + as.numeric(k)),
      "k_eff" = a,
      "log(-k_eff)" = ifelse(a < 0, log(-a), NA),
      check.names = FALSE
    ))
  })
  colnames(out)[1] <- "T [\u00B0C]"
  return(out)
}
