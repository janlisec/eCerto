#' @title fnc_plot_lts_data.
#'
#' @description Plots for LTS module.
#'
#' @param x data.
#' @param type type of plot (see return).
#'
#' @return The plot function can be used to return only the calculated
#'     LTS value (in month) with type=0. If type=1 or 2 the normal or
#'     adjusted LTS plot will be computed additionally.
#'
#' @noRd
#' @keywords internal
plot_lts_data <- function(x = NULL, type = 1) {
  # date estimation is approximate (based on ~30d/month or precisely on 365/12=30.42)
  days_per_month <- 30.4167

  # ensure that data is ordered after time
  x[["val"]] <- x[["val"]][order(x[["val"]][, "Date"]), ]

  # get specific data
  vals <- x[["val"]][, "Value"]
  rt <- x[["val"]][, "Date"]
  mon <- calc_time_diff(rt, type = "day") / days_per_month

  # establish linear model
  foo.lm <- stats::lm(vals ~ mon)
  a <- stats::coef(foo.lm)[1]
  b <- stats::coef(foo.lm)[2]

  # extract relevant values from definition part
  U <- x[["def"]][, "U"]
  mn <- x[["def"]][, "CertVal"]
  ylab <- paste0(x[["def"]][, "KW_Def"], ifelse(is.na(x[["def"]][, "KW"]), "", paste0(" (", x[["def"]][, "KW"], ")")), " [", x[["def"]][, "KW_Unit"], "]")
  main <- x[["def"]][, "KW"]
  sub <- x[["def"]][, "U_Def"]

  # correct values by coef estimate
  foo_adj <- vals - (a - mn)
  foo_lts <- ceiling(abs(U / b))

  # color comment data points differently
  if ("Comment" %in% colnames(x[["val"]])) {
    com <- x[["val"]][, "Comment"]
  } else {
    com <- rep(NA, nrow(x[["val"]]))
  }

  # generate 'real time window' plot
  if (type == 1) {
    plot(
      vals ~ mon, type = "n",
      ylim = range(c(vals, mn + c(-1, 1) * U), na.rm = T),
      xlab = "Month [n]", ylab = ylab, sub = sub, main = main
    )
    graphics::axis(side = 3, at = range(mon), labels = rt[c(1, length(rt))])
    graphics::abline(foo.lm, lty = 2, col = 4) # <-- slope
    graphics::abline(h = mn + c(-1, 0, 1) * U, lty = c(2, 1, 2), col = c(3, 2, 3))
    graphics::points(vals ~ mon, pch = 24, bg = c(grDevices::grey(0.6), 2)[1 + !is.na(com)])
  }

  # generate 'fake time window' plot
  if (type == 2) {
    plot(
      c(foo_adj, mn + b * foo_lts) ~ c(mon, foo_lts),
      pch = 21, bg = c(c(grDevices::grey(0.6), 2)[1 + !is.na(com)], 4),
      ylim = range(c(foo_adj, mn + b * foo_lts, mn + c(-1, 1) * U)),
      xlab = "Month [n]", ylab = paste(ylab, "adjusted"), sub = sub, main = paste(main, "(adjusted)")
    )
    graphics::axis(side = 3, at = c(0, foo_lts), labels = c(rt[1], rt[1] + foo_lts * days_per_month))
    graphics::abline(stats::lm(foo_adj ~ mon), lty = 2, col = 4)
    graphics::abline(h = mn + c(-1, 0, 1) * U, lty = c(2, 1, 2), col = c(3, 2, 3))
    graphics::text(x = foo_lts, y = mn + b * foo_lts, pos = 2, labels = paste("n =", foo_lts))
  }

  names(foo_lts) <- as.character(as.POSIXlt(as.Date(rt[1] + foo_lts * days_per_month, origin = "1900-01-01")))
  invisible(foo_lts)
}
