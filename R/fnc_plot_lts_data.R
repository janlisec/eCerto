#' @title fnc_plot_lts_data.
#'
#' @description Plots for LTS module.
#'
#' @param x data.
#' @param type type of plot (see return).
#' @param t_cert The time of certification (in month). If provided it will be
#'     used to calculate u_stab according to Iso Guide 35 section 8.7.3.
#' @param slope_of_means Average replicate measurements (same Date) before
#'     computing linear model and SE of slope.
#'
#' @return The plot function can be used to return only the calculated
#'     LTS value (in month) with type=0. If type = 1 or 2 the normal or
#'     adjusted LTS plot will be computed additionally. For type = 3, the
#'     CI of the regression line will be shown additionally.
#'
#' @examples
#' x <- eCerto::LTS001[[1]]
#' (plot_lts_data(x = x, type = 0))
#' (plot_lts_data(x = x))
#' plot_lts_data(x = x, type = 2)
#' plot_lts_data(x = x, type = 3)
#' plot_lts_data(x = x, type = 1, t_cert = 60)
#' plot_lts_data(x = x, type = 1, slope_of_means = TRUE)
#'
#' @noRd
#' @keywords internal
plot_lts_data <- function(x = NULL, type = 1, t_cert = 0, slope_of_means = FALSE) {
  # date estimation is approximate (based on ~30d/month or precisely on 365/12=30.42)
  days_per_month <- 30.41667

  # ensure that data is ordered after time
  x[["val"]] <- x[["val"]][order(x[["val"]][, "Date"]), ]

  # calculate means per Date if specified in parameters
  if (slope_of_means) {
    com_exist <- "Comment" %in% colnames(x[["val"]])
    x[["val"]] <- plyr::ldply(split(x[["val"]], x[["val"]][,"Date"]), function(y) {
      data.frame(
        "Value" = mean(y[,"Value"]),
        "Date" = y[1,"Date"],
        "Comment" = ifelse(com_exist && sum(nchar(y[,"Comment"]), na.rm=TRUE)>=1, paste(y[,"Comment"], collapse=", "), NA)
      )
    }, .id = NULL)
  }

  # get specific data
  vals <- x[["val"]][, "Value"]
  rt <- x[["val"]][, "Date"]
  mon <- calc_time_diff(rt, type = "day") / days_per_month

  # establish linear model
  foo.lm <- stats::lm(vals ~ mon)
  a <- stats::coef(foo.lm)[1]
  b <- stats::coef(foo.lm)[2]
  SE_b <- summary(foo.lm)$coefficients["mon", 2]
  #b.ci <- confint(object = foo.lm, parm = 'mon', level = 0.95)

  # extract relevant values from definition part
  U <- x[["def"]][, "U"]
  mn <- x[["def"]][, "CertVal"]
  ylab <- paste0(x[["def"]][, "KW_Def"], ifelse(is.na(x[["def"]][, "KW"]), "", paste0(" (", x[["def"]][, "KW"], ")")), " [", x[["def"]][, "KW_Unit"], "]")
  main <- x[["def"]][, "KW"]
  sub <- x[["def"]][, "U_Def"]
  sub <- paste0("green lines: ", sub, ", blue line: slope, red line: ", "mean")
  if (t_cert>0) {
    sub <- paste0(sub, ", u_stab(t_cert = ", t_cert, "): ", pn(round(SE_b*t_cert, 4)))
  }

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
    if (t_cert>0) {
      x_end <- max(c(max(mon), t_cert, U/SE_b))
      graphics::polygon(x = c(0, x_end, x_end, 0), y = c(mn,mn+SE_b*x_end,mn-SE_b*x_end,mn), col = grDevices::grey(0.9), border = NA)
    }
    graphics::abline(foo.lm, lty = 2, col = 4) # <-- slope
    graphics::abline(h = mn + c(-1, 0, 1) * U, lty = c(2, 1, 2), col = c(3, 2, 3))
    if ("Temp" %in% colnames(x[["val"]])) {
      # accelerated study plot version indicating different symbols for different Temp levels
      temps <- color_temperature_levels(x = x[["val"]][, "Temp"])
      graphics::points(vals ~ mon, pch = temps$pchs, bg = temps$cols)
    } else {
      # standard plot using grey points and highlighting commented values if present
      graphics::points(vals ~ mon, pch = 24, bg = c(grDevices::grey(0.6), 2)[1 + !is.na(com)])
    }
  }

  # generate 'fake time window' plot
  if (type %in% c(2,3)) {
    plot(
      c(foo_adj, mn + b * foo_lts) ~ c(mon, foo_lts),
      ylim = range(c(foo_adj, mn + b * foo_lts, mn + c(-1, 1) * U)), type = "n",
      xlab = "Month [n]", ylab = paste(ylab, "adjusted"), sub = sub, main = ifelse(is.na(main), "", paste(main, "(adjusted)"))
    )
    adj.lm <- stats::lm(foo_adj ~ mon)
    graphics::axis(side = 3, at = c(0, foo_lts), labels = c(rt[1], rt[1] + foo_lts * days_per_month))
    if (type == 3) {
      #browser()
      ## the solution calculating CI for predicted (y_hat) values
      newx <- seq(min(c(mon, foo_lts)), max(c(mon, foo_lts)), length.out=length(mon))
      preds <- stats::predict(adj.lm, newdata = data.frame(mon=newx), interval = 'confidence')
      graphics::polygon(c(rev(newx), newx), c(rev(preds[ ,3]), preds[ ,2]), col = grDevices::grey(0.9), border = NA)
      #lines(newx, preds[ ,3], lty = 'dashed', col = 'blue')
      #lines(newx, preds[ ,2], lty = 'dashed', col = 'blue')

      ## ISO Guide (B.21) solution
      # ...

      ## the solution calculating CI for the lm coefficients
      # adj.ci <- confint(adj.lm)
      # apply(adj.ci, 2, function(x) { graphics::abline(x, col=grey(0.9)) })
      # x.max <- max(c(mon, foo_lts))
      # polygon(x = rep(c(0, x.max), each=2), y = c(adj.ci[1,], rev(adj.ci[1,] + adj.ci[2,]*x.max)), col = grey(0.9), border = NA)
    }
    graphics::abline(h = mn + c(-1, 0, 1) * U, lty = c(2, 1, 2), col = c(3, 2, 3))
    graphics::abline(adj.lm, lty = 2, col = 4)
    graphics::text(x = foo_lts, y = mn + b * foo_lts, pos = 2, labels = paste("n =", foo_lts))
    graphics::points(x = c(mon, foo_lts), y = c(foo_adj, mn + b * foo_lts), pch = 21, bg = c(c(grDevices::grey(0.6), 2)[1 + !is.na(com)], 4))

  }

  names(foo_lts) <- as.character(as.POSIXlt(as.Date(rt[1] + foo_lts * days_per_month, origin = "1900-01-01")))
  invisible(foo_lts)
}
