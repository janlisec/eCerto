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
#' @param show_legend annotate plot.
#' @param show_ids show_ids.
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
#' plot_lts_data(x = x, type = 1, t_cert = 60, show_legend = TRUE)
#' plot_lts_data(x = x, type = 1, slope_of_means = TRUE)
#' x[["def"]][["U_Def"]] <- "U"
#' plot_lts_data(x = x, type = 3, t_cert = 60, show_legend = TRUE)
#'
#' @noRd
#' @keywords internal
plot_lts_data <- function(x = NULL, type = 1, t_cert = 0, slope_of_means = FALSE, show_legend = FALSE, show_ids = FALSE) {

  # date estimation is approximate (based on ~30d/month or precisely on 365/12=30.42)
  days_per_month <- 30.41667

  # dont show IDs for day averages
  if (slope_of_means) show_ids <- FALSE

  # ensure that rownames exist if IDs are to be plotted
  if (show_ids && is.null(rownames(x[["val"]]))) {
    rownames(x[["val"]]) <- 1:nrow(x[["val"]])
  }

  # ensure that data is ordered after time
  x[["val"]] <- x[["val"]][order(x[["val"]][, "Date"]), ]

  # calculate means per Date if specified in parameters
  if (slope_of_means) {
    com_exist <- "Comment" %in% colnames(x[["val"]])
    x[["val"]] <- plyr::ldply(split(x[["val"]], x[["val"]][, "Date"]), function(y) {
      data.frame(
        "Value" = mean(y[, "Value"]),
        "Date" = y[1, "Date"],
        "Comment" = ifelse(com_exist && sum(nchar(y[, "Comment"]), na.rm = TRUE) >= 1, paste(y[, "Comment"], collapse = ", "), NA)
      )
    }, .id = NULL)
  }

  # get specific data
  vals <- x[["val"]][, "Value"]
  if (show_ids) names(vals) <- rownames(x[["val"]])
  rt <- x[["val"]][, "Date"]
  mon <- calc_time_diff(rt, type = "day") / days_per_month

  # establish linear model
  foo.lm <- stats::lm(vals ~ mon)
  a <- stats::coef(foo.lm)[1] # <-- intercept
  b <- stats::coef(foo.lm)[2] # <-- slope
  SE_b <- summary(foo.lm)$coefficients["mon", 2]
  # b.ci <- confint(object = foo.lm, parm = 'mon', level = 0.95)

  # extract relevant values from definition part
  U <- x[["def"]][, "U"]
  ylab <- paste0(x[["def"]][, "KW_Def"], ifelse(is.na(x[["def"]][, "KW"]) | x[["def"]][, "KW"]==x[["def"]][, "KW_Def"], "", paste0(" (", x[["def"]][, "KW"], ")")), " [", x[["def"]][, "KW_Unit"], "]")
  main <- x[["def"]][, "KW"]
  sub <- x[["def"]][, "U_Def"]
  sub <- ifelse(sub == "U", expression(U[abs]), expression(U))
  sub2 <- ifelse(is.na(x[["def"]][, "CertVal"]), "mean", expression("\u03BC"[c]))
  if (is.na(x[["def"]][, "CertVal"])) x[["def"]][, "CertVal"] <- mean(x[["val"]][, "Value"])
  mn <- x[["def"]][, "CertVal"]

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
      vals ~ mon,
      type = "n",
      ylim = range(c(vals, mn + c(-1, 1) * U), na.rm = T),
      xlim = range(c(mon), t_cert),
      xlab = "Month", ylab = ylab, main = main
    )
    graphics::axis(side = 3, at = range(mon), labels = rt[c(1, length(rt))])
    if (t_cert > 0) {
      # x_end <- max(c(max(mon), t_cert, U/SE_b))
      x_end <- t_cert
      graphics::polygon(x = c(0, x_end, x_end, 0), y = c(mn, mn + SE_b * x_end, mn - SE_b * x_end, mn), col = grDevices::grey(0.9), border = NA)
      graphics::segments(x0 = t_cert, y0 = mn - SE_b * t_cert, y1 = mn + SE_b * t_cert)
      if (show_legend) {
        graphics::legend(x = "topright", fill = grDevices::grey(0.9), legend = expression(s(b[1]) ~ "x" ~ t[cert]), bty = "n", inset = c(0.04, 0))
      }
    }
    graphics::abline(a = mn, b = b, lty = 2, col = grDevices::grey(0.8)) # <-- slope shifted to µ_c
    graphics::abline(a = a, b = b, lty = 2, col = 4) # <-- slope
    graphics::abline(h = mn + c(-1, 0, 1) * U, lty = c(2, 1, 2), col = c(3, 2, 3))
    if ("Temp" %in% colnames(x[["val"]])) {
      # accelerated study plot version indicating different symbols for different Temp levels
      temps <- color_temperature_levels(x = x[["val"]][, "Temp"])
      graphics::points(vals ~ mon, pch = temps$pchs, bg = temps$cols)
    } else {
      # standard plot using grey points and highlighting commented values if present
      graphics::points(vals ~ mon, pch = 24, bg = c(grDevices::grey(0.6), 2)[1 + !is.na(com)])
    }
    if (show_ids) {
      graphics::text(y = vals, x = mon, labels = names(vals), pos = 1)
    }
    if (show_legend) {
      x <- graphics::par("usr")[2] - diff(graphics::par("usr")[1:2]) * 0.005
      graphics::text(x = x, y = mn, labels = sub2, adj = 1)
      graphics::text(x = x, y = mn + U, labels = sub, adj = 1)
      graphics::text(x = x, y = stats::predict(foo.lm, newdata = data.frame("mon" = x)), labels = expression(b[1]), adj = 1)
      if (t_cert > 0) {
        graphics::axis(side = 1, at = t_cert, labels = NA, tcl = 0.5)
        graphics::mtext(text = expression(t[cert]), side = 1, line = -2, at = t_cert)
      }
    }
  }

  # generate 'fake time window' plot
  if (type %in% c(2, 3)) {
    ylim <- range(c(foo_adj, mn + b * foo_lts, mn + c(-1, 1) * U))
    x_min <- floor(min(c(mon, foo_lts, t_cert)))
    x_max <- ceiling(max(c(mon, foo_lts, t_cert)))
    xlim <- c(x_min, x_max)
    if (!all(is.finite(ylim))) e_msg(paste("non-finite ylim:", ylim))
    plot(
      c(foo_adj, mn + b * foo_lts) ~ c(mon, foo_lts),
      ylim = ylim, xlim = xlim, type = "n",
      xlab = "Month", ylab = paste(ylab, "adjusted"), main = ifelse(is.na(main), "", paste(main, "(adjusted)"))
    )
    x_ann <- graphics::par("usr")[2] - diff(graphics::par("usr")[1:2]) * 0.005
    adj.lm <- stats::lm(foo_adj ~ mon)
    graphics::axis(side = 3, at = c(0, foo_lts), labels = c(rt[1], rt[1] + foo_lts * days_per_month))
    if (type == 3) {
      # the solution calculating CI for predicted (y_hat) values
      newx <- seq(x_min, x_max, length.out = ifelse(diff(xlim) > 100, diff(xlim) + 1, 2*diff(xlim) + 1))
      preds <- stats::predict(adj.lm, newdata = data.frame(mon = newx), interval = "confidence")
      graphics::polygon(c(rev(newx), newx), c(rev(preds[, 3]), preds[, 2]), col = grDevices::grey(0.9), border = NA)
      # the u_LTS as suggested by the ISO Guide 35
      u_LTS <- newx*SE_b
      graphics::lines(x = newx, y = mn + u_LTS, lty = 4, lwd = 2, col = grDevices::grey(0.8))
      graphics::lines(x = newx, y = mn - u_LTS, lty = 4, lwd = 2, col = grDevices::grey(0.8))
      # the u_CRM (dependent on u_LTS) as suggested by the ISO Guide 35
      u_CRM <- mn * sapply(u_LTS, function(x) {
        #sqrt((x/mn)^2 + (U/mn)^2)
        sqrt(x^2 + (U/mn)^2)
      })
      graphics::lines(x = newx, y = mn + u_CRM, lty = 3, lwd = 2, col = grDevices::grey(0.8))
      graphics::lines(x = newx, y = mn - u_CRM, lty = 3, lwd = 2, col = grDevices::grey(0.8))
      # the suggested U_lim = U_CRM/3 according to the ISO Guide 35
      graphics::lines(x = range(newx), y = rep(mn + U/3, 2), lty = 4, col = 3)
      graphics::lines(x = range(newx), y = rep(mn - U/3, 2), lty = 4, col = 3)
      graphics::lines(x = newx, y = mn + u_CRM/3, lty = 3, lwd = 2, col = grDevices::grey(0.8))
      graphics::lines(x = newx, y = mn - u_CRM/3, lty = 3, lwd = 2, col = grDevices::grey(0.8))
      if (show_legend) {
        graphics::legend(x = "topright", fill = grDevices::grey(0.9), legend = expression(CI[95](b[1])), bty = "n", inset = c(0.04, 0))
        graphics::text(x = x_ann, y = mn + U/3, labels = expression(U[abs]/3), adj = 1)
        graphics::text(x = x_ann, y = mn + max(u_CRM)/3, labels = expression(u[CRM]/3), adj = 1)
        graphics::text(x = x_ann, y = mn + max(u_LTS), labels = expression(u[LTS]), adj = 1)
        graphics::text(x = x_ann, y = mn + max(u_CRM), labels = expression(u[CRM]), adj = 1)
      }
    }
    graphics::abline(h = mn + c(-1, 0, 1) * U, lty = c(2, 1, 2), col = c(3, 2, 3))
    graphics::abline(adj.lm, lty = 2, col = 4)
    if (show_legend) {
      graphics::text(x = x_ann, y = mn, labels = sub2, adj = 1)
      graphics::text(x = x_ann, y = mn + U, labels = sub, adj = 1)
      graphics::text(x = x_ann, y = stats::predict(adj.lm, newdata = data.frame("mon" = x_ann)), labels = expression(b[1]), adj = 1)
      if (t_cert > 0) {
        graphics::axis(side = 1, at = t_cert, labels = NA, tcl = 0.5)
        graphics::mtext(text = expression(t[cert]), side = 1, line = -2, at = t_cert)
      }
    }
    if (type == 2) {
      graphics::text(x = foo_lts, y = mn + b * foo_lts, pos = 2, labels = paste(foo_lts, "month"))
      graphics::points(x = c(mon, foo_lts), y = c(foo_adj, mn + b * foo_lts), pch = 21, bg = c(c(grDevices::grey(0.6), 2)[1 + !is.na(com)], 4))
    } else {
      # if CI_95 of regression line was calculated use the intercept with uncertainty line for life time estimation
      decreasing <- adj.lm$coefficients[2] < 0
      idx <- which.min(abs(preds[, ifelse(decreasing, 2, 3)] - (mn + ifelse(decreasing, -U, U))))
      foo_lts <- round(newx[idx])
      y_foo_lts <- preds[idx, ifelse(decreasing, 2, 3)]
      graphics::text(x = foo_lts, y = y_foo_lts, pos = 2, labels = paste(foo_lts, "month"))
      graphics::points(x = c(mon, foo_lts), y = c(foo_adj, y_foo_lts), pch = 21, bg = c(c(grDevices::grey(0.6), 2)[1 + !is.na(com)], 4))
    }
    if (show_ids) {
      graphics::text(y = foo_adj, x = mon, labels = names(vals), pos = 1)
    }
  }

  names(foo_lts) <- as.character(as.POSIXlt(as.Date(rt[1] + foo_lts * days_per_month, origin = "1900-01-01")))
  invisible(foo_lts)
}
