#' @title fnc_prepFigS2.
#' @description \code{prepFigS2} will generate a temp dependend set of plots.
#' @details tbd.
#' @param tmp The S data from an session R6 object.
#' @param show_reference_point show_reference_point.
#' @param plot_nominal_scale plot_nominal_scale.
#' @param plot_in_month plot_in_month.
#' @param plot_ln_relative plot_ln_relative.
#' @examples
#' x <- eCerto:::test_Stability_Arrhenius(3)
#' x$Value <- x$Value/mean(x$Value[x$time==0])
#' eCerto:::prepFigS2(tmp = x)
#' eCerto:::prepFigS2(tmp = x, show_reference_point = FALSE)
#' eCerto:::prepFigS2(tmp = x, plot_nominal_scale = FALSE)
#' eCerto:::prepFigS2(tmp = x, plot_in_month = FALSE)
#' eCerto:::prepFigS2(tmp = x, plot_ln_relative = FALSE)
#' @return A data frame.
#' @importFrom graphics par
#' @keywords internal
prepFigS2 <- function(tmp, show_reference_point = TRUE, plot_nominal_scale = TRUE, plot_in_month = TRUE, plot_ln_relative = TRUE) {
  stopifnot(is.data.frame(tmp))
  stopifnot(all(c("time", "Value", "Temp") %in% colnames(tmp)))
  stopifnot(is.numeric(tmp[, "time"]))
  stopifnot(is.numeric(tmp[, "Value"]))
  if (min(tmp[, "time"])!=0) warning("Variable 'time' should be in days and start with day 0.")
  if (mean(tmp[tmp[, "time"]==0, "Value"])!=1) warning("Variable 'Value' should be standardized to mean of t=0.")
  time <- tmp[, "time"]
  val <- tmp[, "Value"]
  if (plot_in_month) time <- round(time * 12 / 365, 2)
  if (plot_nominal_scale) time <- factor(time)
  if (plot_ln_relative) val <- log(val)
  tf <- factor(tmp[, "Temp"])
  if (length(levels(tf))>8) message("Nore than 8 Temp levels are not well supported in plotting.")
  pchs <- c(21:25, 21:23)[as.numeric(tf)]
  cols <- c(1:8)[as.numeric(tf)]
  mns <- tapply(val, list(tmp[, "Temp"], time), mean, na.rm = TRUE)
  sds <- tapply(val, list(tmp[, "Temp"], time), stats::sd, na.rm = TRUE)
  xlim <- range(as.numeric(time), na.rm = TRUE)
  ylim <- range(c(mns - sds, mns + sds, val), na.rm = TRUE)
  ylim <- ifelse(plot_ln_relative, 0, 1) + c(-1, 1) * max(abs(ylim - ifelse(plot_ln_relative, 0, 1)))
  cex_plot <- 1.5
  opar <- graphics::par(no.readonly = TRUE)
  on.exit(par(opar))
  graphics::par(mar = c(5.5, 4.5, 1, 1))
  graphics::par(mfrow = c(1, length(levels(tf)) - 1))
  graphics::par(cex.lab = cex_plot * 1.1, cex.axis = cex_plot * 1.1)
  for (k in levels(tf)[-1]) {
    plot(xlim, ylim, xlab = ifelse(plot_in_month, "Month", "Days"), ylab = ifelse(plot_ln_relative, "log(Relative value)", "Relative value"), type = "n", main = "", axes = FALSE)
    graphics::mtext(text = paste0(k, "\u00b0", "C"), side = 1, line = -1.8, adj = 0.98, cex = cex_plot)
    graphics::axis(2)
    graphics::abline(h = ifelse(plot_ln_relative, 0, 1), col = grDevices::grey(0.9), lwd = 3)
    flt <- time == 0
    if (show_reference_point) {
      graphics::points(y = val[flt], x = time[flt], pch = 21, bg = grDevices::grey(0.9), cex = 2)
      graphics::abline(h = mean(val[flt], na.rm = TRUE) + c(-1, 1) * stats::sd(val[flt], na.rm = TRUE), col = grDevices::grey(0.9), lwd = 1, lty = 2)
    }
    if (plot_nominal_scale) {
      tmp.x <- 1:length(levels(factor(time)))
      graphics::axis(1, at = tmp.x, labels = levels(factor(time)))
    } else {
      tmp.x <- as.numeric(levels(factor(time)))
      graphics::axis(1)
    }
    graphics::box()
    graphics::lines(x = tmp.x, y = mns[k, ] - sds[k, ], col = unique(cols[tf == k]), lwd = 1, lty = 2)
    graphics::lines(x = tmp.x, y = mns[k, ], col = unique(cols[tf == k]), lwd = 3)
    graphics::lines(x = tmp.x, y = mns[k, ] + sds[k, ], col = unique(cols[tf == k]), lwd = 1, lty = 2)
    flt <- tmp[, "Temp"] == k
    graphics::points(y = val[flt], x = time[flt], pch = pchs[flt], bg = cols[flt], cex = 2)
    if (!plot_ln_relative) {
      graphics::mtext(text = paste0("recovery = ", round(100 * mean(val[flt], na.rm = T), 1), "%"), side = 3, line = -1.8, adj = 0.02, cex = cex_plot)
      graphics::mtext(text = paste0("(RSD = ", round(100 * stats::sd(val[flt], na.rm = T) / mean(val[flt], na.rm = T), 1), "%)"), side = 3, line = -3.6, adj = 0.02, cex = cex_plot)
    }
    if (plot_ln_relative & plot_in_month) {
      lm_res <- stats::coef(stats::lm(val[flt] ~ as.numeric(as.character(time[flt]))))
      graphics::mtext(text = paste("slope =", round(lm_res[2], 4)), side = 3, line = -1.8, adj = 0.98, col = ifelse(lm_res[2] < 0, 3, 2), cex = cex_plot)
    }
  }
  invisible(NULL)
}
