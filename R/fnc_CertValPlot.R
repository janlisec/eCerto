#' @title CertValPlot.
#'
#' @description \code{CertValPlot} will generate a certified values plot.
#'
#' @details Individual samples have been stripped from the data frame upfront.
#'
#' @param data data.frame containing columns 'value', 'Lab' and 'L_flt'.
#' @param annotate_id T/F to overlay the plot with ID as text if column 'ID' is present.
#' @param filename_labels T/F to use imported file names as labels on x-axes.
#'
#' @return A specific type of boxplot.
#'
#' @examples
#' if (!interactive()) {
#'   data <- data.frame("ID" = 1:20, "value" = rnorm(20), "analyte" = "X", "Lab" = gl(2, 10), "L_flt" = FALSE)
#'   eCerto:::CertValPlot(data = data)
#'   eCerto:::CertValPlot(data = data, annotate_id = TRUE)
#'   data$File <- rep(c("Name_File_1", "F2"), each = 10)
#'   eCerto:::CertValPlot(data = data, filename_labels = TRUE)
#'   data2 <- data.frame("ID" = 1:200, "value" = rnorm(200), "analyte" = "X", "Lab" = gl(20, 10), "L_flt" = FALSE)
#'   data2$L_flt[1:10] <- TRUE
#'   eCerto:::CertValPlot(data = data2)
#'   par(mfrow = c(1, 3))
#'   eCerto:::CertValPlot(data = data)
#'   eCerto:::CertValPlot(data = data, annotate_id = TRUE, filename_labels = TRUE)
#'   eCerto:::CertValPlot(data = data2)
#'   par(mfrow = c(1, 1))
#' }
#'
#' @keywords internal
#' @noRd

CertValPlot <- function(data = NULL, annotate_id = FALSE, filename_labels = FALSE, show_legend = TRUE, dp = 4) {
  ds <- plyr::ldply(split(data[, "value"], data[, "Lab"]), function(x) {
    data.frame(
      "MW" = mean(x, na.rm = T),
      "Median" = stats::median(x, na.rm = T),
      "SD" = stats::sd(x, na.rm = T),
      "n" = sum(is.finite(x))
    )
  }, .id = "Lab")
  ds[!is.finite(ds[,"SD"]),"SD"] <- 0
  ds <- data.frame(ds, "Filter" = sapply(split(data[, "L_flt"], data$Lab), all))
  ds <- ds[order(ds$MW), ]
  dsfm <- ds$MW[!ds[, "Filter"]]
  xlabs <- as.character(ds$Lab)
  xlab <- "Lab"
  mar <- c(5, 6, 0, 0) + 0.2
  if (filename_labels & "File" %in% colnames(data)) {
    data[, "File"] <- as.character(data[, "File"])
    xlabs <- sapply(xlabs, function(x) {
      unique(sub(pattern = "(.*?)\\..*$", replacement = "\\1", basename(data[data[, "Lab"] == x, "File"])))
    })
    mar <- c(3 + floor(max(nchar(xlabs)) * 0.6), 6, 0, 0) + 0.2
    xlab <- NULL
  }
  if (all(c("analyte", "unit") %in% colnames(data))) {
    ylab <- paste0(unique(data[, "analyte"])[1], " [", unique(data[, "unit"])[1], "]")
  } else {
    ylab <- ""
  }
  opar <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = mar)
  on.exit(graphics::par(mar = opar$mar))
  # y range is minimized dependent if Sample-IDs are shown or not
  y_range <- range(c(ds$MW + ds$SD, ds$MW - ds$SD), na.rm = TRUE)
  if (annotate_id) y_range <- range(c(y_range, data[, "value"]), na.rm = TRUE)
  plot(x = range(1:nrow(ds)), y = y_range, type = "n", ann = FALSE, axes = FALSE, xlim = c(0.5, nrow(ds) + 0.5))
  graphics::title(xlab = xlab)
  # compute number of lines we need to reserve fo the axis numbers
  lh <- max(graphics::strwidth(graphics::axTicks(2), units = "inch")) / graphics::strheight("0", units = "inch")
  graphics::title(ylab = ylab, line = 1.25 + 0.75 * lh)
  graphics::axis(1, at = 1:nrow(ds), labels = xlabs, las = ifelse(filename_labels, 2, 1))
  graphics::axis(2, las = 2)
  graphics::abline(h = mean(dsfm), col = 3, lwd = 2)
  graphics::abline(h = mean(dsfm) + c(-1, 1) * stats::sd(dsfm), lty = 2, col = grDevices::grey(0.8))
  graphics::segments(x0 = 1:nrow(ds), y0 = ds$MW - ds$SD, y1 = ds$MW + ds$SD)
  lw <- 0.15
  # lw <- 0.1
  graphics::segments(x0 = 1:nrow(ds) - lw, x1 = 1:nrow(ds) + lw, y0 = ds$MW - ds$SD)
  graphics::segments(x0 = 1:nrow(ds) - lw, x1 = 1:nrow(ds) + lw, y0 = ds$MW + ds$SD)
  graphics::symbols(x = 1:nrow(ds), y = ds$MW, circles = rep(lw, nrow(ds)), bg = c(3, grDevices::grey(0.8))[1 + ds[, "Filter"]], add = T, inches = FALSE)
  # graphics::points(x=1:nrow(ds), y=ds$MW, pch=21, bg=c(3, grDevices::grey(0.8))[1+ds[,"Filter"]], cex=2)
  if (show_legend) {
    graphics::legend(x = "topleft", bty = "n", lty = c(1, 2), col = c(3, grDevices::grey(0.8)), legend = paste0(c("mean", "sd"), " (", round(c(mean(dsfm), stats::sd(dsfm)), dp), ")"))
    graphics::mtext(text = paste("n =", length(dsfm)), side = 1, adj = 0.98, line = -1.2)
  }
  if (annotate_id & "ID" %in% colnames(data)) {
    tmp_x <- sapply(data[, "Lab"], function(x) {
      which(ds[, "Lab"] == x)
    })
    graphics::text(x = jitter(tmp_x, amount = 0.25), y = data[, "value"], label = data[, "ID"], col = 4)
  }
  graphics::box()
  invisible(NULL)
}
