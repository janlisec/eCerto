#' @title fnc_prepFigV2_3.
#' @description \code{prepFigV2_3} will style prepFigV2_3
#' @details plot repeatability values and fit data.
#' @param x The calculated repeatabilities of the imported data from validation2 module.
#' @return A data frame.
#' @examples
#' inp <- eCerto:::init_V2_data()
#' mns <- eCerto:::V2_calc_stats(inp = inp)
#' df <- eCerto:::prepTabV2_3(mns = mns)
#' eCerto:::prepFigV2_3(df = df[,2:3])
#' @keywords internal
#' @noRd
prepFigV2_3 <- function(df) {
  x <- df[,1]
  y <- df[,2]
  xlab <- markdown2expression(endmod(colnames(df)[1],"~"))
  ylab <- markdown2expression(endmod(colnames(df)[2],"~"))
  graphics::par(mar=c(4.5,3.5,0,0)+0.5)
  plot(x=x, y=y, xlim=range(c(0,max(x)+0.05*max(x))), ylim=c(0, max(y)), xaxs="i", type="n", xlab=xlab, ylab=ylab)
  # fit linear model with intercept
  graphics::abline(stats::lm(y~x), col = "blue", lwd=2)
  # fit linear model without intercept
  graphics::abline(stats::lm(y~x+0), col = "lightblue", lwd=2)
  # fit log model with intercept
  fit_lm <- stats::lm(log(y) ~ x)
  a <- exp(stats::coef(fit_lm)[1])
  b <- stats::coef(fit_lm)[2]
  xx <- seq(min(x), max(x), length.out = 200)
  yy <- a * exp(b * xx)
  graphics::lines(xx, yy, col = "orange", lwd = 2)
  graphics::points(x=x, y=y, pch=21, bg=grDevices::grey(0.8), cex=1.5)
}
