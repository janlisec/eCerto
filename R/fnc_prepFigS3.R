#' @title fnc_prepFigS3.
#' @description \code{prepFigS3} will generate a temp dependend set of plots.
#' @details tbd.
#' @param tab The table containing data as basis for plot generation.
#' @examples
#' x <- data.frame(
#'   "dummy_name" = c("0", "2", "4"),
#'   "1/K" = 1/(c(0, 2, 4) + 273.15),
#'   "log(-k_eff)" = 1:3,
#'   "CI_upper" = 2:4,
#'   "CI_lower" = 0:2,
#'   "log(k)_calc" = 1:3,
#'   check.names = FALSE
#' )
#' eCerto:::prepFigS3(tab = x)
#' @return A data frame.
#' @importFrom graphics par
#' @keywords internal
prepFigS3 <- function(tab) {
  stopifnot(is.data.frame(tab))
  stopifnot(all(c("1/K", "log(-k_eff)", "CI_upper", "CI_lower", "log(k)_calc") %in% colnames(tab)))
  stopifnot(is.numeric(tab[, "1/K"]))
  stopifnot(is.numeric(tab[, "log(-k_eff)"]))
  stopifnot(is.numeric(tab[, "CI_upper"]))
  stopifnot(is.numeric(tab[, "CI_lower"]))
  stopifnot(is.numeric(tab[, "log(k)_calc"]))

  xlim <- range(tab[,"1/K"], na.rm=TRUE)
  ylim <- range(c(tab[,"log(-k_eff)"], tab[,"CI_upper"], tab[,"CI_lower"]), na.rm=TRUE)
  opar <- graphics::par(no.readonly = TRUE)
  on.exit(par(opar))
  graphics::par(mar=c(4,4.5,4,0.5))
  plot(xlim, ylim, xlab=NA, ylab=expression(ln(-k[eff])), type="n", main="")
  graphics::mtext(side = 1, line = 2.5,  adj = 0.5, text = "Inverse Temp [1/K]")
  graphics::axis(side = 3, at = 1/(273.15+as.numeric(tab[,1])), labels = tab[,1])
  graphics::mtext(side = 3, line = 2.5,  adj = 0.5, text = "Temp [\u00B0C]")
  graphics::lines(x=tab[,"1/K"], y=tab[,"CI_upper"], col=2, lwd=1, lty=2)
  graphics::lines(x=tab[,"1/K"], y=tab[,"log(k)_calc"], col=2, lwd=3)
  graphics::lines(x=tab[,"1/K"], y=tab[,"CI_lower"], col=2, lwd=1, lty=2)
  #graphics::points(y=tab[,"log(-k_eff)"], x=tab[,"1/K"], pch=21, bg=4, cex=2)
  tempC <- color_temperature_levels(as.numeric(tab[,1]))
  graphics::points(y=tab[,"log(-k_eff)"], x=tab[,"1/K"], pch=tempC$pchs, bg=tempC$cols, cex=2)
  invisible(NULL)
}