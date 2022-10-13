#' @title fnc_prepFigS3.
#' @description \code{prepFigS3} will generate a temp dependend set of plots.
#' @details tbd.
#' @param tab The table containing data as basis for plot generation.
#' @examples
#' x <- data.frame(
#'   "dummy_name" = c("0", "4"),
#'   "1/K" = 1/(c(0, 4)+273.15),
#'   "log(-k_eff)" = c(1, 2),
#'   "CI_upper" = c(2, 3),
#'   "CI_lower" = c(0, 1),
#'   "log(k)_calc" = c(1, 2),
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
  graphics::par(mar=c(5,4,2.5,1))
  plot(xlim, ylim, xlab="1/K", ylab="log(-k_eff)", type="n", main="")
  graphics::axis(side = 3, at = 1/(273.15+as.numeric(tab[,1])), labels = tab[,1])
  graphics::lines(x=tab[,"1/K"], y=tab[,"CI_upper"], col=2, lwd=1, lty=2)
  graphics::lines(x=tab[,"1/K"], y=tab[,"log(k)_calc"], col=2, lwd=3)
  graphics::lines(x=tab[,"1/K"], y=tab[,"CI_lower"], col=2, lwd=1, lty=2)
  graphics::points(y=tab[,"log(-k_eff)"], x=tab[,"1/K"], pch=21, bg=4, cex=2)
  invisible(NULL)
}