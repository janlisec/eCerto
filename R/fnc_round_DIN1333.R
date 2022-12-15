#' @title round_DIN1333.
#' @description Round .5 always up (non ISO standard and as defined in DIN 1333).
#' @param x A numeric vector.
#' @param n precision after decimal.
#' @examples
#' x <- c(-2.5, 2.5, 3.5)
#' round(x)
#' eCerto:::round_DIN1333(x)
#' @keywords internal
#' @noRd
round_DIN1333 <- function(x, n=0) {
  z <- abs(x)*10^n
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z/10^n
  return(sign(x)*z)
}
