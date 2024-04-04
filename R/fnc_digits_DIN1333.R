#' @title digits_DIN1333.
#' @description Determine the number of digits to round a uncertainty to depending
#'    on the position of the first non-zero digit after the decimal.
#' @param x A number.
#' @examples
#' x <- c(0.011, 0.021, 0.0299999, 0.03, 0.031, 0.000299, 29.01, 3.01, 200, 300)
#' eCerto:::digits_DIN1333(x)
#' @keywords internal
#' @noRd
digits_DIN1333 <- function(x) {
  # if(!(is.numeric(x) && length(x)==1)) return(NA)
  if (!(is.numeric(x))) {
    return(NA)
  }
  xc <- as.character(x + sqrt(.Machine$double.eps))
  non_zero_pos <- sapply(gregexpr("[123456789]", as.character(x)), function(y) {
    y[1]
  })
  dec_pos <- sapply(gregexpr("[.]", xc), function(y) {
    y[1]
  })
  digit_at_pos <- substr(xc, non_zero_pos, non_zero_pos)
  rounding_pos <- non_zero_pos - dec_pos + as.numeric(digit_at_pos %in% c(1:2))
  rounding_pos[(non_zero_pos - dec_pos) < 0] <- rounding_pos[(non_zero_pos - dec_pos) < 0] + 1
  return(rounding_pos)
}
