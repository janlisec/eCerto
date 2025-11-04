#' @name steyx
#' @title Implementation of the STEYX function from Excel.
#' @description Translation of `STEYX` function from Excel to R. It is implemented
#'    according to the formula described in
#'    <https://support.microsoft.com/en-us/office/steyx-function-6ce74b2c-449d-4a6e-b9ac-f9cef5ba48ab>.
#'    At least 3 finite pairs of data points are required for the calculation.
#' @param x x values as numeric vector.
#' @param y y values as numeric vector of similar length as x.
#' @examples
#' steyx(x = 1:3, y = 2:4)
#' steyx(x = 1:3, y = c(2, 3.1, 3.9))
#' @return The standard error of the predicted y-value for each x in the regression.
#' @export
steyx <- function(x, y) {
  # checks
  if (missing(x)) {
    stop("Parameter x missing.")
  }
  if (missing(y)) {
    stop("Parameter y missing.")
  }
  if (length(x) != length(y)) {
    stop("x and y not of the same length...")
  }
  if (sum(is.finite(x) & is.finite(y)) <= 2) {
    stop("Number of finite value pairs from x and y are < 3.")
  }
  # calculations
  flt <- which(is.finite(x) & is.finite(y))
  x <- x[flt]
  y <- y[flt]
  n <- length(x)
  out <- sqrt((1 / (n - 2)) * (sum((y - mean(y))^2) - ((sum((x - mean(x)) * (y - mean(y))))^2) / (sum((x - mean(x))^2))))
  # result
  return(out)
}
