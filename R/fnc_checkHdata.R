#' @title fnc_checkHdata.
#' @description \code{checkHdata} will check imported homogeneity data.
#' @details tbd.
#' @param x The Hom data from an session R6 object.
#' @examples
#' \donttest{
#' x <- eCerto:::test_homog()$data
#' eCerto:::checkHdata(x = x)
#' eCerto:::checkHdata(x = x[, -2])
#' }
#' @return A data frame with at least columns 'analyte', 'H_type', 'Flasche' and 'value'.
#' @keywords internal
#' @noRd
checkHdata <- function(x) {
  e_msg("ensure integrity of imported homogeneity data")
  # ensure that x is a data frame
  if (!is.data.frame(x)) x <- as.data.frame(x)
  # ensure that column 'analyte' exists, is unique, first column of df and converted to factor keeping order of elements
  x <- assert_col(df = x, name = "analyte", pos = 1, type = "factor", default_value = "analyte")
  # ensure that there is a second column 'H_type' and convert to factor with at least one level
  x <- assert_col(df = x, name = "H_type", pos = 2, type = "factor", default_value = "hom")
  # ensure that there is a third column 'Flasche' and convert to factor
  x <- assert_col(df = x, name = "Flasche", pos = 3, type = "factor", default_value = "F")
  # ensure that there is a fourth column 'value' and convert to numeric
  x <- assert_col(df = x, name = "value", pos = 4, type = "numeric", default_value = 0)
  # return checked data
  return(x)
}
