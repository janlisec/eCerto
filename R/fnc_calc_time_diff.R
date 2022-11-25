#' @title fnc_calc_time_diff.
#'
#' @description Calculation of a time difference between time points in a vector `x` and
#'     a specific start date `d_start` in month (days or years).
#'
#' @param x A vector of dates or character in format 'yyyy-mm-dd'.
#' @param d_start A specific start date (if unspecified the minimum of x will be used to ensure positive values).
#' @param type You may specify 'year' or 'day' instead of month here.
#' @param origin The origin used.
#'
#' @examples
#' x <- c("2022-02-01", "2022-02-03", "2022-03-01", "2024-02-01")
#' calc_time_diff(x=x)
#' calc_time_diff(x=x, type="day")
#' calc_time_diff(x=x, type="year")
#' calc_time_diff(x=x, type="year", d_start="2021-12-31")
#'
#' @return A numeric vector of length `x` containing calculated time differences.
#' @export
calc_time_diff <- function(x=NULL, d_start=NULL, type=c("year","mon","day")[2], origin="1900-01-01") {
  lt <- as.POSIXlt(as.Date(x, origin=origin))
  if (is.null(d_start)) d_start <- min(lt) else d_start <- as.POSIXlt(as.Date(d_start, origin=origin))
  out <- switch(type,
                "mon" = (lt$year*12 + lt$mon) - (d_start$year*12 + d_start$mon),
                "year" = lt$year - d_start$year,
                "day" = (lt - d_start)/(24*60*60)
  )
  return(out)
}
