#' @title fnc_calc_time_diff.
#'
#' @description Calculation of a time difference between time points in a vector `x` and
#'     a specific start date `d_start` in month (days or years).
#'
#' @param x A vector of dates or character in format 'yyyy-mm-dd'.
#' @param d_start A specific start date (if unspecified the minimum of x will be used to ensure positive values).
#' @param type You may specify 'year' or 'day' instead of month here.
#' @param origin The origin used.
#' @param exact Function will return exact values instead of full month and year if this is set to TRUE.
#'
#' @examples
#' x <- c("2022-02-01", "2022-02-03", "2022-03-01", "2024-02-01")
#' calc_time_diff(x = x)
#' calc_time_diff(x = x, exact = TRUE)
#' calc_time_diff(x = x, type = "day")
#' calc_time_diff(x = x, type = "year")
#' calc_time_diff(x = x, type = "year", d_start = "2021-12-31")
#' calc_time_diff(x = 1:3, type = "day", origin = Sys.Date())
#'
#' @return A numeric vector of length `x` containing calculated time differences
#'     in the unit specified by `type`. Not a difftime object.
#' @export
calc_time_diff <- function(x = NULL, d_start = NULL, type = c("mon", "day", "year"), origin = "1900-01-01", exact = FALSE) {
  type <- match.arg(type)
  lt <- as.POSIXlt(as.Date(x, origin = origin))
  if (is.null(d_start)) d_start <- min(lt) else d_start <- as.POSIXlt(as.Date(d_start, origin = origin))
  if (exact) {
    # date estimation is approximate (based on ~30d/month or precisely on 365.25/12=30.4375)
    days_per_month <- 30.4375
    out <- switch(type,
      "mon" = as.numeric((lt - d_start) / (24 * 60 * 60)) / days_per_month,
      "year" = as.numeric((lt - d_start) / (24 * 60 * 60)) / 365,
      "day" = as.numeric((lt - d_start) / (24 * 60 * 60))
    )
  } else {
    out <- switch(type,
      "mon" = (lt$year * 12 + lt$mon) - (d_start$year * 12 + d_start$mon),
      "year" = lt$year - d_start$year,
      "day" = as.numeric((lt - d_start) / (24 * 60 * 60))
    )
  }
  return(out)
}
