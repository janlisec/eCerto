#' updates slider in excel tabset
#'
#' @param session 
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
sliderupdate = function(session, dat) {
  # "deletes" all previous settings:
  shiny::updateSliderInput(
    session = session,
    inputId = "rowslider",
    min = 0,
    max = nrow(dat()),
    value = c(0, nrow(dat()))
  )
  shiny::updateSliderInput(
    session = session,
    inputId = "colslider",
    min = 0,
    max = ncol(dat()),
    value = c(0, ncol(dat()))
  )
}