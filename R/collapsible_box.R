#'@title collapsible_box.
#'
#'@description
#'\code{collapsible_box} will provide an input element (box) from `shinydashboard`.
#'
#'@details
#'tbd.
#'
#'@param x The Cert data from an session R6 object.
#'@param c_apm The parameters of the current analyte from an session R6 object.
#'
#'@examples
#' if (interactive()) {
#' shiny::shinyApp(
#'   shiny::fluidPage(
#'     collapsible_box(title = "My box", status = "danger"),
#'   ),
#'   function(input, output) {}
#' )
#'}
#'@return
#'An box input element.
#'
#'@keywords internal
collapsible_box <- function(title = NULL, status = NULL, width = 6, height = NULL) {
  dashboard_ui <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(),
    shinydashboard::dashboardSidebar(),
    shinydashboard::dashboardBody()
  )
  dashboard_deps <- htmltools::findDependencies(dashboard_ui)
  shiny::tagList(
    shinydashboard::box(title = title, status = status, collapsible = TRUE, width = width, height = height),
    dashboard_deps
  )
}