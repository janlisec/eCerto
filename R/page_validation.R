#' @title Validation-Page
#' @description \code{page_validation} is the eCerto container module for different method validation strategies.
#' @details Not yet.
#' @param id Name when called as a module in a shiny app.
#' @param panel_style same style as for other app panels.
#' @examples
#' if (interactive()) {
#'   test_nav_panel_app(
#'     panel = app_panels()$V,
#'     server = function(input, output, session) {
#'       eCerto:::page_validationServer(id = "Validation")
#'     }
#'   )
#' }
#' @keywords internal
#' @noRd
page_validationUI <- function(id, panel_style = "padding: 72px 16px 40px; margin: 0px; overflow-y: auto;") {
  ns <- shiny::NS(id)
  bslib::nav_menu(
    title = "Validation",
    icon = shiny::icon("angle-right"),
    value = "tP_Validation",
    bslib::nav_panel(
      title = "DIN 32645",
      style = panel_style,
      page_validation32645UI(id = ns("V32645"))
    ),
    bslib::nav_panel(
      title = "DIN 5725-2",
      style = panel_style,
      page_validation57252UI(id = ns("V57252"))
    )
  )
}

#' @noRd
page_validationServer <- function(id) {
  ns <- shiny::NS(id)
  page_validation32645Server(id = ns("V32645"))
  page_validation57252Server(id = ns("V57252"))
}
