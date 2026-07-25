#' @title Validation-Page
#' @description \code{page_validation} is the eCerto container module for different method validation strategies.
#' @details Not yet.
#' @param id Name when called as a module in a shiny app.
#' @examples
#' if (interactive()) {
#'   test_nav_panel_app(
#'     panel = app_panels()$V,
#'     server = function(input, output, session) {
#'       eCerto:::page_validationServer(id = "Validation")
#'     }
#'   )
#' }
#' @return Shiny tagList.
#' @noRd

page_validationUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::nav_menu(
    title = "Validation",
    icon = shiny::icon("angle-right"),
    value = "tP_Validation",
    bslib::nav_panel(
      title = "DIN 32645",
      shiny::div(
        class = "main-content",
        page_validation32645UI(id = ns("V32645"))
      )
    ),
    bslib::nav_panel(
      title = "DIN 5725-2",
      shiny::div(
        class = "main-content",
        page_validation57252UI(id = ns("V57252"))
      )
    )
  )
}

#' @noRd
page_validationServer <- function(id) {
  ns <- shiny::NS(id)
  page_validation32645Server(id = ns("V32645"))
  page_validation57252Server(id = ns("V57252"))
}
