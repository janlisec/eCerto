#' @title Help-Page
#' @description \code{page_help} is the module for eCerto Help.
#'
#' @details A combination of all Rmd help files as one page with TOC.
#'
#' @param id Name when called as a module in a shiny app.
#'
#' @examples
#' if (interactive()) {
#'   test_nav_panel_app(
#'     panel = app_panels()$Hp,
#'     server = function(input, output, session) {
#'       eCerto:::page_helpServer(id = "Help")
#'     }
#'   )
#' }
#' @return Nothing
#' @noRd

page_helpUI <- function(id) {
  ns <- shiny::NS(id)
  if (getOption("eCerto.renderHelp", default = TRUE)) {
    #shiny::div(shiny::withMathJax(shiny::includeCSS(rmarkdown::render(input = get_local_file("help_start.Rmd"), runtime = "static", quiet = TRUE))))
    shiny::div(shiny::withMathJax(shiny::includeCSS(rmarkdown::render(input = app_sys("app/www/rmd/help_start.Rmd"), runtime = "static", quiet = TRUE))))
  } else {
    shiny::div("No help page because App is in testing mode currently.")
  }
}

#' @noRd
page_helpServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)
  })
}
