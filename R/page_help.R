#' @title Help-Page
#' @description \code{page_help} is the module for eCerto Help.
#' @details A combination of all Rmd help files as one page with TOC.
#' @param id Name when called as a module in a shiny app.
#' @examples
#' if (interactive()) {
#'   eCerto:::test_nav_panel_app(
#'     panel = eCerto:::app_panels()$Hp,
#'     server = function(input, output, session) {
#'       eCerto:::page_helpServer(id = "Help")
#'     }
#'   )
#' }
#' @keywords internal
#' @noRd
page_helpUI <- function(id) {
  if (getOption("eCerto.renderHelp", default = FALSE)) {
    # render help new
    rmarkdown::render(input = app_sys("app/www/rmd/help_start.Rmd"), runtime = "static", quiet = TRUE)
    #shiny::div(shiny::withMathJax(shiny::includeCSS(rmarkdown::render(input = app_sys("app/www/rmd/help_start.Rmd"), runtime = "static", quiet = TRUE))))
  }
  shiny::tags$iframe(src = "www/rmd/help_start.html", style = paste("width:100%;", "height:calc(100vh - 56px - 24px);", "border:none;"))
}

#' @noRd
page_helpServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {})
}
