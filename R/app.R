#' App
#'
#' @return The eCerto App.
#' @export
app <- function() {

  # setup app by providing a resource path
  www_dir <- ifelse(file.exists("inst/app/www"), "inst/app/www", "www")
  shiny::addResourcePath(prefix = "ecerto", directoryPath = www_dir)

  # start app with app ui and server
  shiny::shinyApp(ui = app_ui(), server = app_server, enableBookmarking = "server")

}