# to test without rebuild one could source all R Files
# for (f in dir("R/", full.names = TRUE)) source(file = f, encoding = "utf8")
# shiny::shinyApp(ui = app_ui(), server = app_server, enableBookmarking = "server")
#
#' Title
#'
#' @return
#' @export
app <- function(){

  shiny::shinyApp(ui = app_ui(), server = app_server, enableBookmarking = "server")

}
