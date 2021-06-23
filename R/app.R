#' App
#' 
#'
#' @return the App
#' @export
app <- function(){

  shiny::shinyApp(ui = app_ui(), server = app_server, enableBookmarking = "server")

}
