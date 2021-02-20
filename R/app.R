#' Title
#'
#' @return
#' @export

#'
#' @examples
app <- function(){
  
  shiny::shinyApp(ui = app_ui(), server = app_server, enableBookmarking = "server")
  
}
