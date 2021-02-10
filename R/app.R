#' Title
#'
#' @return
#' @export
#' @import shiny
#'
#' @examples
app <- function(){
  
  shinyApp(ui, server, enableBookmarking = "server")

}
