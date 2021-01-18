#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
server = function(input, output, session) {
  upld.cntrller = list(
    "Certifications" = NULL,
    "Homogeneity" = NULL,
    "Stability" = NULL
  )
  
  rv = do.call("reactiveValues", upld.cntrller)
  excelfile = xlsxload_ImportCntrlServer("excelfile", rv)
  # observeEvent(excelfile(),{
  #   rv() = excelfile()
  # },ignoreInit = TRUE)
  output$out = shiny::renderPrint(shiny::reactiveValuesToList(rv), width = 40)
  # output$out = renderPrint(is.reactivevalues(rv()))
}