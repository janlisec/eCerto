#' Title
#'
#' @return
#' @export
#'
#' @examples
ui = function() {
  shiny::fluidPage(shinyjs::useShinyjs(),
                   shiny::wellPanel(xlsxload_ImportCntrlUI("excelfile")),
                   shiny::wellPanel(shiny::verbatimTextOutput("out")))
}
