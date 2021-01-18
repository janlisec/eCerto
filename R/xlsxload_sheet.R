#' Title
#'
#' @param id 
#' @param datafile 
#'
#' @return
#' @export
#'
#' @examples
xlsxload_sheetServer = function(id, datafile) {
  stopifnot(is.reactive(datafile))
  # TODO check if datafile is really an excel
  
  shiny::moduleServer(id, function(input, output, session) {
    #excel-file is uploaded --> update selectInput of available sheets
    shiny::observeEvent(datafile(), {
      choices_list = openxlsx::getSheetNames(datafile()$datapath[1])
      shiny::updateSelectInput(session = session,
                               inputId = "sheet",
                               choices = choices_list)
    })
    #eventReactive(datafile(),input$sheet)
    shiny::reactive(input$sheet)
  })
}

#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
xlsxload_sheetUI = function(id) {
  shiny::selectInput(shiny::NS(id, "sheet"), choices = NULL, label = "Sheet")
}