#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
xlsxload_xlsxinputServer = function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # The selected file, if any
    shiny::reactive({
      # If no file is selected, don't do anything
      shiny::req(input$file)
      input$file
    })
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
xlsxload_xlsxinputUI = function(id) {
  shiny::fileInput(inputId = shiny::NS(id, "file"), label = "Test-Upload (.xlsx format)")
}