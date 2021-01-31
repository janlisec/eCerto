#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
xlsxload_ExcelServer = function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    datafile = xlsxload_xlsxinputServer("xlsxfile")
    sh = xlsxload_sheetServer("sheet", datafile)
    
    
    # when sheet is selected, upload Excel and enable button
    t = shiny::reactive({
      shiny::req(sh())
      tryCatch({
        lapply(shiny::isolate(datafile()$datapath), function(x) {
          openxlsx::read.xlsx(x, sh())
        })
      }, error = function(e) {
        stop(safeError(e))
      })
      
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
xlsxload_ExcelUI = function(id) {
  shiny::tagList(xlsxload_xlsxinputUI(id = shiny::NS(id, "xlsxfile")), # upload input
                 xlsxload_sheetUI(id = shiny::NS(id, "sheet"))) # sheet select
  
  
}