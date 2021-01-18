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
      openxlsx::read.xlsx(shiny::isolate(datafile()$datapath[1]), sh())
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