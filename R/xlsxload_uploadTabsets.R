#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
xlsxload_uploadTabsetsUI = function(id) {
  shiny::fluidRow(shiny::column(id = shiny::NS(id,"leftcol"),width = 4,
                                xlsxload_ExcelUI(shiny::NS(id, "upld")),
                                xlsxload_parameter_UI(shiny::NS(id, "pam")), ),
                  shiny::column(width = 8,
                                shiny::p("Preview (first 6 lines)"),
                                shiny::verbatimTextOutput(shiny::NS(id, "preview_out"))))
}

#' Title
#'
#' @param id 
#' @param excelformat 
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
xlsxload_uploadTabsetsServer = function(id, excelformat, dat) {
  # stopifnot(!is.reactivevalues(dat))
  shiny::moduleServer(id, function(input, output, session) {
    t = xlsxload_ExcelServer("upld") # call module that gives initial table
    param =  xlsxload_parametertabsServer("pam", t,  excelformat)
    
    
    shiny::observeEvent(excelformat(),{
      if(is.null(dat())){
        print("is null")
        shinyjs::enable(id = "leftcol")
      } else if(!is.null(dat())) {
        print("is not null")
        shinyjs::disable(id = "leftcol")
      } else {
        print("wat anderes")
        shinyjs::enable(id = "leftcol")
      }
    })
    
    
    output$preview_out = shiny::renderPrint(
      if(is.null(dat())){
        list(
          paste0(excelformat()," has not been uploaded yet"),
          head(t()[param$start_row():param$end_row(), param$start_col():param$end_col()])
        )
        # t()[param$start_row():param$end_row(), param$start_col():param$end_col()]
      } else {
        head(dat())
      }
    )
    
    shiny::reactive({
      # t()[param$start_row():param$end_row(),]
      t()[param$start_row():param$end_row(), param$start_col():param$end_col()]
    })
  })
}