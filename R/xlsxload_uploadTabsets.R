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
    param =  xlsxload_parametertabsServer("pam", reactive ({t()[[1]]}),  excelformat)
    
    
    shiny::observeEvent(excelformat(),{
      if(is.null(dat())){
        shinyjs::enable(id = "leftcol")
      } else if(!is.null(dat())) {
        shinyjs::disable(id = "leftcol")
      } else {
        shinyjs::enable(id = "leftcol")
      }
    })
    
    # if one parameter gets updated, re-subset all data frames by index
    a = shiny::eventReactive({
      param$change_detector()
    },{
      lapply(shiny::isolate(t()), function(x) {
        shiny::isolate(x)[as.numeric(param$start_row()):as.numeric(param$end_row()),
                   as.numeric(param$start_col()):as.numeric(param$end_col())]})
    }, ignoreInit = TRUE)
    
    
    ex = reactive({
      b1  = lapply(a(), function(x) {
        laboratory_dataframe(isolate(x))
      })
      c = data.frame(
        "Lab" = rep(paste0("L", seq_along(b1)), times = sapply(b1, nrow)),
        as.data.frame(do.call(rbind, b1)),
        #"File" = rep(input$c_input_files$name, times =sapply(data, nrow)),
        "S_flt" = FALSE,
        "L_flt" = FALSE)
      c <- c[is.finite(c[, "value"]), ] # remove non-finite values
      # perform minimal validation tests
      validate(
        need(is.numeric(c[, "value"]), message = "measurement values seem not to be numeric"),
        need(length(levels(as.factor(c[, "Lab"]))) >= 2, message = "less than 2 Labs imported")
      )
      c <- data.frame("ID" = 1:nrow(c), c)
      return(c)
    })
    
    
    
    
    output$preview_out = shiny::renderPrint(
      if(is.null(dat())){
        head(a()[[1]])
      } else {
        head(dat())
      })
    
    reactive({
      ex()
    })
  })
}