# library(shiny)

xlsx_range_select_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(outputId = ns('uitxt')),
    DT::DTOutput(outputId = ns('uitab'))
  )
}

xlsx_range_select_Server <- function(id, x=NULL, sheet=NULL) {
  
  ns <- NS(id)
  
  moduleServer(id, function(input, output, session) {
    
    tab <- reactive({
      validate(need(x(), "no valid file"), need(sheet(), "no valid sheet"))
     # l = fnc_load_xlsx(filepath = x()$datapath, sheet = sheet())
      load_excelfiles(filepath = x()$datapath, sheet = sheet())
    })
    
    observeEvent(tab(), {
      rv$tab <- tab()
    })
    
    rv <- reactiveValues("tab"=matrix(1), "t"=1, "b"=1, "l"=1, "r"=1, "tab_flt"=matrix(1))
    
    output$uitab <- DT::renderDT({
      req(tab())
      rv$tab[[1]]
    }, options=list("dom"="t", pageLength=nrow(rv$tab[[1]])), selection=list(target="cell"))
    
    output$uitxt <- renderUI({
      str1 <- ifelse(is.null(x()), "", paste("You see a preview of File:", x()$name))
      str2 <- "Please select 2 cells (top left and bottom right) by mouse to specify a range."
      str3 <- paste("Current selection:", paste0(LETTERS[rv$l], rv$t, ":", LETTERS[rv$r], rv$b))
      HTML(paste(str1, str2, str3, sep = '<br/>'))
    })
    
    observeEvent(input$uitab_cells_selected, {
      a <- input$uitab_cells_selected
      
      if (prod(dim(a))>=2) {
        
        rv$l <- min(a[,2])
        rv$start_col = min(a[,2])
        rv$r <- max(a[,2])
        rv$end_col = max(a[,2])
        rv$t <- min(a[,1])
        rv$start_row = min(a[,1])
        rv$b <- max(a[,1])
        rv$end_row = max(a[,1])
        
        
        rv$tab_flt = .computation_preview_data("a", rv, rv$tab)
        if(!"File" %in% colnames(rv$tab_flt) && dim(a)[1] > 1 && dim(a)[2] >1){
          message("add File column")
          # add file name to each data frame
          for (i in 1:length(rv$tab_flt)) {
            rv$tab_flt[[i]][["File"]] = rep(isolate(x()$name[i]), nrow(rv$tab_flt[[i]]))
          }
          # filename = x$File[as.numeric(param$start_row):as.numeric(param$end_row)]
          # a = cbind(a, File = filename)
        }
        
        
        # rv$tab_flt <- rv$tab[rv$t:rv$b,rv$l:rv$r,drop=F]
      }
    })
    
    # return a reactiveValues List
    return(rv)
    
  })
}


# ui <- fluidPage(
#   fileInput(inputId = "test_file", label = "test_file (xlsx)", accept = "xlsx", multiple = TRUE),
#   numericInput(inputId = "sheet_number", label = "sheet_number", value = 1),
#   hr(),
#   xlsx_range_select_UI("test")
# )
# server <- function(input, output, session) {
#   out <- xlsx_range_select_Server(id = "test",
#                                   x = reactive({ input$test_file }),
#                                   sheet = reactive({ input$sheet_number })
#   )
#   observeEvent(out$tab_flt, {
#     print(out$tab_flt)
#   })
# }
# shinyApp(ui, server)
