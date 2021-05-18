# library(shiny)
# shinyApp(ui=xlsx_range_select_UI, server=xlsx_range_select_Server)

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

    z = reactiveVal(0) # to trigger finalization (add "File" column)

    tab <- reactive({
      validate(
        need(x(), message = FALSE),
        need(sheet(), message = FALSE),
        need(all(tools::file_ext(x()$datapath) == "xlsx"), message = "Please upload Excel only")
      )
     # l = fnc_load_xlsx(filepath = x()$datapath, sheet = sheet())
      l = load_excelfiles(filepath = x()$datapath, sheet = sheet())
      validate(
        # Check if no empty dataframe
        need(all(!unlist(lapply(l, is.null))), "Excel file must not be empty")
      )
      return(l)
    })


    rv <- reactiveValues("tab"=matrix(1), "start_row"=1, "end_row"=1, "start_col"=1, "end_col"=1, "tab_flt"=matrix(1))

    # after upload of excel file(s)
    observeEvent(tab(), {
      rv$tab <- tab()
      rv$tab_flt <- rv$tab
      rv$start_row <- 1
      rv$start_col <- 1
      rv$end_row <- nrow(tab()[[1]])
      rv$end_col <- ncol(tab()[[1]])
      new_z = z() + 1
      z(new_z)
    })

    # if rows and columns in the DT() have been selected
    observeEvent(input$uitab_cells_selected, {
      a <- input$uitab_cells_selected
      if (prod(dim(a))>2 && diff(range(a[,1]))>=2 && diff(range(a[,2]))>=2) {
        rv$start_col = min(a[,2])
        rv$end_col = max(a[,2])
        rv$start_row = min(a[,1])
        rv$end_row = max(a[,1])
        message("xlsx_range_select_Server: observeEvent(input$uitab_cells_selected): crop dataframe(s)")
        rv$tab_flt = crop_dataframes(
          dfs = rv$tab,
          rows = as.numeric(rv$start_row):as.numeric(rv$end_row),
          cols = as.numeric(rv$start_col):as.numeric(rv$end_col)
        )
        # rv$tab_flt = lapply(rv$tab, function(y) {
        #   y[as.numeric(rv$start_row):as.numeric(rv$end_row),
        #     as.numeric(rv$start_col):as.numeric(rv$end_col)]
        # })
        new_z = z() + 1
        z(new_z)
      }
    })


    observeEvent(z(),{
      if(z()>0){
        if(!"File" %in% colnames(rv$tab_flt) && rv$end_row>rv$start_row && rv$end_col>rv$start_col ){
          # add file name to each data frame
          message("xlsx_range_select_Server: observeEvent(z()): add File column")
          for (i in 1:length(rv$tab_flt)) {
            rv$tab_flt[[i]][["File"]] = rep(isolate(x()$name[i]), nrow(rv$tab_flt[[i]]))
          }
        }
      }
    })


    output$uitab <- DT::renderDT({
      req(tab())
      out <- rv$tab[[1]]
      if (prod(dim(out))>1) {
        out <- apply(out, 2, substr, start=1, stop=5)
        if (ncol(out)<=length(LETTERS)) colnames(out) <- LETTERS[1:ncol(out)]
      }
      return(out)
    }, options=list("dom"="t", pageLength=nrow(rv$tab[[1]]), ordering=FALSE),
       selection=list(
         target="cell",
         selected=matrix(c(rv$start_row, rv$end_row, rv$start_col, rv$end_col), ncol=2),
         selectable=matrix(-1*c(1:nrow(rv$tab[[1]]), rep(0,nrow(rv$tab[[1]]))), ncol=2)
       )
    )

    output$uitxt <- renderUI({
      req(tab())
      str1 <- ifelse(is.null(x()), "", paste("You see a preview of File:", x()$name))
      str2 <- "Please select 2 cells (top left and bottom right) by mouse click to specify a range."
      str3 <- paste("Currently selected range:", paste0(LETTERS[rv$start_col], rv$start_row, ":", LETTERS[rv$end_col], rv$end_row))
      HTML(paste(str1, str2, str3, sep = '<br/>'))
    })

    return(rv)

  })
}

# testServer(app = xlsx_range_select_Server, expr, args = list(), session = MockShinySession$new()) {
#
# }

# test <- function() {
#   ui <- fluidPage(
#     fileInput(inputId = "test_file", label = "test_file (xlsx)", accept = "xlsx", multiple = TRUE),
#     numericInput(inputId = "sheet_number", label = "sheet_number", value = 1),
#     hr(),
#     xlsx_range_select_UI("test")
#   )
#   server <- function(input, output, session) {
#     out <- xlsx_range_select_Server(id = "test",
#                                     x = reactive({ input$test_file }),
#                                     sheet = reactive({ input$sheet_number })
#     )
#     observeEvent(out$tab_flt, {
#       print(out$tab_flt)
#     })
#   }
#   shinyApp(ui, server)
# }
# test()
