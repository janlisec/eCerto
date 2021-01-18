



# EXCEL FILE SELECTION MODULE UI ------------------------------------------------------------

.input.xlsxUI = function(id) {
  fileInput(inputId = NS(id, "file"), label = "Test-Upload (.xlsx format)")
}

# EXCEL FILE SELECTION MODULE SERVER ------------------------------------------------------------

.input.xlsxServer = function(id) {
  moduleServer(id, function(input, output, session) {

    # The selected file, if any
    reactive({
      # If no file is selected, don't do anything
      req(input$file)
      input$file
    })
  })
}

# SHEET SELECTION MODULE UI ------------------------------------------------------------

.input.sheetUI = function(id) {
  selectInput(NS(id, "sheet"), choices = NULL, label = "Sheet")
}

# SHEET SELECTION MODULE SERVER ------------------------------------------------------------

.input.sheetServer = function(id, datafile) {
  stopifnot(is.reactive(datafile))
  # TODO check if datafile is really an excel
  
  moduleServer(id, function(input, output, session) {
    #excel-file is uploaded --> update selectInput of available sheets
    observeEvent(datafile(), {
      choices_list = openxlsx::getSheetNames(datafile()$datapath[1])
      updateSelectInput(session = session,
                        inputId = "sheet",
                        choices = choices_list)
    })
    #eventReactive(datafile(),input$sheet)
    reactive(input$sheet)
  })
}


# UPLOAD MODULE UI --------------------------------------------------------


.ExcelUI = function(id) {
  tagList(.input.xlsxUI(id = NS(id, "xlsxfile")), # upload input
          .input.sheetUI(id = NS(id, "sheet"))) # sheet select
  
  
}


# UPLOAD MODULE SERVER ------------------------------------------------------------

.ExcelServer = function(id) {
  moduleServer(id, function(input, output, session) {
    datafile = .input.xlsxServer("xlsxfile")
    sh = .input.sheetServer("sheet", datafile)

    
    # when sheet is selected, upload Excel and enable button
    t = reactive({
      req(sh())
      openxlsx::read.xlsx(isolate(datafile()$datapath[1]), sh())
    })
  })
}

# PARAMETER MODULE SERVER ---------------------------------------------

.parametertabsServer = function(id, dat, excelformat) {
  stopifnot(is.reactive(dat))
  moduleServer(id, function(input, output, session) {
    observeEvent(excelformat(), {
      updateTabsetPanel(session = session,
                        inputId = "params",
                        selected = excelformat())
      sliderupdate(session, dat)
    })
    
    # update slider when new data set
    observeEvent(dat(), {
      sliderupdate(session, dat)
    })
    
    # TODO validation part here
    
    # returns list with selected additional parameters (if any)
    list(
      param_format = reactive(excelformat()),
      start_row = reactive(input$rowslider[1]),
      end_row = reactive(input$rowslider[2]),
      start_col = reactive(input$colslider[1]),
      end_col = reactive(input$colslider[2])
    )
  })
}


sliderupdate = function(session, dat) {
  # "deletes" all previous settings:
  updateSliderInput(
    session = session,
    inputId = "rowslider",
    min = 0,
    max = nrow(dat()),
    value = c(0, nrow(dat()))
  )
  updateSliderInput(
    session = session,
    inputId = "colslider",
    min = 0,
    max = ncol(dat()),
    value = c(0, ncol(dat()))
  )
}

# PARAMETER MODULE UI -----------------------------------------------------

.parameter_UI = function(id) {
  tagList(tabsetPanel(
    id = NS(id, "params"),
    type = "hidden",
    tabPanel(
      "Certifications",
      sliderInput(
        NS(id, "rowslider"),
        "Rows",
        value = c(0, 20),
        min = 0,
        max = 100
      ),
      sliderInput(
        NS(id, "colslider"),
        "Columns",
        value = c(0, 1),
        min = 0,
        max = 2
      )
    ),
    tabPanel("Homogeneity"),
    tabPanel("Stability")
  ))
}


# UPLOAD TABSETS MODULE UI ------------------------------------------------


.uploadTabsetsUI = function(id) {
  fluidRow(column(id = NS(id,"leftcol"),width = 4,
                  .ExcelUI(NS(id, "upld")),
                  .parameter_UI(NS(id, "pam")), ),
           column(width = 8,
                  p("Preview (first 6 lines)"),
                  verbatimTextOutput(NS(id, "preview_out"))))
}

# UPLOAD TABSETS MODULE SERVER --------------------------------------------

.uploadTabsetsServer = function(id, excelformat, dat) {
  # stopifnot(!is.reactivevalues(dat))
  moduleServer(id, function(input, output, session) {
    t = .ExcelServer("upld") # call module that gives initial table
    param =  .parametertabsServer("pam", t,  excelformat)
    

    observeEvent(excelformat(),{
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
    
    
    output$preview_out = renderPrint(
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
    
    reactive({
      # t()[param$start_row():param$end_row(),]
      t()[param$start_row():param$end_row(), param$start_col():param$end_col()]
    })
  })
}

# IMPORT CONTROLLER MODULE UI ------------------------------------------------------------

.ImportCntrlUI = function(id) {
  tagList(
    selectInput(
      inputId = NS(id, "moduleSelect"),
      choices = NULL,
      label = "module",
      width = "50%"
    ),
    .uploadTabsetsUI(NS(id, "test")),
    actionButton(inputId = NS(id, "go"),
                 label = "LOAD"#, disabled = FALSE
    ),
  )
}

# IMPORT CONTROLLER MODULE SERVER ----------------------------------------------

.ImportCntrlServer = function(id, c) {
  stopifnot(is.reactivevalues(c))
  moduleServer(id, function(input, output, session) {
    updateSelectInput(inputId = "moduleSelect",
                      session = session,
                      choices =  isolate(names(c)))
    
  
    choosen = eventReactive(input$moduleSelect,{c[[input$moduleSelect]]},ignoreInit = TRUE )
    
    t = .uploadTabsetsServer("test", reactive({input$moduleSelect}), choosen) 
    
    # must be extra disabled after loading, since is in parent module of upload panel
    observeEvent(input$moduleSelect, {
      if(is.null(c[[input$moduleSelect]])){
        shinyjs::enable("go")
      } else {
        shinyjs::disable("go")
      }
    }, ignoreInit = TRUE)
    
    # update list ater pushing upload button
    observeEvent(input$go, {
      #if(is.null(c[[input$moduleSelect]])){
        c[[input$moduleSelect]] = t()
        print("go")
        shinyjs::disable("go")
      #}
      
      # --> ... 1) change label to "reload"
      # updateActionButton(session = session,
      #                    inputId = "go",
      #                    label = "Reload")
      # disable("go")
      # TODO "choice" grün färben
      
      # TODO disable panel
      
    })
    
    # builds a reactive expression that only invalidates
    # when the value of input$go becomes out of date
    # (i.e., when the button is pressed)
    # eventReactive(input$go, {
    #   # --> ... 3) return the data list
    #   c
    # })
    
  })
}


# main -------------------------------------------------------------------


library(shiny)
library(shinyjs)

upld.cntrller = list(
  "Certifications" = NULL,
  "Homogeneity" = NULL,
  "Stability" = NULL
)


ui = fluidPage(shinyjs::useShinyjs(),
                wellPanel(.ImportCntrlUI("excelfile")),
                wellPanel(verbatimTextOutput("out")))

server = function(input, output, session) {
  rv = do.call("reactiveValues", upld.cntrller)
  excelfile = .ImportCntrlServer("excelfile", rv)
  # observeEvent(excelfile(),{
  #   rv() = excelfile()
  # },ignoreInit = TRUE)
  output$out = renderPrint(reactiveValuesToList(rv), width = 40)
  # output$out = renderPrint(is.reactivevalues(rv()))
}

shinyApp(ui, server)
