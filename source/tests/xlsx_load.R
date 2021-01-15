

# EXCEL FILE SELECTION MODULE UI ------------------------------------------------------------

.input.xlsxUI <- function(id) {
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
  
  moduleServer(id, function(input, output, session) {
    #excel-file is uploaded --> update selectInput of available sheets
    observeEvent(datafile(), {
      choices_list = openxlsx::getSheetNames(datafile()$datapath[1])
      updateSelectInput(session = session,
                        inputId = "sheet",
                        choices = choices_list)
    })
    reactive(input$sheet)
  })
}


# UPLOAD MODULE UI --------------------------------------------------------


.UploadUI = function(id){
  tagList(
    .input.xlsxUI(id = NS(id, "xlsxfile")), # upload input
    .input.sheetUI(id = NS(id, "sheet")), # sheet select
  )
  
}


# UPLOAD MODULE SERVER ------------------------------------------------------------

.UploadServer <- function(id){
  moduleServer(id, function(input, output, session) {
    datafile <- .input.xlsxServer("xlsxfile")
    sh <- .input.sheetServer("sheet", datafile)
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
      updateTabsetPanel(
        session = session,
        inputId = "params",
        selected = excelformat()
      )
      sliderupdate(session,dat)
    })
    
    # update slider when new data set
    observeEvent(dat(), {
      sliderupdate(session,dat)
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
    value = c(0,nrow(dat()))
  )
  updateSliderInput(
    session = session,
    inputId = "colslider",
    min = 0,
    max = ncol(dat()),
    value = c(0,ncol(dat()))
  )
}

# PARAMETER MODULE UI -----------------------------------------------------

.parameter_UI <- function(id) {
  tagList(
    tabsetPanel(
      id = NS(id, "params"),
      type = "hidden",
      tabPanel(
        "Certifications",
        sliderInput(NS(id,"rowslider"), "Rows",value = c(0, 20),  min = 0, max = 100),
        sliderInput(NS(id,"colslider"), "Columns", value = c(0, 20), min = 0, max = 100)
      ),
      tabPanel("Homogeneity"),
      tabPanel("Stability")
    )
  )
}

# EXCEL IMPORT MODULE UI ------------------------------------------------------------

.ImportCntrlUI <- function(id){
  fluidRow(
    column(
      width = 4,
      selectInput(
        inputId = NS(id, "moduleSelect"), 
        choices = NULL, 
        label = "module"
      ),
      
      
      
      
      .UploadUI(NS(id,"upld")),
      .parameter_UI(NS(id, "pam")),
      actionButton(
        inputId = NS(id, "go"),
        label = "LOAD",
        disabled = TRUE
      ),
    ),
    
    column(width = 8,
           p("Preview (first 6 lines)"),
           tableOutput(NS(id, "preview_out")))
  )
}

# EXCEL IMPORT MODULE SERVER ----------------------------------------------

.ImportCntrlServer <- function(id){
  moduleServer(id, function(input, output, session) {
    upld.cntrller = list(
      "Certifications" = NULL, 
      "Homogeneity" = NULL, 
      "Stability" = NULL)
    
    updateSelectInput(
      inputId = "moduleSelect",
      session = session, 
      choices =  names(upld.cntrller)
    )
    
    t = .UploadServer("upld") # call module that gives initial table
    
    # enable load button when file was uploaded:
    observeEvent(t(),{
      shinyjs::enable("go")
    })
    
    param <- .parametertabsServer("pam", t,  reactive(input$moduleSelect)) 
    
    output$preview_out <- renderTable(
      head(
        t()[param$start_row():param$end_row(),param$start_col():param$end_col()]
      )
    )
    
    observeEvent(input$go,{
      # --> ... 1) change label to "reload"
      # updateActionButton(session = session,
      #                    inputId = "go",
      #                    label = "Reload")
      disable("go")
      # TODO "choice" grün färben
      # TODO 
    })
    
    # builds a reactive expression that only invalidates 
    # when the value of input$go becomes out of date 
    # (i.e., when the button is pressed)
    eventReactive(input$go, {
      # TODO --> ... 2) disable uploaded part
      
      # --> ... 3) return the Excel file and format
      # print(param$param_format())
      list(
        xlsx =  t()[param$start_row():param$end_row(),param$start_col():param$end_col()],
        xlsx_format = param$param_format()
      )
    }) # end:input$go
  })
}


# main -------------------------------------------------------------------


library(shiny)
library(shinyjs)
# source("../read_excel_input.R")

ui <- fluidPage(useShinyjs(),
                wellPanel(.ImportCntrlUI("excelfile")),
                wellPanel(verbatimTextOutput("out")))

server <- function(input, output, session) {
  xlsxfile <- .ImportCntrlServer("excelfile")
  output$out <- renderPrint(xlsxfile(), width = 40)
}

shinyApp(ui, server)
