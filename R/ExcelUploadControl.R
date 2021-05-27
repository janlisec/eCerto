#'@title ExcelUploadControl.
#'
#'@description \code{ExcelUploadControl} will provide a module to upload excel
#'  data files.
#'
#'@details not yet
#'
#'@param id Name when called as a module in a shiny app.
#'@param excelformat Selector for dataset type (reactive).
#'@param check Is current dataset slot empty (TRUE) or NULL (FALSE).
#'@param silent Option to print or omit status messages.
#'
#'@return A reactive dataframe.
#'
#'@export
#'
m_ExcelUploadControl_UI = function(id) {
  shiny::tagList(
    # control elements
    fluidRow(
      column(3, uiOutput(outputId = shiny::NS(id,"excel_file"))),
      column(3, numericInput(inputId = shiny::NS(id,"sheet_number"), label = "sheet_number", value = 1)),
      column(6, align="right", uiOutput(outputId = shiny::NS(id,"btn_load")))
    ),
    # preview table
    xlsx_range_select_UI(shiny::NS(id,"Upload")),
  )
}

#'@export
m_ExcelUploadControl_Server = function(id, excelformat, check, silent=FALSE) {

  stopifnot(is.reactive(excelformat))

  shiny::moduleServer(id, function(input, output, session) {
    
    output$btn_load <- renderUI({
      fluidRow(
        strong("Click to load"),
        br(),
        shiny::actionButton(inputId = shiny::NS(id, "go"), label = "LOAD")
      )
    })

    current_file_input <- reactiveVal(NULL)
    output$excel_file <- renderUI({
      if (check()) {
        updateNumericInput(session = session, inputId = "sheet_number", value=1)
        shinyjs::hideElement(id = "sheet_number")
        shinyjs::hideElement(id = "btn_load")
        current_file_input(NULL)
        fileInput(multiple = excelformat()=="Certifications", inputId = shiny::NS(id,"excel_file"), label = "Select Excel (xlsx)", accept = "xlsx")
      } else {
        HTML("<p style='color:red;'>Already uploaded</p>")
      }
    })

    observeEvent(input$excel_file, {
      sheetnames <- load_sheetnames(input$excel_file$datapath)
      if (length(sheetnames)>1) {
        updateNumericInput(session = session, inputId = "sheet_number", value=1, min=1, max=length(sheetnames), step=1)
        shinyjs::showElement(id = "sheet_number")
      }
      shinyjs::showElement(id = "btn_load")
      current_file_input(input$excel_file)
    })

    rv_xlsx_range_select <- xlsx_range_select_Server(
      id = "Upload",
      x = current_file_input,
      sheet = reactive({ input$sheet_number }),
      excelformat = excelformat
    )

    out <- reactiveVal()
    observeEvent(input$go, {
      dat <- rv_xlsx_range_select$tab_flt
      
      # perform minimal validation checks
      if(excelformat()=="Homogeneity") {
        dat <- dat[[1]]
        if (!"analyte" %in% colnames(dat)) message("m_ExcelUploadControl_Server: observeEvent(input$go): No column 'analyte' found in input file.")
        if (!"value" %in% colnames(dat)) message("m_ExcelUploadControl_Server: observeEvent(input$go): No column 'value' found in input file.")
        if (!is.numeric(dat[,"value"])) message("m_ExcelUploadControl_Server: observeEvent(input$go): Column 'value' in input file contains non-numeric values.")
        out(dat)
      }
      if(excelformat() == "Certifications") {
        # perform minimal validation tests
        if (!length(dat)>=2) message("m_ExcelUploadControl_Server: observeEvent(input$go): Less than 2 laboratory files uploaded. Please select more files!")
        out(combine_cert_data(df_list = dat))
      }
      if(excelformat() == "Stability") {
        browser()
        out(dat[[1]])
      }
    })

    return(out)
  })
}


