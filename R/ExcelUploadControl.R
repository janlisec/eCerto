#' @name ExcelUploadControl
#' @aliases m_ExcelUploadControl_UI
#' @aliases m_ExcelUploadControl_Server
#'
#' @title ExcelUploadControl.
#'
#'@description \code{ExcelUploadControl} will provide a module to upload excel data files.
#'
#'@details not yet
#'
#'@param id Name when called as a module in a shiny app.
#'@param excelformat Selector for dataset type (reactive).
#'@param check Check if 'excelformat' dataset has been uploaded (FALSE) or not (TRUE) (reactive).
#'@param silent Option to print or omit status messages.
#'
#'@return A reactiveVal containing desired data
#'
#'@examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    shiny::selectInput(inputId = "excelformat", label = "excelformat",
#'      choices = c("Certifications","Homogeneity","Stability")),
#'    shiny::hr(),
#'    m_ExcelUploadControl_UI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'   out <- m_ExcelUploadControl_Server(
#'     id = "test",
#'     excelformat = reactive({input$excelformat}),
#'     check = reactive({TRUE}))
#'   #observeEvent(out(), {print(out())})
#'  }
#' )
#' }
#'
#' @rdname ExcelUploadControl
#' @export
#'
m_ExcelUploadControl_UI <- function(id) {

  shiny::tagList(
    # control elements
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(outputId = shiny::NS(id,"excel_file"))),
      shiny::column(3, shiny::numericInput(inputId = shiny::NS(id,"sheet_number"), label = "sheet_number", value = 1)),
      shiny::column(6, align="right", shiny::uiOutput(outputId = shiny::NS(id,"btn_load")))
    ),
    # preview table
    ecerto::xlsx_range_select_UI(shiny::NS(id,"Upload")),
  )
}

#' @rdname ExcelUploadControl
#' @export

m_ExcelUploadControl_Server <- function(id, excelformat, check, silent=FALSE) {

  stopifnot(shiny::is.reactive(excelformat))

  shiny::moduleServer(id, function(input, output, session) {

    output$btn_load <- shiny::renderUI({
      shiny::fluidRow(
        shiny::strong("Click to load"),
        shiny::br(),
        shiny::actionButton(inputId = shiny::NS(id, "go"), label = "LOAD")
      )
    })

    current_file_input <- shiny::reactiveVal(NULL)
    # Excel-File-Input und Sheet-number
    output$excel_file <- shiny::renderUI({
      if (check()) {
        shiny::updateNumericInput(session = session, inputId = "sheet_number", value=1)
        shinyjs::hideElement(id = "sheet_number")
        shinyjs::hideElement(id = "btn_load")
        current_file_input(NULL)
        shiny::fileInput(multiple = excelformat()=="Certifications", inputId = shiny::NS(id,"excel_file"), label = "Select Excel (xlsx)", accept = "xlsx")
      } else {
        shiny::HTML("<p style='color:red;'>Already uploaded</p>")
      }
    })

    shiny::observeEvent(input$excel_file, {
      sheetnames <- ecerto::load_sheetnames(input$excel_file$datapath)
      if (length(sheetnames)>1) {
        shiny::updateNumericInput(session = session, inputId = "sheet_number", value = 1, min = 1, max = length(sheetnames), step = 1)
        shinyjs::showElement(id = "sheet_number")
      }
      shinyjs::showElement(id = "btn_load")
      current_file_input(input$excel_file)
    })

    # --- --- --- --- --- --- --- --- --- ---
    rv_xlsx_range_select <- ecerto::xlsx_range_select_Server(
      id = "Upload",
      x = current_file_input,
      sheet = shiny::reactive({ input$sheet_number }),
      excelformat = excelformat
    )
    # --- --- --- --- --- --- --- --- --- ---                     

    out <- shiny::reactiveVal()
    # when LOAD Button is clicked
    shiny::observeEvent(input$go, {
      dat <- rv_xlsx_range_select$tab_flt
      whereami::cat_where(where = "ExcelUpload: Excel uploaded",color = "grey")
      
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
        results = tryCatch({
          expr = combine_cert_data(df_list = dat)
        },
          error = function(errormessage) {
            showModal(
              modalDialog(
                title = "Something went wrong with Upload.",
                paste(
                  "Check for example the selected rows and columns\n", 
                  errormessage
                )
              )
            )
            return(NULL)
          }
        )
        out(results)
      }
      if(excelformat() == "Stability") {
        out(dat[[1]])
      }
     
    })
    return(out)
  })
}