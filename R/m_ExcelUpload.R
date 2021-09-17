#' @name ExcelUpload
#' @aliases m_ExcelUpload_UI
#' @aliases m_ExcelUpload_Server
#'
#' @title ExcelUpload.
#'
#'@description \code{ExcelUpload} will provide a module to upload excel data files.
#'
#'@details not yet
#'
#'@param id Name when called as a module in a shiny app.
#'@param excelformat Selector for dataset type (reactive).
#'@param check Check if 'excelformat' dataset has been uploaded (FALSE) or not (TRUE) (reactive).
#'@param silent Option to print or omit status messages.
#'
#'@return A reactiveValues containing desired data and the name of the input_files
#'
#'@examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    shiny::selectInput(inputId = "excelformat", label = "excelformat",
#'      choices = c("Certification","Homogeneity","Stability")),
#'    shiny::hr(),
#'    m_ExcelUpload_UI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'   out <- m_ExcelUpload_Server(
#'     id = "test",
#'     excelformat = reactive({input$excelformat}),
#'     check = reactive({TRUE}))
#'  }
#' )
#' }
#'
#' @rdname ExcelUpload
#' @export
#'
m_ExcelUpload_UI <- function(id) {
  
  shiny::tagList(
    # control elements
    shiny::fluidRow(
      shiny::column(3, shiny::uiOutput(outputId = shiny::NS(id,"excel_file"))),
      shiny::column(3, shiny::numericInput(inputId = shiny::NS(id,"sheet_number"), label = "sheet_number", value = 1)),
      shiny::column(6, align="right", shiny::uiOutput(outputId = shiny::NS(id,"btn_load")))
    ),
    # preview table
    ecerto::m_xlsx_range_select_UI(shiny::NS(id,"Upload")),
  )
}

#' @rdname ExcelUpload
#' @export

m_ExcelUpload_Server <- function(id, excelformat, check, silent=FALSE) {
  
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
        shiny::fileInput(multiple = excelformat()=="Certification", inputId = shiny::NS(id,"excel_file"), label = "Select Excel (xlsx)", accept = "xlsx")
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
    
    
    sheetnumber = shiny::reactive({
      shiny::req(input$sheet_number)
      switch (excelformat(),
              "Certification" = input$sheet_number,
              "Homogeneity" = input$sheet_number,
              "Stability" = 1:length(ecerto::load_sheetnames(input$excel_file$datapath))
      )
    })
    # --- --- --- --- --- --- --- --- --- ---
    # Module to select rows and columns
    rv_xlsx_range_select <- m_xlsx_range_select_Server(
      id = "Upload",
      current_file_input = current_file_input,
      sheet = sheetnumber,
      excelformat = excelformat
    )
    # --- --- --- --- --- --- --- --- --- ---
    
    out <- shiny::reactiveValues(data = NULL, input_files = NULL)
    # when LOAD Button is clicked
    shiny::observeEvent(input$go, {
      # Append File column
      message("ExcelUpload: Load-button clicked")
      tab_flt = rv_xlsx_range_select$tab
      out$input_files = current_file_input()$name
      # perform minimal validation checks
      if(excelformat()=="Homogeneity") {
        tab_flt <- tab_flt[[1]]
        tab_flt[["File"]] = rep(current_file_input()$name[1], nrow(tab_flt))
        if (!"analyte" %in% colnames(tab_flt)) message("m_ExcelUpload_Server: observeEvent(input$go): No column 'analyte' found in input file.")
        if (!"value" %in% colnames(tab_flt)) message("m_ExcelUpload_Server: observeEvent(input$go): No column 'value' found in input file.")
        if (!is.numeric(tab_flt[,"value"])) message("m_ExcelUpload_Server: observeEvent(input$go): Column 'value' in input file contains non-numeric values.")
        out$data = tab_flt
      
      } else if(excelformat() == "Certification") {
        # CERTIFICATION
        if(!silent) message("m_ExcelUpload_Server: Certification ")
        for (i in 1:length(tab_flt)) {
          tab_flt[[i]][["File"]] = rep(current_file_input()$name[i], nrow(tab_flt[[i]]))
        }
        uploadExcel = function(){
          # perform minimal validation tests
          if (!length(tab_flt)>=2) message("m_ExcelUpload_Server: observeEvent(input$go): Less than 2 laboratory files uploaded. Please select more files!")
          # Try-Catch any errors during upload and open a modal window if so
          results = tryCatch({
            expr = combine_cert_data(df_list = tab_flt)
          },
          error = function(errormessage) {
            message("Excel-Upload: Error occured: ", errormessage)
            shiny::showModal(
              shiny::modalDialog(
                title = "Something went wrong with Upload.",
                paste(
                  "Check for example the selected rows and columns\n",
                  errormessage
                )
              )
            )
            return(NULL)
          })
          out$data = results
        }
        # in case (a) it is Certification module and (b) the input table has not
        # been filtered, then ask if this is correct
        if(ncol(rv_xlsx_range_select$tab_upload[[1]]) == ncol(rv_xlsx_range_select$tab[[1]]) &
           nrow(rv_xlsx_range_select$tab_upload[[1]]) == nrow(rv_xlsx_range_select$tab[[1]])
        ) {
          if(!silent) message("m_ExcelUpload_Server: Forgot select row and column?")
          shinyalert::shinyalert(
            title = "Forgot select row and column?",
            text = "You're trying to upload Certification data without selection of row and column. Are you sure to proceed?",
            showCancelButton = TRUE,
            showConfirmButton = TRUE,
            callbackR = function(x) { if(x != FALSE) uploadExcel() }
          )
        } else {
          uploadExcel()
        }
       
      } else if(excelformat() == "Stability") {
        # STABILITY
        test_format <- tab_flt[[1]] # openxlsx::read.xlsx(xlsxFile = input$s_input_file$datapath[1], sheet = 1)
        if (ncol(test_format)>=3 && "KW" %in% colnames(test_format)) {
          s_dat <- read_lts_input(
            file = input$excel_file$datapath[1],
            simplify=TRUE)
          colnames(s_dat)[colnames(s_dat)=="KW"] <- "analyte"
        } else {
          sheetnames = ecerto::load_sheetnames(input$excel_file$datapath[1])
          s_dat = plyr::ldply(
            sheetnumber(),
            function(x) {
              cbind(
                "analyte"= sheetnames[x],
                tab_flt[[x]]
                # "File" = current_file_input()$name
              )
            })
        }
        shiny::validate(shiny::need(c("analyte","Value","Date") %in% colnames(s_dat), "No all required input columns found in input file."))
        shiny::validate(shiny::need(is.numeric(s_dat[,"Value"]), "Column 'Value' in input file contains non-numeric values."))
        if (class(s_dat[,"Date"])!="Date") { s_dat[,"Date"] <- as.Date.character(s_dat[,"Date"],tryFormats = c("%Y-%m-%d","%d.%m.%Y","%Y/%m/%d")) }
        shiny::validate(shiny::need(class(s_dat[,"Date"])=="Date", "Sorry, could not convert column 'Date' into correct format."))
        s_dat[,"analyte"] <- factor(s_dat[,"analyte"])
        out$data = s_dat
      }
      
    })
    return(out)
  })
}