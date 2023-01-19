#' @name ExcelUpload
#' @aliases m_ExcelUpload_UI
#' @aliases m_ExcelUpload_Server
#'
#' @title ExcelUpload.
#'
#' @description \code{ExcelUpload} will provide a module to upload excel data files.
#'
#' @details not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv The global R6 object..
#'
#' @return A reactiveValues containing desired data and the name of the input_files
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    shinyjs::useShinyjs(),
#'    eCerto:::m_ExcelUpload_UI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- eCerto::eCerto$new(eCerto:::init_rv()) # initiate persistent variables
#'    eCerto:::m_ExcelUpload_Server(id = "test", rv = rv)
#'    shiny::observeEvent(getValue(rv, c("Certification", "uploadsource")), { print(getValue(rv, c("Certification", "uploadsource"))) })
#'  }
#' )
#' }
#'
#' @noRd
#' @keywords internal
m_ExcelUpload_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # [JL] calling useShinyjs() here is required because ???
    shinyjs::useShinyjs(),
    shiny::tagList(
      shiny::fluidRow(
        style = "background-color: #f5f5f5; border: 1px; border-radius: 4px; border-color: #e3e3e3; border-style: solid; margin: 0px; padding-top: 6px",
        shiny::div(
          style = "width: 130px; float: left; margin-left: 15px;",
          shiny::selectInput(
            inputId = ns("moduleSelect"),
            choices = NULL,
            label = "Target module"
          )
        ),
        shiny::div(
          style = "width: 260px; float: left; margin-left: 15px;",
          shiny::uiOutput(outputId = ns("inp_file"))
        ),
        shiny::div(
          style = "width: 260px; float: left; margin-left: 15px;",
          shinyjs::hidden(shiny::selectInput(inputId = ns("file_name"), label = "File", choices = ""))
        ),
        shiny::div(
          style = "width: 70px; float: left; margin-left: 15px;",
          shinyjs::hidden(shiny::selectInput(inputId = ns("sheet_number"), label = "Sheet #", choices = "1"))
        ),
        shiny::div(
          style = "width:270px; float: left; margin-left: 15px; color: red", id = ns("info_msg")
        ),
        shiny::div(
          style = "float: right; margin-right: 15px; margin-top: 15px;",
          shinyjs::hidden(shiny::actionButton(inputId = ns("btn_load"), label = "Load selected cell range"))
        )
      )
    ),
    # preview Excel table
    m_xlsx_range_select_UI(ns("rng_select")),
  )
}

#' @noRd
#' @keywords internal
m_ExcelUpload_Server <- function(id, rv = NULL) {

  ns <- shiny::NS(id)
  silent <- get_golem_config("silent")

  shiny::moduleServer(id, function(input, output, session) {

    # remeber locally what has been uploaded already from Excel
    uploaded_datasets <- shiny::reactiveVal("")

    # Certification, Homogeneity, Stability -----------------------------------
    shiny::updateSelectInput(
      inputId = "moduleSelect",
      session = session,
      choices = getValue(rv, "modules")
    )

    exl_fmt <- shiny::reactive({input$moduleSelect})

    # monitor the status of the file selector
    current_file_input <- shiny::reactiveVal(NULL)

    # Excel-File-Input wrapped in renderUI to allow triggering some JS and empty the fileInput widget
    output$inp_file <- shiny::renderUI({
      current_file_input(NULL)
      shinyjs::hideElement(id = "sheet_number")
      shinyjs::hideElement(id = "file_name")
      shinyjs::hideElement(id = "btn_load")
      if (exl_fmt() %in% uploaded_datasets()) {
        shinyjs::html(id = "info_msg", html = shiny::HTML("Note! You have uploaded <strong>", exl_fmt(), "</strong> data already. If you upload a different file, all your selected parameters may be lost."))
      } else {
        shinyjs::html(id = "info_msg", html = "")
      }
      shiny::tagList(
        shiny::fileInput(
          inputId = session$ns("excel_file"),
          multiple = exl_fmt()=="Certification",
          #label = "Select Excel (xlsx)",
          label = shiny::tagList("Select Excel, ", shiny::actionLink(inputId = session$ns("moduleUploadHelp"), label = "example")),
          accept = "xlsx"
        )
      )
    })

    # Excel Sheet-number selector
    shiny::observeEvent(input$excel_file, {
      sheetnames <- xlsxSheetNames(input$excel_file$datapath)
      filenames <- input$excel_file$name
      if (length(sheetnames)>1) {
        shiny::updateSelectInput(session = session, inputId = "sheet_number", choices = 1:length(sheetnames))
        shinyjs::showElement(id = "sheet_number")
      } else {
        shiny::updateSelectInput(session = session, inputId = "sheet_number", choices = "1")
        shinyjs::hideElement(id = "sheet_number")
      }
      shiny::updateSelectInput(session = session, inputId = "file_name", choices = filenames)
      if (length(filenames)>1) {
        shinyjs::showElement(id = "file_name")
      } else {
        shinyjs::hideElement(id = "file_name")
      }
      shinyjs::showElement(id = "btn_load")
      shiny::updateActionButton(session = session, inputId = "btn_load", label = "Load selected<br>cell range")
      current_file_input(input$excel_file)
    })

    sheetnumber <- shiny::reactive({
      shiny::req(input$sheet_number)
      switch (
        exl_fmt(),
        "Certification" = as.numeric(input$sheet_number),
        "Homogeneity" = as.numeric(input$sheet_number),
        "Stability" = 1:length(xlsxSheetNames(input$excel_file$datapath))
      )
    })

    file_number <- shiny::reactive({
      shiny::req(input$file_name)
      which(input$excel_file$name %in% input$file_name)
    })

    # Show file preview to select rows and columns
    rv_xlsx_range_select <- m_xlsx_range_select_Server(
      id = "rng_select",
      current_file_input = current_file_input,
      sheet = sheetnumber,
      file = file_number,
      excelformat = exl_fmt
    )

    # initialize return object 'out'
    out <- shiny::reactiveValues(data = NULL, input_files = NULL)

    # when LOAD Button is clicked
    shiny::observeEvent(input$btn_load, {
      # Append File column
      message("[m_ExcelUpload] Load-button clicked")
      req(rv_xlsx_range_select$tab)
      tab_flt <- rv_xlsx_range_select$tab
      out$input_files <- current_file_input()$name
      # perform minimal validation checks
      if (exl_fmt()=="Homogeneity") {
        tab_flt <- tab_flt[[1]]
        tab_flt[["File"]]<- rep(current_file_input()$name[1], nrow(tab_flt))
        if (!"analyte" %in% colnames(tab_flt)) message("m_ExcelUpload_Server: observeEvent(input$btn_load): No column 'analyte' found in input file.")
        if (!"value" %in% colnames(tab_flt)) message("m_ExcelUpload_Server: observeEvent(input$btn_load): No column 'value' found in input file.")
        if (!is.numeric(tab_flt[,"value"])) message("m_ExcelUpload_Server: observeEvent(input$btn_load): Column 'value' in input file contains non-numeric values.")
        out$data <- tab_flt

      } else if (exl_fmt() == "Certification") {
        if (!silent) message("[m_ExcelUpload_Server] Load Certification data")
        for (i in 1:length(tab_flt)) {
          tab_flt[[i]][["File"]] = rep(current_file_input()$name[i], nrow(tab_flt[[i]]))
        }
        error_modal <- function() {
          # perform minimal validation tests
          if (!length(tab_flt)>=2) message("m_ExcelUpload_Server: observeEvent(input$btn_load): Less than 2 laboratory files uploaded. Please select more files!")
          # Try-Catch any errors during upload and open a modal window if so
          results = tryCatch({
            expr = prepTabC0(df_list = tab_flt)
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
          if(!silent) message("[m_ExcelUpload] Forgot select row and column?")
          shinyalert::shinyalert(
            title = "Forgot select row and column?",
            text = "You're trying to upload Certification data without selection of row and column. Are you sure to proceed?",
            showCancelButton = TRUE,
            showConfirmButton = TRUE,
            callbackR = function(x) { if(x != FALSE) error_modal() }
          )
        } else {
          error_modal()
        }

      } else if (exl_fmt() == "Stability") {
        # STABILITY data may come in 3 versions
        # (1) as simple two column format (Date, Value) with separate tables for each analyte
        # (2) as LTS format with a meta data header containing machine info, certification data etc.
        # (3) as a data frame giving 'Temp' info additionally to compute Arrhenius estimate of uncertainty
        test_format <- tab_flt[[1]] # openxlsx::read.xlsx(xlsxFile = input$s_input_file$datapath[1], sheet = 1)
        if (ncol(test_format)>=4) {
          if ("KW" %in% colnames(test_format)) {
            # (2) as LTS format with a meta data header containing machine infos, certification data etc.
            s_dat <- read_lts_input(
              file = input$excel_file$datapath[1],
              simplify=TRUE)
            colnames(s_dat)[colnames(s_dat)=="KW"] <- "analyte"
          } else {
            # (3) as a dataframe giving Temp info additionally to compute Arrhenius estimate of uncertainty
            s_dat <- tab_flt[[1]]
            cns <- c("analyte","Value","Date","Temp")
            if (!all(cns %in% colnames(s_dat))) {
              shinyalert::shinyalert(
                title = "Error in data upload",
                text = paste0("Required column(s) '", paste(cns[!(cns %in% colnames(s_dat))], collapse="', '"), "' not found in input file."),
                showCancelButton = FALSE,
                showConfirmButton = TRUE,
                callbackR = function(x) { s_dat <- NULL }
              )
            }
            if ("Date" %in% colnames(s_dat)) s_dat[,"time"] <- as.numeric(s_dat[,"Date"]-min(s_dat[,"Date"]))
          }
        } else {
          # (1) as simple two column format (Date, Value) with separate tables for each analyte
          sheetnames <- xlsxSheetNames(input$excel_file$datapath[1])
          s_dat <- plyr::ldply(
            sheetnumber(),
            function(x) {
              cbind(
                "analyte"= sheetnames[x],
                tab_flt[[x]]
                # "File" = current_file_input()$name
              )
            }
          )
        }
        cns <- c("analyte","Value","Date")
        shiny::validate(shiny::need(
          expr = all(cns %in% colnames(s_dat)),
          message = paste("Require column(s)", paste(cns[!(cns %in% colnames(s_dat))], collapse=", "), "in input file.")
        ))
        shiny::validate(shiny::need(is.numeric(s_dat[,"Value"]), "Column 'Value' in input file contains non-numeric values."))
        if (!inherits(s_dat[,"Date"], "Date")) { s_dat[,"Date"] <- as.Date.character(s_dat[,"Date"], tryFormats = c("%Y-%m-%d","%d.%m.%Y","%Y/%m/%d")) }
        shiny::validate(shiny::need(inherits(s_dat[,"Date"], "Date"), "Sorry, could not convert column 'Date' into correct format."))
        s_dat[,"analyte"] <- factor(s_dat[,"analyte"])
        out$data <- s_dat
      }

    }, ignoreInit = TRUE)

    shiny::observeEvent(out$data, {
      tmp <- uploaded_datasets()
      tmp <- c(tmp, exl_fmt())
      uploaded_datasets(tmp)
    })

    # Action link for help on Excel format for upload
    shiny::observeEvent(input$moduleUploadHelp, {
      switch(
        input$moduleSelect,
        "Certification" = show_help("certification_dataupload"),
        "Homogeneity" = show_help("homogeneity_dataupload"),
        "Stability" = show_help("stability_dataupload")
      )
    }, ignoreInit = TRUE)

    # when Excel was uploaded with LOAD-Button...
    shiny::observeEvent(out$data, {
      if (!silent) message("[page_start] (Excel Upload) set rv.Data; set rv.Uploadsource")
      ex_frm <- input$moduleSelect
      setValue(rv, c(ex_frm, "data"), out$data)
      setValue(rv, c(ex_frm, "input_files"), out$input_files)
      setValue(rv, c(ex_frm, "uploadsource"), value = "Excel")
      if (ex_frm=="Certification") {
        # (re)initiate apm and materialtabelle
        setValue(rv, c("General","apm"), init_apm(getValue(rv, c("Certification", "data"))))
        setValue(rv, c("General","materialtabelle"), init_materialtabelle(levels(getValue(rv, c("Certification", "data"))[,"analyte"])))
      }
    }, ignoreInit = TRUE)

    # return data as uploaded from Excel
    #return(out)

  })
}
