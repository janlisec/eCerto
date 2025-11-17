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
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyjs::useShinyjs(),
#'       eCerto:::m_ExcelUpload_UI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       rv <- eCerto::eCerto$new()
#'       eCerto:::m_ExcelUpload_Server(id = "test", rv = rv)
#'       shiny::observeEvent(rv$e_present(), {
#'         print(rv$e_present())
#'       })
#'     }
#'   )
#' }
#'
#' @noRd
#' @keywords internal
m_ExcelUpload_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # show welcome screen if no range select is needed
    div(id = ns("welcome_screen"), welcome_screen(id = id)),
    # show upload options
    bslib::card(
        #style = "background-color: #f5f5f5; border: 1px; border-radius: 4px; border-color: #e3e3e3; border-style: solid; margin: 0px; padding-top: 6px",
        style = "background-color: var(--_sidebar-bg); min-height: 148px; max-height: 148px;",
        shiny::div(
          shiny::div(
            style = "width: 130px; float: left; margin-bottom: -12px; margin-right: var(--bs-card-spacer-y);",
            shiny::radioButtons(
              inputId = ns("moduleSelect"),
              label = "File format",
              choices = "dummy"
            )
          ),
          shiny::div(
            style = "width: 300px; float: left; margin-right: var(--bs-card-spacer-y);",
            shiny::uiOutput(outputId = ns("inp_file"))
          ),
          shiny::div(
            style = "width: 280px; float: left; margin-right: var(--bs-card-spacer-y);",
            shinyjs::hidden(shinyWidgets::pickerInput(inputId = ns("file_name"), label = "File", choices = "", options = list(container = "body")))
          ),
          shiny::div(
            style = "width: 90px; float: left; margin-right: var(--bs-card-spacer-y);",
            shinyjs::hidden(shinyWidgets::pickerInput(inputId = ns("sheet_number"), label = "Sheet #", choices = "1", options = list(container = "body")))
          ),
          shiny::div(
            style = "width: 160px; float: left; margin-right: var(--bs-card-spacer-y); margin-top: 31px;",
            shinyjs::hidden(shiny::actionButton(inputId = ns("btn_load"), label = "Load selected cell range", style = "background-color: rgb(140,180,15)"))
          ),
          shiny::div(
            style = "width: 420px; float: left; color: red; background: rgba(0,0,0,0.04); border: 4px; padding: 16px;",
            id = ns("info_msg")
          ),
        )
    ),
    # preview Excel table
    m_xlsx_range_select_UI(ns("rng_select")),
  )
}

#' @noRd
#' @keywords internal
m_ExcelUpload_Server <- function(id, rv = NULL, msession = NULL) {
  ns <- shiny::NS(id)

  shiny::moduleServer(id, function(input, output, session) {
    # Certification, Homogeneity, Stability -----------------------------------
    shiny::updateRadioButtons(session = session, inputId = "moduleSelect", choices = getValue(rv, "modules"))

    # rename input into a reactive
    exl_fmt <- shiny::reactive({
      input$moduleSelect
    })

    shiny::observeEvent(check(), {
      if (check()) {
        shinyjs::html(id = "info_msg", html = shiny::HTML("Note! You have uploaded <strong>", exl_fmt(), "</strong> data already. If you upload a different file, all your selected parameters may be lost."))
        shinyjs::show(id = "info_msg")
      } else {
        shinyjs::html(id = "info_msg", html = "")
        shinyjs::hide(id = "info_msg")
      }
    })


    # monitor the status of the file selector
    current_file_input <- shiny::reactiveVal(NULL)

    # Excel-File-Input wrapped in renderUI to allow triggering some JS and empty the fileInput widget
    output$inp_file <- shiny::renderUI({
      shiny::req(exl_fmt())
      e_msg("render upload UI (m_ExcelUpload_Server)")
      current_file_input(NULL)
      rv$e_present()
      shinyjs::hideElement(id = "sheet_number")
      shinyjs::hideElement(id = "file_name")
      shinyjs::hideElement(id = "btn_load")
      shiny::tagList(
        shiny::fileInput(
          inputId = session$ns("excel_file"),
          multiple = exl_fmt() == "Certification",
          label = shiny::tagList("Select ", shiny::actionLink(inputId = session$ns("moduleUploadHelp"), label = paste(exl_fmt(), "data")), "Excel", ifelse(exl_fmt() == "Certification", "Files", "File")),
          accept = "xlsx"
        )
      )
    })

    shiny::observe({
      # hide welcome screen when some data was loaded already
      shinyjs::toggleElement(id = "welcome_screen", condition = !any(rv$e_present()) & is.null(current_file_input()))
    })

    # Excel Sheet-number selector
    shiny::observeEvent(input$excel_file, {
      sheetnames <- xlsxSheetNames(input$excel_file$datapath)
      filenames <- input$excel_file$name
      if (length(sheetnames) > 1) {
        shinyWidgets::updatePickerInput(session = session, inputId = "sheet_number", choices = 1:length(sheetnames))
        shinyjs::showElement(id = "sheet_number")
      } else {
        shiny::updateSelectInput(session = session, inputId = "sheet_number", choices = "1")
        shinyjs::hideElement(id = "sheet_number")
      }
      shinyWidgets::updatePickerInput(session = session, inputId = "file_name", choices = filenames)
      if (length(filenames) > 1) {
        shinyjs::showElement(id = "file_name")
      } else {
        shinyjs::hideElement(id = "file_name")
      }
      shinyjs::showElement(id = "btn_load")
      shiny::updateActionButton(session = session, inputId = "btn_load", label = "Load selected<br>cell range")
      current_file_input(input$excel_file)
    })

    file_number <- shiny::reactive({
      shiny::req(input$file_name)
      which(input$excel_file$name %in% input$file_name)
    })

    check <- shiny::reactive({
      # req(any(rv$e_present()), exl_fmt() %in% names(rv$e_present()))
      # rv$e_present()[exl_fmt()]
      if (any(rv$e_present()) && exl_fmt() %in% names(rv$e_present())) {
        rv$e_present()[exl_fmt()]
      } else {
        FALSE
      }
    })

    # Show file preview to select rows and columns
    rv_xlsx_range_select <- m_xlsx_range_select_Server(
      id = "rng_select",
      current_file_input = current_file_input,
      sheet = shiny::reactive({ as.numeric(input$sheet_number) }),
      file = file_number,
      excelformat = exl_fmt
    )

    # initialize return object 'out'
    out <- shiny::reactiveValues(data = NULL, input_files = NULL)

    # load from Excel
    load_from_excel <- function(fn = current_file_input()$name, fmt = c("Stability", "Homogeneity", "Certification")) {
      fmt <- match.arg(fmt)
      load_result <- NULL
      tab_flt <- rv_xlsx_range_select$tab
      # Append File column
      out$input_files <- fn
      # perform minimal validation checks
      if (fmt == "Homogeneity") {
        x <- tab_flt[[1]]
        x <- checkHdata(x)
        x[, "File"] <- rep(fn[1], nrow(x))
        load_result <- x
      } else if (fmt == "Certification") {
        e_msg("Load Certification data (m_ExcelUpload_Server)")
        # append file info
        for (i in 1:length(tab_flt)) {
          tab_flt[[i]][["File"]] <- rep(fn[i], nrow(tab_flt[[i]]))
        }
        # try to convert to data frame
        tabC0 <- tryCatch(
          expr = {
            prepTabC0(df_list = tab_flt)
          },
          error = function(e) {
            out <- tab_flt[[i]]
            attr(out, "msg") <- e
            return(out)
          }
        )
        # in case (a) it is Certification module and (b) the input table has not been filtered, then ask if this is correct
        test_selection <- ncol(rv_xlsx_range_select$tab_upload[[1]]) == ncol(rv_xlsx_range_select$tab[[1]]) & nrow(rv_xlsx_range_select$tab_upload[[1]]) == nrow(rv_xlsx_range_select$tab[[1]])
        if (test_selection) {
          e_msg("Range specification is on default value")
          #attr(tabC0, "msg") <- "Range specification is on default value"
        }
        load_result <- tabC0
      } else if (fmt == "Stability") {
        # STABILITY data may come in 3 versions
        # (1) as simple two column format (Date, Value) with separate tables for each analyte
        # (2) as LTS format with a meta data header containing machine info, certification data etc.
        # (3) as a data frame giving 'Temp' info additionally to compute Arrhenius estimate of uncertainty
        test_format <- tab_flt[[as.numeric(input$sheet_number)]] # openxlsx::read.xlsx(xlsxFile = input$s_input_file$datapath[1], sheet = 1)
        if (ncol(test_format) < 4) {
          # (1) as simple two column format (Date, Value) with separate tables for each analyte
          sheetnames <- xlsxSheetNames(input$excel_file$datapath[1])
          s_dat <- plyr::ldply(1:length(sheetnames), function(x) {
            cbind("analyte" = sheetnames[x], tab_flt[[x]])
          })
        } else {
          if ("KW" %in% colnames(test_format)) {
            # (2) as LTS format with a meta data header containing machine infos, certification data etc.
            s_dat <- read_lts_input(file = input$excel_file$datapath[1], simplify = TRUE)
            colnames(s_dat)[colnames(s_dat) == "KW"] <- "analyte"
          } else {
            # (3) as a dataframe giving Temp info additionally to compute Arrhenius estimate of uncertainty
            s_dat <- tab_flt[[as.numeric(input$sheet_number)]]
            s_dat <- assert_col(df = s_dat, name = "Temp", type = "numeric")
          }
        }
        s_dat <- assert_col(df = s_dat, name = "analyte", type = "factor")
        s_dat <- assert_col(df = s_dat, name = "Value", type = "numeric")
        s_dat <- assert_col(df = s_dat, name = "Date", type = "Date")
        s_dat[, "time"] <- as.numeric(s_dat[, "Date"] - min(s_dat[, "Date"]))
        load_result <- s_dat
      }
      return(load_result)
    }

    # when LOAD Button is clicked
    shiny::observeEvent(input$btn_load,
      {
        req(rv_xlsx_range_select$tab)
        e_msg("Load-button clicked (m_ExcelUpload_Server)")
        tmp <- try(load_from_excel(fn = current_file_input()$name, fmt = exl_fmt()))
        if (inherits(tmp, "try-error") | !is.null(attr(tmp, "msg")) | is.null(tmp)) {
          shinyWidgets::ask_confirmation(
            inputId = "ignore_problems", btn_labels = c("Cancel upload", "Upload anyways"),
            title = "Problems detected", type = "error", html = TRUE,
            text = shiny::tagList(
              shiny::div(
                style = "text-align: left;",
                shiny::HTML("<b>These messages were returned:</b><br>"),
                shiny::div(style = "font-size: 12px;", tags$div(tags$ul(lapply(attr(tmp, "msg"), tags$li)))),
                shiny::HTML("<b>This would be the structure of the upload:</b><br>"),
                shiny::div(style = "font-size: 12px;", shiny::HTML(paste(utils::capture.output(utils::str(tmp)), collapse = "<br>")))
              )
            )
          )
        } else {
          out$data <- tmp
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(input$ignore_problems, {
      if (input$ignore_problems) {
        tmp <- try(load_from_excel(fn = current_file_input()$name, fmt = exl_fmt()))
        out$data <- tmp
      }
    })

    # when Excel was uploaded with LOAD-Button...
    shiny::observeEvent(out$data,
      {
        e_msg("set rv.Data (m_ExcelUpload_Server)")
        setValue(rv, c(exl_fmt(), "data"), out$data)
        setValue(rv, c(exl_fmt(), "input_files"), out$input_files)
        if (exl_fmt() == "Certification") {
          # (re)initiate apm and materialtabelle
          setValue(rv, c("General", "apm"), init_apm(getValue(rv, c("Certification", "data"))))
          setValue(rv, c("General", "materialtabelle"), init_materialtabelle(levels(getValue(rv, c("Certification", "data"))[, "analyte"])))
        }
      },
      ignoreInit = TRUE
    )

    # Help section -------------------------------------------------------------
    shiny::observeEvent(input$getHelp, {
      show_help("start_gethelp")
    })
    shiny::observeEvent(input$showHelp, {
      shiny::updateNavbarPage(session = msession, inputId = "navbarpage", selected = "tP_help")
    })
    # Action link for help on Excel format for upload
    shiny::observeEvent(input$moduleUploadHelp,
      {
        switch(exl_fmt(),
          "Certification" = show_help("certification_dataupload"),
          "Homogeneity" = show_help("homogeneity_dataupload"),
          "Stability" = show_help("stability_dataupload")
        )
      },
      ignoreInit = TRUE
    )
  })
}
