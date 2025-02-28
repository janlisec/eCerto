#' @description \code{m_analyte} Module for modification of analyte parameters.
#'
#' @details This module will present to the user (and allow to modify)
#'    analyte parameters which it receives from a reactive variable apm()
#'    stored in a standard R6 eCerto object as reactiveValues list.
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv eCerto R6 object, which includes available parameters for all analytes.
#'
#' @return Nothing. Will update 'apm' in eCerto R6 object should the user modify
#'    one or several input values.
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = bslib::page_fluid(
#'       shinyjs::useShinyjs(),
#'       m_analyteUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       rv <- eCerto:::test_rv("SR3")
#'       # within eCerto m_analyte gets (also initially) triggered from outside
#'       m_analyteServer(id = "test", rv = rv)
#'       shiny::observeEvent(eCerto::getValue(rv, c("General", "apm")), {
#'         print(eCerto::getValue(rv, c("General", "apm"))[[rv$cur_an]])
#'       })
#'     }
#'   )
#' }
#'
m_analyteUI <- function(id) {
  ns <- shiny::NS(id)

  # parameter panel for an analyte
  shiny::tagList(
    shinyWidgets::dropdown(
      inputId = ns("dropdown_analyt_pars"),
      label = "Options",
      width = "735px",
      circle = FALSE,
      shiny::tagList(
        bslib::layout_columns(
          shiny::div(
            style = "float: left;",
            shiny::actionLink(inputId = ns("analyte_help_link"), label = "Parameters for Analyte", style = "font-weight: 700; margin-bottom: 10px;"),
            shiny::p(id = ns("curr_analyte"), style = "color: #ffffff; background-color: #0d6efd; font-weight: 700; text-align: center; margin-bottom: 2px; padding: 2px", "select analyte"),
            shiny::checkboxInput(inputId = ns("pooling"), label = "pooling", value = FALSE, width = "90px")
          ),
          shiny::div(
            style = "float:left;",
            shiny::div("Filter IDs", style = "background: grey; text-align: center; padding: 1px; font-size: 80%; color: white;"),
            shiny::div(
              style = "float: left; width: 50%; min-width: 90px;",
              sub_header("Samples", b = 0),
              shinyWidgets::pickerInput(inputId = ns("sample_filter"), label = NULL, choices = "", multiple = TRUE, width = "100px", options = list(container = "body"))
            ),
            shiny::div(
              style = "float: left; width: 50%; min-width: 90px;",
              sub_header("Labs", b = 0),
              shinyWidgets::pickerInput(inputId = ns("lab_filter"), label = NULL, choices = "", multiple = TRUE, width = "100px", options = list(container = "body"))
            ),
          ),
          shiny::div(
            style = "float: left;",
            shiny::div("Precision (acc. to DIN1333)", style = "background: grey; text-align: center; padding: 1px; font-size: 80%; color: white;"),
            shiny::div(
              style = "float: left; width: 50%; min-width: 80px;",
              sub_header("Tables", b = 0),
              shiny::numericInput(inputId = ns("precision"), label = NULL, value = 4, min = 0, max = 10, step = 1, width = "100px")
            ),
            shiny::div(
              style = "float: left; width: 50%; min-width: 80px;",
              shiny::div(id = ns("DIN1333_info"), sub_header("Certified Values", b = 0)),
              shiny::numericInput(inputId = ns("precision_export"), label = NULL, value = 4, min = -2, max = 6, step = 1, width = "100px")
            )
          )
        )
      )
    )
  )
}

#' @noRd
#' @keywords internal
m_analyteServer <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    # set up error message system to inform user
    err_msg <- shiny::reactiveVal(NULL)
    shiny::observeEvent(err_msg(), {
      shinyWidgets::show_alert(title = "Error", text = err_msg(), type = "error")
      err_msg(NULL)
    }, ignoreNULL = TRUE)

    a <- shiny::reactive({
      #req(rv$a_p("name"))
      req(rv$e_present()["Certification"])
      # [JL] it would be nice to remove apm() from this reactive but analyte parameter update fails
      # unfortunately if apm() is not present for the case where rv$cur_an was already set by an upload
      # of S data. It is not initiated by C modul than and parameters get not updated in this case
      #apm()
      shiny::validate(shiny::need(expr = rv$cur_an %in% rv$a_p("name"), message = paste("Analyte", rv$cur_an, "is not present in C data.")))
      #if (rv$a_p("confirmed")[rv$cur_an]) rv$cur_an else NULL
      rv$cur_an
    })

    apm <- shiny::reactiveVal() # make a local copy of apm for current analyte
    shiny::observeEvent(getValue(rv, c("General", "apm"))[[a()]], {
      if (!identical(getValue(rv, c("General", "apm"))[[a()]], apm())) {
        apm(getValue(rv, c("General", "apm"))[[a()]])
      }
    }, ignoreNULL = TRUE)

    # update DIN1333 HTML
    update_DIN1333_HTML <- function() {
      shiny::isolate({
        mt <- getValue(rv, c("General", "materialtabelle"))
        n <- digits_DIN1333(x = mt[mt[, "analyte"] == a(), "U_abs"])
        n_col <- ifelse(n == apm()$precision_export, "#00FF00", "#FF0000")
        if (is.finite(n)) {
          shinyjs::html(id = "DIN1333_info", html = paste0("<strong>\u00B5<sub>c</sub> </strong><span style = 'background-color: ", n_col, "'>(", n, ")</span>"))
        }
      })
    }

    # watch out if analyte did change and
    # update inputs when different analyte is set in rv
    shiny::observeEvent(apm(), {
      a <- apm()$name
      ap <- apm()
      shinyjs::html(id = "curr_analyte", html = a)
      if (!identical(ap$pooling, input$pooling)) {
        e_msg(paste("update parameter input 'pooling' for", a))
        shiny::updateCheckboxInput(inputId = "pooling", value = ap$pooling)
      }
      # [JL] 20240418 alternatively use the list structure of the select input to group sample IDs by lab in the widget
      x <- getValue(rv, c("Certification","data"))
      f_ui <- input$sample_filter_open
      if (is.null(f_ui) || (is.logical(f_ui) && !f_ui)) {
        e_msg(paste("update parameter input 'S_Flt' for", a))
        shinyWidgets::updatePickerInput(
          inputId = "sample_filter",
          choices = split(x[x[,"analyte"]==a,"ID"], x[x[,"analyte"]==a,"Lab"]),
          selected = ap$sample_filter
        )
      }
      f_ui <- input$lab_filter_open
      if (is.null(f_ui) || (is.logical(f_ui) && !f_ui)) {
        e_msg(paste("update parameter input 'L_Flt' for", a))
        shinyWidgets::updatePickerInput(
          inputId = "lab_filter",
          choices = ap$lab_ids,
          selected = ap$lab_filter
        )
      }
      if (!identical(input$precision, ap$precision)) {
        e_msg(paste("update parameter input 'precision' for", a))
        shiny::updateNumericInput(
          inputId = "precision",
          value = ap$precision
        )
      }
      if (!identical(input$precision_export, ap$precision_export)) {
        e_msg(paste("update parameter input 'precision_export' for", a))
        shiny::updateNumericInput(
          inputId = "precision_export",
          value = ap$precision_export
        )
      }

      # check if color of suggested rounding is already correct for this analyte
      update_DIN1333_HTML()

      # whenever any analyte parameter is changed update the reactive value in the R6 object
      if (!identical(getValue(rv, c("General", "apm"))[[a]], ap)) {
        e_msg("apm changed, set rv.apm")
        tmp <- getValue(rv, c("General", "apm"))
        tmp[[a]] <- ap
        setValue(rv, c("General", "apm"), tmp)
      }

    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    shiny::observeEvent(getValue(rv, c("General", "materialtabelle")), {
      # this additional observer is required in case that the user interactively manipulates the material table
      shiny::req(apm(), a())
      update_DIN1333_HTML()
    })

    # update apm in case of changes in precision inputs
    shiny::observeEvent(input$precision, {
      shiny::req(apm())
      if (!identical(input$precision, apm()$precision)) {
        tmp <- apm()
        tmp$precision <- input$precision
        apm(tmp)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # update apm in case of changes in precision_export inputs
    shiny::observeEvent(input$precision_export, {
      shiny::req(apm())
      if (!identical(input$precision_export, apm()$precision_export)) {
        tmp <- apm()
        tmp$precision_export <- input$precision_export
        apm(tmp)
        update_DIN1333_HTML()
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # update apm in case of changes in sample_filter inputs
    shiny::observeEvent(input$sample_filter, {
      shiny::req(apm())
      if (!identical(input$sample_filter, apm()$sample_filter)) {
        tmp <- apm()
        if (length(input$sample_filter) < length(tmp$sample_ids) - 1) {
          if (is.null(input$sample_filter)) {
            tmp$sample_filter <- NULL
          } else {
            tmp$sample_filter <- input$sample_filter
          }
          apm(tmp)
        } else {
          err_msg("Sorry. Please keep at least 2 replicates for non-filtered labs.")
          shinyWidgets::updatePickerInput(inputId = "sample_filter", choices = apm()$sample_ids, selected = apm()$sample_filter)
        }
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # update apm in case of changes in lab_filter inputs
    shiny::observeEvent(input$lab_filter, {
      shiny::req(apm())
      if (!identical(input$lab_filter, apm()$lab_filter)) {
        tmp <- apm()
        if (length(input$lab_filter) < length(tmp$lab_ids) - 1) {
          if (is.null(input$lab_filter)) {
            tmp$lab_filter <- NULL
          } else {
            tmp$lab_filter <- input$lab_filter
          }
          apm(tmp)
        } else {
          err_msg("Sorry. Please keep at least 2 labs as statistical tests will fail otherwise.")
          shinyWidgets::updatePickerInput(inputId = "lab_filter", choices = apm()$lab_ids, selected = apm()$lab_filter)
        }
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # update apm in case of changes in pooling
    shiny::observeEvent(input$pooling, {
      shiny::req(apm())
      if (!identical(input$pooling, apm()$pooling)) {
        tmp <- apm()
        tmp$pooling <- input$pooling
        apm(tmp)
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # help modals
    shiny::observeEvent(input$analyte_help_link, {
      show_help("certification_analyte_options")
    })
  })
}
