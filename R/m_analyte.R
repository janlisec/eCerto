#'@description \code{m_analyte} Module for modification of analyte parameters.
#'
#'@details This module will present to the user (and allow to modify)
#'    analyte parameters which it receives from a reactive variable apm()
#'    stored in a standard R6 eCerto object as reactiveValues list.
#'
#'@param id Name when called as a module in a shiny app.
#'@param rv eCerto R6 object, which includes available parameters for all analytes.
#'
#'@return Nothing. Will update 'apm' in eCerto R6 object should the user modify
#'    one or several input values.
#'
#'@noRd
#'@keywords internal
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    shinyjs::useShinyjs(),
#'    m_analyteUI(id = "test")),
#'  server = function(input, output, session) {
#'    rv <- eCerto:::test_rv()
#'    # within eCerto m_analyte gets (also initially) triggered from outside
#'    gargoyle::init("update_c_analyte")
#'    reactive({gargoyle::trigger("update_c_analyte")})
#'    m_analyteServer(id = "test", rv = rv)
#'    shiny::observeEvent(eCerto::getValue(rv, c("General","apm")), {
#'      print(eCerto::getValue(rv, c("General","apm"))[[rv$c_analyte]])
#'    })
#'  }
#' )
#' }
#'
m_analyteUI = function(id) {
  ns <- shiny::NS(id)

  # parameter panel for an analyte
  shiny::tagList(
    shiny::div(style = "width: 200px; float:left; margin-right:5px; margin-left:35px;",
      shiny::actionLink(inputId = ns("analyte_help_link"), label = "Parameters for Analyte", style = "font-weight: 700; margin-bottom: 10px;"),
      shiny::p(id = ns("curr_analyte"), style = "background: red; text-align: center; margin-bottom: 0px", "test-text"),
      shiny::checkboxInput(inputId = ns("pooling"), label = "pooling", value = FALSE)
    ),
    shiny::div(style="width: 200px; float:left; margin-right:5px; margin-left:15px;",
      shiny::div("Filter IDs", style = "background: grey; text-align: center"),
      shiny::div(
        style="float: left; width: 50%; min-width: 80px; margin-bottom: 0px;",
        sub_header("Samples", b=0),
        shiny::selectizeInput(inputId = ns("sample_filter"), label = NULL, choices = "", multiple = TRUE)
      ),
      shiny::div(
        style="float: left; width: 50%; min-width: 80px; margin-bottom: 0px;",
        sub_header("Labs", b=0),
        shiny::selectizeInput(inputId = ns("lab_filter"), label = NULL, choices = "", multiple = TRUE)
      ),
    ),
    shiny::div(style="width: 200px; float:left; margin-right:5px; margin-left:15px;",
      shiny::div("Precision (acc. to DIN1333)", style = "background: grey; text-align: center"),
      shiny::div(
        style="float: left; width: 50%; min-width: 80px; margin-bottom: 0px;",
        sub_header("Tables", b=0),
        shiny::numericInput(inputId = ns("precision"), label = NULL, value = 4, min = 0, max = 10, step = 1)),
      shiny::div(
        style="float: left; width: 50%; min-width: 80px; margin-bottom: 0px;",
        shiny::div(id = ns("DIN1333_info"), sub_header("Certified Values", b=0)),
        shiny::numericInput(inputId = ns("precision_export"), label = NULL, value = 4, min = -2, max = 6, step = 1)
      ),
    )
  )
}

#' @noRd
#' @keywords internal
m_analyteServer = function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    apm <- shiny::reactiveVal() # make a local copy of apm
    shiny::observeEvent(getValue(rv, c("General","apm")), {
      # $$ToDo$$ attach the unit information to apm if not yet present
      if (!identical(getValue(rv, c("General","apm")), apm())) {
        apm(getValue(rv, c("General","apm")))
        gargoyle::trigger("update_c_analyte")
      }
    }, ignoreNULL = TRUE)

    # set up error message system to inform user
    err_msg <- shiny::reactiveVal(NULL)
    shiny::observeEvent(err_msg(), {
      shinyalert::shinyalert(
        title = "Error",
        text = err_msg()
      )
      err_msg(NULL)
    }, ignoreNULL = TRUE)

    # watch out if analyte did change and
    # update inputs when different analyte is set in rv
    shiny::observeEvent(gargoyle::watch("update_c_analyte"), {
      shiny::req(apm())
      message("[m_analyte] update parameter inputs for ", rv$c_analyte)
      shinyjs::html(id = "curr_analyte", html = apm()[[rv$c_analyte]]$name)
      shiny::updateCheckboxInput(
        inputId = "pooling",
        value = apm()[[rv$c_analyte]]$pooling
      )
      shiny::updateSelectizeInput(
        inputId = "sample_filter",
        choices = apm()[[rv$c_analyte]]$sample_ids,
        selected = apm()[[rv$c_analyte]]$sample_filter
      )
      shiny::updateSelectizeInput(
        inputId = "lab_filter",
        choices = apm()[[rv$c_analyte]]$lab_ids,
        selected = apm()[[rv$c_analyte]]$lab_filter
      )
      shiny::updateNumericInput(
        inputId = "precision",
        value = apm()[[rv$c_analyte]]$precision
      )
      shiny::updateNumericInput(
        inputId = "precision_export",
        value = apm()[[rv$c_analyte]]$precision_export
      )
      mt <- getValue(rv, c("General", "materialtabelle"))
      n <- digits_DIN1333(x = mt[mt[,"analyte"]==rv$c_analyte,"U_abs"])
      if (is.finite(n)) { shinyjs::html(id = "DIN1333_info", html = paste0("<strong>Cert. Val. </strong>(", n, ")")) }
    }, ignoreInit = FALSE)

    shiny::observeEvent(getValue(rv, c("General", "materialtabelle")), {
      # this additional observer is required in case that the user interactively manipulates the material table
      shiny::req(apm())
      mt <- getValue(rv, c("General", "materialtabelle"))
      n <- digits_DIN1333(x = mt[mt[,"analyte"]==rv$c_analyte,"U_abs"])
      #shiny::updateNumericInput(inputId = "precision_export", value = apm()[[rv$c_analyte]]$precision_export)
      if (is.finite(n)) { shinyjs::html(id = "DIN1333_info", html = paste0("<strong>Cert. Val. </strong>(", n, ")")) }
    })

    # update apm in case of changes in precision inputs
    shiny::observeEvent(input$precision, {
      shiny::req(apm(), rv$c_analyte)
      tmp <- apm()
      if (!identical(input$precision, tmp[[rv$c_analyte]]$precision)) {
        message("[m_analyte] update 'precision'")
        tmp[[rv$c_analyte]]$precision <- input$precision
        apm(tmp)
      }
    }, ignoreNULL = FALSE, ignoreInit=TRUE)

    # update apm in case of changes in precision_export inputs
    shiny::observeEvent(input$precision_export, {
      shiny::req(apm(), rv$c_analyte)
      tmp <- apm()
      if (!identical(input$precision_export, tmp[[rv$c_analyte]]$precision_export)) {
        message("[m_analyte] update 'precision_export'")
        tmp[[rv$c_analyte]]$precision_export <- input$precision_export
        apm(tmp)
      }
    }, ignoreNULL = FALSE, ignoreInit=TRUE)

    # update apm in case of changes in sample_filter inputs
    shiny::observeEvent(input$sample_filter, {
      shiny::req(apm(), rv$c_analyte)
      tmp <- apm()
      if (!identical(input$sample_filter, tmp[[rv$c_analyte]]$sample_filter)) {
        message("[m_analyte] update 'sample_filter'")
        if (length(input$sample_filter) < length(tmp[[rv$c_analyte]]$sample_ids)-1) {
          if (is.null(input$sample_filter)) {
            tmp[[rv$c_analyte]]["sample_filter"] <- list(NULL)
          } else {
            tmp[[rv$c_analyte]]$sample_filter <- input$sample_filter
          }
          apm(tmp)
        } else {
          err_msg("Sorry. Please keep at least 2 replicates for non-filtered labs.")
          shiny::updateSelectizeInput(
            inputId = "sample_filter",
            choices = apm()[[rv$c_analyte]]$sample_ids,
            selected = apm()[[rv$c_analyte]]$sample_filter
          )
        }
      }
    }, ignoreNULL = FALSE, ignoreInit=TRUE)

    # update apm in case of changes in lab_filter inputs
    shiny::observeEvent(input$lab_filter, {
      shiny::req(apm(), rv$c_analyte)
      tmp <- apm()
      if (!identical(input$lab_filter, tmp[[rv$c_analyte]]$lab_filter)) {
        message("[m_analyte] update 'lab_filter'")
        if (length(input$lab_filter) < length(tmp[[rv$c_analyte]]$lab_ids)-1) {
          if (is.null(input$lab_filter)) {
            tmp[[rv$c_analyte]]["lab_filter"] <- list(NULL)
          } else {
            tmp[[rv$c_analyte]]$lab_filter <- input$lab_filter
          }
          apm(tmp)
        } else {
          err_msg("Sorry. Please keep at least 2 labs as statistical tests will fail otherwise.")
          shiny::updateSelectizeInput(
            inputId = "lab_filter",
            choices = apm()[[rv$c_analyte]]$lab_ids,
            selected = apm()[[rv$c_analyte]]$lab_filter
          )
        }
      }
    }, ignoreNULL = FALSE, ignoreInit=TRUE)

    # update apm in case of changes in pooling
    shiny::observeEvent(input$pooling, {
      shiny::req(apm(), rv$c_analyte)
      tmp <- apm()
      if (!identical(input$pooling, tmp[[rv$c_analyte]]$pooling)) {
        message("[m_analyte] update 'pooling'")
        tmp[[rv$c_analyte]]$pooling <- input$pooling
        apm(tmp)
      }
    }, ignoreNULL = FALSE, ignoreInit=TRUE)

    # whenever any analyte parameter is changed update the reactive value in the R6 object
    shiny::observeEvent(apm(), {
      if (!identical(getValue(rv, c("General","apm")), apm())) {
        message("[m_analyte] apm changed, set rv.apm")
        setValue(rv, c("General","apm"), apm())
      }
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$analyte_help_link,{
      show_help("certification_analyte_options")
    })

  })
}
