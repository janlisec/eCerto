#' @description \code{m_analyte} Module for modification of analyte paramters.
#'
#' @details Note! This module will modify a reactive variable apm() provided to the module as a parameter
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv reactiveValues object, which gives available parameters for each analyte.
#' @param selected_tab The currently selected analyte (as valid name from apm). Reactive.
#' @param allow_selection Set to TRUE for testing purposes only.
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    shinyjs::useShinyjs(),
#'    m_analyteUI(id = "test")),
#'  server = function(input, output, session) {
#'    rv <- eCerto:::test_rv()
#'    m_analyteServer(
#'      id = "test",
#'      rv = rv,
#'      selected_tab = shiny::reactiveVal("Si"),
#'      allow_selection = TRUE
#'    )
#'    shiny::observeEvent(eCerto::getValue(rv, c("General","apm")), {
#'      print(eCerto::getValue(rv, c("General","apm")))
#'    })
#'  }
#' )
#' }
#'
m_analyteUI = function(id) {
  ns <- shiny::NS(id)

  # empty tabset panel, to be filled by the analytes in the server Module
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::selectInput(
          inputId = ns("Name"),
          label = "Analyt Name",
          choices = ""
        )
      ),
      shiny::column(
        width = 3,
        shiny::selectizeInput(
          inputId = ns("sample_filter"),
          label = "Filter Sample IDs",
          choices = "",
          multiple = TRUE
        )
      ),
      shiny::column(
        width = 3,
        shiny::numericInput(
          inputId =ns("precision"),
          label = "Precision (Input)",
          value =  4, min = 0, max = 10, step = 1
        )
      ),
      shiny::column(
        width = 3,
        shiny::helpText("")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::checkboxInput(
          inputId = ns("pooling"),
          label = "pooling",
          value = FALSE
        ),
      ),
      shiny::column(
        width = 3,
        shiny::selectizeInput(
          inputId = ns("lab_filter"),
          label = "Filter Lab IDs",
          choices = "",
          multiple = TRUE
        )
      ),
      shiny::column(
        width = 3,
        shiny::numericInput(
          inputId =ns("precision_export"),
          label = "Precision (Export)",
          value = 4, min = 0, max = 10, step = 1
        )
      ),
      shiny::column(
        width = 3,
        shiny::helpText("")
      )
    )
  )
}

#' @noRd
#' @keywords internal
m_analyteServer = function(id, rv, selected_tab, allow_selection=FALSE) {

  stopifnot(shiny::is.reactive(selected_tab))

  shiny::moduleServer(id, function(input, output, session) {

    apm <- shiny::reactiveVal() # make a local copy of apm
    shiny::observeEvent(getValue(rv, c("General","apm")), {
      # attach the unit information if not yet present
      #browser()
      if (!identical(getValue(rv, c("General","apm")), apm())) {
        apm(getValue(rv, c("General","apm")))
      }
    }, ignoreNULL = TRUE)

    if (!allow_selection) {
      shiny::updateSelectInput(
        inputId = "Name",
        label = "Row selected in Tab.3",
      )
      shinyjs::disable(id = "Name")
    }
    err_msg <- shiny::reactiveVal(NULL)
    shiny::observeEvent(err_msg(), {
      shinyalert::shinyalert(
        title = "Error",
        text = err_msg()
      )
      err_msg(NULL)
    }, ignoreNULL = TRUE)

    # update inputs when different analyte is selected
    shiny::observe({
      shiny::req(apm(), selected_tab())
      message("[m_analyte] update parameter inputs for ", selected_tab())
      shiny::updateSelectInput(
        inputId = "Name",
        choices = names(apm()),
        selected = apm()[[selected_tab()]]$name
      )
      shiny::updateCheckboxInput(
        inputId = "pooling",
        value = apm()[[selected_tab()]]$pooling
      )
      shiny::updateSelectizeInput(
        inputId = "sample_filter",
        choices = apm()[[selected_tab()]]$sample_ids,
        selected = apm()[[selected_tab()]]$sample_filter
      )
      shiny::updateSelectizeInput(
        inputId = "lab_filter",
        choices = apm()[[selected_tab()]]$lab_ids,
        selected = apm()[[selected_tab()]]$lab_filter
      )
      shiny::updateNumericInput(
        inputId = "precision",
        value = apm()[[selected_tab()]]$precision
      )
      shiny::updateNumericInput(
        inputId = "precision_export",
        value = apm()[[selected_tab()]]$precision_export
      )
    })

    # update apm in case of changes in precision inputs
    shiny::observeEvent(input$precision, {
      shiny::req(apm(), selected_tab())
      tmp <- apm()
      if (!identical(input$precision, tmp[[selected_tab()]]$precision)) {
        message("[m_analyte] update 'precision'")
        tmp[[selected_tab()]]$precision <- input$precision
        apm(tmp)
      }
    })

    # update apm in case of changes in precision_export inputs
    shiny::observeEvent(input$precision_export, {
      shiny::req(apm(), selected_tab())
      tmp <- apm()
      if (!identical(input$precision_export, tmp[[selected_tab()]]$precision_export)) {
        message("[m_analyte] update 'precision_export'")
        tmp[[selected_tab()]]$precision_export <- input$precision_export
        apm(tmp)
      }
    })

    # update apm in case of changes in sample_filter inputs
    shiny::observeEvent(input$sample_filter, {
      shiny::req(apm(), selected_tab())
      tmp <- apm()
      if (!identical(input$sample_filter, tmp[[selected_tab()]]$sample_filter)) {
        message("[m_analyte] update 'sample_filter'")
        if (length(input$sample_filter) < length(tmp[[selected_tab()]]$sample_ids)-1) {
          if (is.null(input$sample_filter)) {
            tmp[[selected_tab()]]["sample_filter"] <- list(NULL)
          } else {
            tmp[[selected_tab()]]$sample_filter <- input$sample_filter
          }
          apm(tmp)
        } else {
          err_msg("Sorry. Please keep at least 2 replicates for non-filtered labs.")
          shiny::updateSelectizeInput(
            inputId = "sample_filter",
            choices = apm()[[selected_tab()]]$sample_ids,
            selected = apm()[[selected_tab()]]$sample_filter
          )
        }
      }
    }, ignoreNULL = FALSE, ignoreInit=TRUE)

    # update apm in case of changes in lab_filter inputs
    shiny::observeEvent(input$lab_filter, {
      shiny::req(apm(), selected_tab())
      tmp <- apm()
      if (!identical(input$lab_filter, tmp[[selected_tab()]]$lab_filter)) {
        message("[m_analyte] update 'lab_filter'")
        if (length(input$lab_filter) < length(tmp[[selected_tab()]]$lab_ids)-1) {
          if (is.null(input$lab_filter)) {
            tmp[[selected_tab()]]["lab_filter"] <- list(NULL)
          } else {
            tmp[[selected_tab()]]$lab_filter <- input$lab_filter
          }
          apm(tmp)
        } else {
          err_msg("Sorry. Please keep at least 2 labs as statistical tests will fail otherwise.")
          shiny::updateSelectizeInput(
            inputId = "lab_filter",
            choices = apm()[[selected_tab()]]$lab_ids,
            selected = apm()[[selected_tab()]]$lab_filter
          )
        }
      }
    }, ignoreNULL = FALSE, ignoreInit=TRUE)

    # update apm in case of changes in module inputs
    shiny::observeEvent(input$pooling, {
      shiny::req(apm(), selected_tab())
      tmp <- apm()
      if (!identical(input$pooling, tmp[[selected_tab()]]$pooling)) {
        message("[m_analyte] update 'pooling'")
        tmp[[selected_tab()]]$pooling <- input$pooling
        apm(tmp)
      }
    })

    # update apm in case of changes in module inputs
    shiny::observeEvent(input$Name, {
      shiny::req(apm())
      selected_tab(input$Name)
    })

    # whenever the analyte parameter like lab filter, sample filter etc are changed
    shiny::observeEvent(apm(), {
      if (!identical(getValue(rv, c("General","apm")), apm())) {
        message("[m_analyte] apm changed, set rv.apm")
        setValue(rv, c("General","apm"), apm())
      }
    }, ignoreNULL = TRUE)

  })
}
