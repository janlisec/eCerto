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
#'     ui = shiny::fluidPage(
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
    shiny::div(
      style = "width: 200px; float:left; margin-right:5px; margin-left:35px;",
      shiny::actionLink(inputId = ns("analyte_help_link"), label = "Parameters for Analyte", style = "font-weight: 700; margin-bottom: 10px;"),
      shiny::p(id = ns("curr_analyte"), style = "background-color: rgb(0,175,240); font-weight: 700; text-align: center; margin-bottom: 0px; padding-top: 3px", "select analyte"),
      shiny::checkboxInput(inputId = ns("pooling"), label = "pooling", value = FALSE)
    ),
    shiny::div(
      style = "width: 200px; float:left; margin-right:5px; margin-left:15px;",
      shiny::div("Filter IDs", style = "background: grey; text-align: center; padding-top: 2px"),
      shiny::div(
        style = "float: left; width: 50%; min-width: 80px; margin-bottom: 0px;",
        sub_header("Samples", b = 0),
        shiny::selectizeInput(inputId = ns("sample_filter"), label = NULL, choices = "", multiple = TRUE)
      ),
      shiny::div(
        style = "float: left; width: 50%; min-width: 80px; margin-bottom: 0px;",
        sub_header("Labs", b = 0),
        shiny::selectizeInput(inputId = ns("lab_filter"), label = NULL, choices = "", multiple = TRUE)
      ),
    ),
    shiny::div(
      style = "width: 200px; float:left; margin-right:5px; margin-left:15px;",
      shiny::div("Precision (acc. to DIN1333)", style = "background: grey; text-align: center; padding-top: 2px"),
      shiny::div(
        style = "float: left; width: 50%; min-width: 80px; margin-bottom: 0px;",
        sub_header("Tables", b = 0),
        shiny::numericInput(inputId = ns("precision"), label = NULL, value = 4, min = 0, max = 10, step = 1)
      ),
      shiny::div(
        style = "float: left; width: 50%; min-width: 80px; margin-bottom: 0px;",
        shiny::div(id = ns("DIN1333_info"), sub_header("Certified Values", b = 0)),
        shiny::numericInput(inputId = ns("precision_export"), label = NULL, value = 4, min = -2, max = 6, step = 1)
      ),
    )
  )
}

#' @noRd
#' @keywords internal
m_analyteServer <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    apm <- shiny::reactiveVal() # make a local copy of apm
    shiny::observeEvent(getValue(rv, c("General", "apm")),
      {
        if (!identical(getValue(rv, c("General", "apm")), apm())) {
          apm(getValue(rv, c("General", "apm")))
        }
      },
      ignoreNULL = TRUE
    )

    # set up error message system to inform user
    err_msg <- shiny::reactiveVal(NULL)
    shiny::observeEvent(err_msg(),
      {
        shinyWidgets::show_alert(title = "Error", text = err_msg(), type = "error")
        err_msg(NULL)
      },
      ignoreNULL = TRUE
    )

    a <- shiny::reactive({
      req(rv$a_p("name"))
      req(rv$e_present()["Certification"])
      # [JL] it would be nice to remove apm() from this reactive but analyte parameter update fails
      # unfortunately if apm() is not present for the case where rv$cur_an was already set by an upload
      # of S data. It is not initated by C modul than and parameters get not updated in this case
      apm()
      shiny::validate(shiny::need(expr = rv$cur_an %in% rv$a_p("name"), message = paste("Analyte", rv$cur_an, "is not present in C data.")))
      if (rv$a_p("confirmed")[rv$cur_an]) rv$cur_an else NULL
    })

    # watch out if analyte did change and
    # update inputs when different analyte is set in rv
    shiny::observeEvent(a(),
      {
        shiny::req(apm())
        a <- a()
        if (a %in% rv$a_p("name")) {
          message("[m_analyte] update parameter inputs for ", a)
          shinyjs::html(id = "curr_analyte", html = apm()[[a]]$name)
          shiny::updateCheckboxInput(
            inputId = "pooling",
            value = apm()[[a]]$pooling
          )
          shiny::updateSelectizeInput(
            inputId = "sample_filter",
            choices = apm()[[a]]$sample_ids,
            selected = apm()[[a]]$sample_filter
          )
          shiny::updateSelectizeInput(
            inputId = "lab_filter",
            choices = apm()[[a]]$lab_ids,
            selected = apm()[[a]]$lab_filter
          )
          shiny::updateNumericInput(
            inputId = "precision",
            value = apm()[[a]]$precision
          )
          shiny::updateNumericInput(
            inputId = "precision_export",
            value = apm()[[a]]$precision_export
          )
          # check if color of suggested rounding is already correct for this analyte
          update_DIN1333_HTML()
        } else {
          message("[m_analyte] Can't update parameter inputs for ", a)
        }
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    # update DIN1333 HTML
    update_DIN1333_HTML <- function() {
      shiny::isolate({
        mt <- getValue(rv, c("General", "materialtabelle"))
        n <- digits_DIN1333(x = mt[mt[, "analyte"] == a(), "U_abs"])
        n_col <- ifelse(n == apm()[[a()]]$precision_export, "#00FF00", "#FF0000")
        if (is.finite(n)) {
          shinyjs::html(id = "DIN1333_info", html = paste0("<strong>Cert. Val. </strong><span style = 'background-color: ", n_col, "'>(", n, ")</span>"))
        }
      })
    }

    shiny::observeEvent(getValue(rv, c("General", "materialtabelle")), {
      # this additional observer is required in case that the user interactively manipulates the material table
      shiny::req(apm(), a())
      update_DIN1333_HTML()
    })

    # update apm in case of changes in precision inputs
    shiny::observeEvent(input$precision,
      {
        shiny::req(apm(), a())
        tmp <- apm()
        if (!identical(input$precision, tmp[[a()]]$precision)) {
          message("[m_analyte] update 'precision'")
          tmp[[a()]]$precision <- input$precision
          apm(tmp)
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # update apm in case of changes in precision_export inputs
    shiny::observeEvent(input$precision_export,
      {
        shiny::req(apm(), a())
        tmp <- apm()
        if (!identical(input$precision_export, tmp[[a()]]$precision_export)) {
          message("[m_analyte] update 'precision_export'")
          tmp[[a()]]$precision_export <- input$precision_export
          apm(tmp)
          update_DIN1333_HTML()
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # update apm in case of changes in sample_filter inputs
    shiny::observeEvent(input$sample_filter,
      {
        shiny::req(apm(), a())
        tmp <- apm()
        if (!identical(input$sample_filter, tmp[[a()]]$sample_filter)) {
          message("[m_analyte] update 'sample_filter'")
          if (length(input$sample_filter) < length(tmp[[a()]]$sample_ids) - 1) {
            if (is.null(input$sample_filter)) {
              tmp[[a()]]["sample_filter"] <- list(NULL)
            } else {
              tmp[[a()]]$sample_filter <- input$sample_filter
            }
            apm(tmp)
          } else {
            err_msg("Sorry. Please keep at least 2 replicates for non-filtered labs.")
            shiny::updateSelectizeInput(
              inputId = "sample_filter",
              choices = apm()[[a()]]$sample_ids,
              selected = apm()[[a()]]$sample_filter
            )
          }
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # update apm in case of changes in lab_filter inputs
    shiny::observeEvent(input$lab_filter,
      {
        shiny::req(apm(), a())
        tmp <- apm()
        if (!identical(input$lab_filter, tmp[[a()]]$lab_filter)) {
          message("[m_analyte] update 'lab_filter'")
          if (length(input$lab_filter) < length(tmp[[a()]]$lab_ids) - 1) {
            if (is.null(input$lab_filter)) {
              tmp[[a()]]["lab_filter"] <- list(NULL)
            } else {
              tmp[[a()]]$lab_filter <- input$lab_filter
            }
            apm(tmp)
          } else {
            err_msg("Sorry. Please keep at least 2 labs as statistical tests will fail otherwise.")
            shiny::updateSelectizeInput(
              inputId = "lab_filter",
              choices = apm()[[a()]]$lab_ids,
              selected = apm()[[a()]]$lab_filter
            )
          }
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # update apm in case of changes in pooling
    shiny::observeEvent(input$pooling,
      {
        shiny::req(apm(), a())
        tmp <- apm()
        if (!identical(input$pooling, tmp[[a()]]$pooling)) {
          message("[m_analyte] update 'pooling'")
          tmp[[a()]]$pooling <- input$pooling
          apm(tmp)
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # whenever any analyte parameter is changed update the reactive value in the R6 object
    shiny::observeEvent(apm(),
      {
        if (!identical(getValue(rv, c("General", "apm")), apm())) {
          message("[m_analyte] apm changed, set rv.apm")
          setValue(rv, c("General", "apm"), apm())
        }
      },
      ignoreNULL = TRUE
    )

    # help modals
    shiny::observeEvent(input$analyte_help_link, {
      show_help("certification_analyte_options")
    })
  })
}
