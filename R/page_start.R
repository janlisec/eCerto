#' @title Start-Page
#' @description \code{page_start} is the module for eCerto startup.
#'
#' @details Providing the backup/restore modules as well as the Excel upload.
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv The global R6 object.
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    eCerto:::page_startUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- eCerto::eCerto$new(eCerto:::init_rv()) # initiate persistent variables
#'    eCerto:::page_startServer(id = "test", rv = rv)
#'  }
#' )
#' }
#'
#' @return Nothing
#' @noRd

page_startUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 10,
      m_ExcelUpload_UI(ns("excelfile"))
    ),
    shiny::column(
      width = 2,
      shiny::wellPanel(
        shiny::actionButton(inputId = ns("load_test_data"), label = "Load Test Data", width="100%", style = "font-weight: 700; background-color: rgb(0,175,240);"),
        hr(),
        m_RDataImport_UI(ns("Rdatain")),
        hr(),
        m_RDataExport_UI(ns("Rdataex")),
        hr(),
        shiny::actionButton(inputId = ns("session_restart"), label = "Restart eCerto App", width="100%", style = "font-weight: 700; background-color: rgb(210,0,30)")

      )
    )
  )
}

#' @noRd
page_startServer = function(id, rv, msession = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    silent <- get_golem_config("silent")

    # Upload Controller -------------------------------------------------------
    m_ExcelUpload_Server(id = "excelfile", rv = rv, msession = msession)

    m_RDataexport_Server(id = "Rdataex", rv = rv)

    m_RDataImport_Server(id = "Rdatain", rv = rv)

    # Restart App --------------------------------------------------------------
    # Open confirmation dialog
    shiny::observeEvent(input$session_restart, {
      shiny::showModal(shiny::modalDialog(
        easyClose = FALSE,
        title="Sure you want to restart the session?",
        "This will erase all non-saved inputs!",
        footer = shiny::tagList(
          shiny::actionButton(session$ns("confirmRestart"), "Restart"),
          shiny::modalButton("Cancel")
        )
      ))
    })
    shiny::observeEvent(input$confirmRestart, {
      session$reload()
      shiny::removeModal()
    })

    # helper function
    load_test_data <- function() {
      rv_test <- fnc_load_RData(x = eCerto::CRM001)
      rv_test_names <- listNames(rv_test, split = TRUE)
      rv_name <- listNames(rv, split = TRUE)
      if (identical(rv_test_names, rv_name)) {
        for (n in rv_test_names) {
          setValue(rv, n, getValue(rv_test, n))
        }
        # set current analyte to trigger C Modul elements
        rv$cur_an <- unname(rv$a_p("name")[1])
      } else {
        message("Probably the format of 'rv' has changed. Please update 'data/CRM001.rda'")
      }
    }
    # Load Test Data -----------------------------------------------------------
    shiny::observeEvent(input$load_test_data, {
      # check if data was already uploaded or this is a new session
      if (all(sapply(getValue(rv, "modules"), function(x) { is.null(getValue(rv, c(x, "data"))) }))) {
        load_test_data()
      } else {
        shiny::showModal(shiny::modalDialog(
          easyClose = FALSE,
          title="Warning",
          "Please click 'Cancel' to save your current data or 'Overwrite' to proceed!",
          footer = shiny::tagList(
            shiny::modalButton("Cancel"),
            shiny::actionButton(inputId = ns("overwrite"), label = "Overwrite", class = "btn btn-danger")
          )
        ))
      }
    })

    continue <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$overwrite, {
      continue(TRUE)
      shiny::showNotification("Overwritten")
      shiny::removeModal()
    })
    shiny::observeEvent(continue(), {
      load_test_data()
      continue(NULL)
    }, ignoreNULL = TRUE)

    # Help section -------------------------------------------------------------
    shiny::observeEvent(input$getHelp, { show_help("start_gethelp") })

  })

}

