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
        shiny::div(
          style = "margin-bottom: 15px;",
          shiny::tagList("Click on", shiny::actionLink(inputId = ns("getHelp"), label = shiny::HTML("<strong>this Link</strong>")), shiny::HTML("when you are <span style='color: red;'>a first time user</span> to get help!"))
        ),
        shiny::fluidRow(
          style = "margin-bottom: 15px;",
          shiny::column(
            width = 6,
            sub_header("Restart Session"),
            shiny::actionButton(inputId = ns("session_restart"), label = "Restart", width="80%")
          ),
          shiny::column(
            width = 6,
            sub_header("Load Test Data"),
            shiny::actionButton(inputId = ns("load_test_data"), label = "Load", width="80%")
          )
        ),
        m_RDataImport_UI(ns("Rdatain")),
        m_RDataExport_UI(ns("Rdataex"))
      )
    )
  )
}

#' @noRd
page_startServer = function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    silent <- get_golem_config("silent")

    # Upload Controller -------------------------------------------------------
    m_ExcelUpload_Server(id = "excelfile", rv = rv)

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

    # Load Test Data -----------------------------------------------------------
    shiny::observeEvent(input$load_test_data, {
      # check if data was already uploaded or this is a new session
      if (all(sapply(getValue(rv, "modules"), function(x) { is.null(getValue(rv, c(x, "data"))) }))) {
        rv_test <- fnc_load_RData(x = eCerto::CRM001)
        rv_test_names <- listNames(rv_test, split = TRUE)
        rv_name <- listNames(rv, split = TRUE)
        if (identical(rv_test_names, rv_name)) {
          for (n in rv_test_names) {
            setValue(rv, n, getValue(rv_test, n))
          }
        } else {
          message("Probably the format of 'rv' has changed. Please update 'data/CRM001.rda'")
        }
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
      res <- eCerto::CRM001
      rv_tmp <- fnc_load_RData(x = res)
      shiny::updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification"
      )
      continue(NULL)
      rv_tmp_names <- listNames(rv_tmp, split = TRUE)
      for (n in rv_tmp_names) {
        setValue(rv, n, getValue(rv_tmp, n))
      }
    }, ignoreNULL = TRUE)

    # Help section -------------------------------------------------------------
    shiny::observeEvent(input$getHelp, { show_help("start_gethelp") })

  })

}

