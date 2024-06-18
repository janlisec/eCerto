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
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       eCerto:::page_startUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       rv <- eCerto::eCerto$new(eCerto:::init_rv()) # initiate persistent variables
#'       eCerto:::page_startServer(id = "test", rv = rv)
#'     }
#'   )
#' }
#'
#' @return Nothing
#' @noRd

page_startUI <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 360,
        shiny::div(
          m_RDataImport_UI(ns("Rdatain")),
          hr(),
          m_RDataExport_UI(ns("Rdataex")),
          hr(),
          shiny::actionButton(inputId = ns("session_restart"), label = shiny::HTML("Restart<br>eCerto"), style = "width: 150px; font-weight: 700; background-color: rgb(210,0,30)")
        )
      ),
      m_ExcelUpload_UI(ns("excelfile"))
    )
  )
}

#' @noRd
page_startServer <- function(id, rv, msession = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    # Upload Controller -------------------------------------------------------
    m_ExcelUpload_Server(id = "excelfile", rv = rv, msession = msession)
    m_RDataexport_Server(id = "Rdataex", rv = rv)
    m_RDataImport_Server(id = "Rdatain", rv = rv)

    # Restart App --------------------------------------------------------------
    # Open confirmation dialog
    shiny::observeEvent(input$session_restart, {
      shiny::showModal(shiny::modalDialog(
        easyClose = FALSE,
        title = "Sure you want to restart the session?",
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

    # Help section -------------------------------------------------------------
    shiny::observeEvent(input$getHelp, { show_help("start_gethelp") })

  })
}
