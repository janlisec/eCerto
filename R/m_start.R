#' @name mod_Start
#' @aliases m_startUI
#' @aliases m_startServer
#'
#' @title start MODULE
#'
#' @description \code{m_start} is the module for eCerto startup.
#'
#' @details Providing the backup/restore modules as well as the Excel upload.
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv The gloobal R6 object.
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_startUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- reactiveClass$new(init_rv()) # initiate persistent variables
#'    datreturn <- ecerto:::test_datreturn()
#'    m_startServer(id = "test", rv = rv)
#'  }
#' )
#' }
#'
#' @return h_vals = The start data (not the transferred ones yet)
#' @rdname mod_start
#' @export

m_startUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 3,
      shiny::wellPanel(
        shiny::wellPanel(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::strong("Restart Session"),
              shiny::br(),
              shiny::actionButton(inputId = ns("session_restart"), label = "Restart")
            ),
            shiny::column(
              width = 6,
              shiny::strong("Load Test Data"),
              shiny::br(),
              shiny::actionButton(inputId = ns("load_test_data"), label = "Load")
            )
          )
        ),
        m_RDataImport_UI(ns("Rdatain")),
        m_RDataExport_UI(ns("Rdataex"))
      )
    ),
    shiny::column(
      width = 9,
      shiny::wellPanel(
        shiny::selectInput(
          inputId = ns("moduleSelect"),
          choices = NULL,
          label = shiny::actionLink(inputId = ns("moduleUploadHelp"), label = "Module"),
          width = "50%"

        ),
        m_ExcelUpload_UI(ns("excelfile"))
      )
    )
  )
}

#' @rdname mod_start
#' @export
m_startServer = function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    # Certification, Homogeneity, Stability -----------------------------------
    shiny::updateSelectInput(
      inputId = "moduleSelect",
      session = session,
      choices = getValue(rv, "modules")
    )

    # Upload Controller -------------------------------------------------------
    ExcelUp <- m_ExcelUpload_Server(
      id = "excelfile",
      exl_fmt = shiny::reactive({input$moduleSelect})
    )

    m_RDataexport_Server(id = "Rdataex", rv = rv)

    rv_rdata <- m_RDataImport_Server(
      id = "Rdatain",
      modules = shiny::reactive({getValue(rv,"modules")}),
      uploadsources = shiny::reactive({
        sapply(getValue(rv, "modules"), function(x) {
          getValue(rv, c(x,"uploadsource"))
        })
      })
    )

    shiny::observeEvent(rv_rdata(),{
      shiny::updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification"
      )
      rv_rdatanames <- listNames(rv_rdata(), split = TRUE)
      # overwrite
      for (n in rv_rdatanames) {
        setValue(rv,n,getValue(rv_rdata(),n))
      }
    }, ignoreNULL = TRUE)

    # when Excel was uploaded with LOAD-Button...
    shiny::observeEvent(ExcelUp$data,{
      # ToDo: make silent a global parameter
      #if (!silent) message("app_server: (Excel Upload) set rv.Data; set rv.Uploadsource")
      ex_frm <- input$moduleSelect
      setValue(rv, c(ex_frm, "data"), ExcelUp$data)
      setValue(rv, c(ex_frm, "input_files"), ExcelUp$input_files)
      setValue(rv, c(ex_frm, "uploadsource"), value = "Excel")
    })

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
      shiny::showModal(shiny::modalDialog(
        easyClose = FALSE,
        title="Sure you want to load test data?",
        "This will erase all non-saved inputs!",
        footer = shiny::tagList(
          shiny::actionButton(session$ns("confirmLoadTestData"), "Load Test Data"),
          shiny::modalButton("Cancel")
        )
      ))
    })

    shiny::observeEvent(input$confirmLoadTestData, {
      # browser()
      ne <- new.env()
      load(file = fnc_get_local_file(x = "CRM001.RData"), envir = ne)
      res <- get("res", ne)
      rv_test <- fnc_load_RData(x = res)
      rv_testnames <- listNames(rv_test, split = TRUE)
      # overwrite
      for (n in rv_testnames) {
        setValue(rv, n, getValue(rv_test, n))
      }
      shiny::removeModal()
    })

    # Action link for help
    shiny::observeEvent(input$moduleUploadHelp, {
      switch (input$moduleSelect,
              "Certification" = help_the_user("certification_dataupload", modal=TRUE),
              "Homogeneity" = help_the_user("homogeneity_dataupload", modal=TRUE),
              "Stability" = help_the_user("stability_dataupload", modal=TRUE)
      )
    })

  })

}

