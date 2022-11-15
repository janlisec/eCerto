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
#'    rv <- eCerto::eCerto$new(init_rv()) # initiate persistent variables
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
      width = 3,
      shiny::wellPanel(
        shiny::wellPanel(
          shiny::fluidRow(
            shiny::tagList("Click on", shiny::actionLink(inputId = ns("getHelp"), label = "this Link"), shiny::HTML("when you are <span style='color: red;'>a first time user</span> to get help!"))
          )
        ),
        shiny::wellPanel(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              sub_header("Restart Session", b=3),
              shiny::actionButton(inputId = ns("session_restart"), label = "Restart", width="80%")
            ),
            shiny::column(
              width = 6,
              sub_header("Load Test Data", b=3),
              shiny::actionButton(inputId = ns("load_test_data"), label = "Load", width="80%")
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
          label = shiny::tagList("Module (click", shiny::actionLink(inputId = ns("moduleUploadHelp"), label = "here"),  "to see example format)"),
          width = "50%"
        ),
        m_ExcelUpload_UI(ns("excelfile"))
      )
    )
  )
}

#' @noRd
page_startServer = function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    silent <- get_golem_config("silent")

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
        setValue(rv, n, getValue(rv_rdata(), n))
      }
    }, ignoreNULL = TRUE)

    # when Excel was uploaded with LOAD-Button...
    shiny::observeEvent(ExcelUp$data, {
      if (!silent) message("[page_start] (Excel Upload) set rv.Data; set rv.Uploadsource")
      ex_frm <- input$moduleSelect
      setValue(rv, c(ex_frm, "data"), ExcelUp$data)
      setValue(rv, c(ex_frm, "input_files"), ExcelUp$input_files)
      setValue(rv, c(ex_frm, "uploadsource"), value = "Excel")
      if (ex_frm=="Certification") {
        # (re)initiate apm and materialtabelle
        setValue(rv, c("General","apm"), init_apm(getValue(rv, c("Certification", "data"))))
        setValue(rv, c("General","materialtabelle"), init_materialtabelle(levels(getValue(rv, c("Certification", "data"))[,"analyte"])))
      }
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
      # check if data was already uploaded or this is a new session
      if (all(sapply(getValue(rv, "modules"), function(x) { is.null(getValue(rv, c(x,"data"))) }))) {
        res <- eCerto::CRM001
        rv_test <- fnc_load_RData(x = res)
        rv_test_names <- listNames(rv_test, split = TRUE)
        rv_name <- listNames(rv, split = TRUE)
        if (identical(rv_test_names, rv_name)) {
          for (n in rv_test_names) {
            setValue(rv, n, getValue(rv_test, n))
          }
        } else {
          message("Probably the format of 'rv' has changed. Please update 'testdata.RData'")
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
    # Action link for help
    shiny::observeEvent(input$moduleUploadHelp, {
      switch(
        input$moduleSelect,
        "Certification" = help_the_user_modal("certification_dataupload"),
        "Homogeneity" = help_the_user_modal("homogeneity_dataupload"),
        "Stability" = help_the_user_modal("stability_dataupload")
      )
    })

    shiny::observeEvent(input$getHelp, { help_the_user_modal("start_gethelp") })

  })

}

