#' @title RDataImport.
#'
#' @description \code{RDataImport} will provide a module to upload/backup Rdata
#'   files for certification trial data.
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv The eCerto object.
#'
#' @return A reactive object `rdata`, but returned only for notifying the side effect
#'     to notiy the navbarpanel to update.
#'
#' @examples
#' if (interactive()) {
#' rv <- eCerto:::test_rv()
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(eCerto:::m_RDataImport_UI(id = "test")),
#'  server = function(input, output, session) {
#'    eCerto:::m_RDataImport_Server(id = "test", rv = rv)
#'    shiny::observeEvent(rv$e_present(), { print(rv$e_present()) })
#'  }
#' )
#' }
#'
#' @noRd
#' @keywords internal
m_RDataImport_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::actionButton(inputId = ns("load_test_data"), label = "Load Test Data", style = "width: 100%; max-width: 160px; font-weight: 700; background-color: rgb(0,175,240); margin-bottom: 10px;"),
    shiny::actionButton(inputId = ns("load_zenodo_data"), label = "Load from Zenodo", style = "width: 100%; max-width: 160px; font-weight: 700; background-color: rgb(0,175,240); margin-bottom: 10px;"),
    shiny::fileInput(
      inputId = ns("in_file_ecerto_backup"),
      label = "Load Previous Analysis",
      multiple = FALSE,
      accept = c("RData")
    )
  )
}

#' @noRd
#' @keywords internal
m_RDataImport_Server = function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    silent <- get_golem_config("silent")

    # helper function
    load_test_data <- function(x = NULL) {
      if (is.null(x)) {
        rv_test <- list2rv(x = eCerto::CRM001)
      } else {
        rv_test <- list2rv(x = x)
      }
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

    div_check_present <- function() {
      msg <- shiny::HTML("")
      if (any(rv$e_present())) {
        msg <- paste0("You have data in the module", ifelse(sum(rv$e_present())>1, "s <u>", " <u>"), paste(names(which(rv$e_present())), collapse="</u> and <u>"),
                      "</u>.<br>This data will be overwritten if you proceed. Click 'Cancel' if you need to backup the current data.")
        msg <- shiny::div(style = "background: red; padding: 10px; margin-bottom: 10px; color: white;", shiny::HTML(msg))
      }
      return(msg)
    }

    # Load Test Data -----------------------------------------------------------
    shiny::observeEvent(input$load_test_data, {
      # check if data was already uploaded or this is a new session
      if (all(!rv$e_present())) {
        load_test_data()
      } else {
        shinyalert::shinyalert(
          title = "Test Data", confirmButtonText = "Load", showCancelButton = TRUE, size = "xs", html = TRUE,
          text = shiny::tagList(div_check_present()),
          callbackR = function(value) {
            if (value) { load_test_data() }
          }
        )
      }
    })

    # Load Zenodo Data ---------------------------------------------------------
    shiny::observeEvent(input$load_zenodo_data, {
      shinyalert::shinyalert(
        title = "Zenodo Import", confirmButtonText = "Load", showCancelButton = TRUE, size = "xs", html = TRUE,
        text = shiny::tagList(div_check_present(), shiny::textInput(inputId = session$ns("z_id"), label = "Enter Zonodo Record ID", value = "8380870")),
        callbackR = function(value) {
          if (value) {
            x <- read_zenodo(input$z_id)
            load_test_data(x = x)
          }
        }
      )
    })

    # Load Backup Data ---------------------------------------------------------
    rdata <- shiny::eventReactive(input$in_file_ecerto_backup, {
      check_RData(x = input$in_file_ecerto_backup$datapath)
    }, ignoreNULL = TRUE)

    shiny::observeEvent(rdata(), {
      if (all(!rv$e_present())) {
        load_test_data(x = rdata())
      } else {
        shinyalert::shinyalert(
          title = "RData Import", confirmButtonText = "Load", showCancelButton = TRUE, size = "xs", html = TRUE,
          text = shiny::tagList(div_check_present()),
          callbackR = function(value) {
            if (value) { load_test_data(x = rdata()) }
          }
        )
      }
    })

  })

}

