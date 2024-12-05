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
#'   rv <- eCerto:::test_rv()
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(eCerto:::m_RDataImport_UI(id = "test")),
#'     server = function(input, output, session) {
#'       eCerto:::m_RDataImport_Server(id = "test", rv = rv)
#'       shiny::observeEvent(rv$e_present(), {
#'         print(rv$e_present())
#'       })
#'     }
#'   )
#' }
#'
#' @noRd
#' @keywords internal
m_RDataImport_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      shiny::actionButton(inputId = ns("load_test_data"), label = shiny::HTML("Load<br>Test Data"), style = "display: inline-block; width: 150px; font-weight: 700; background-color: rgb(0,175,240);"),
      shiny::actionButton(inputId = ns("load_zenodo_data"), label = shiny::HTML("Load from<br>Zenodo"), style = "display: inline-block; width: 150px; font-weight: 700; background-color: rgb(0,175,240);")
    ),
    shiny::div(
      style = "padding-top: 1em;",
      shiny::fileInput(
        inputId = ns("in_file_ecerto_backup"),
        label = "Load Previous Analysis",
        placeholder = "Select Rdata file...",
        multiple = FALSE,
        accept = c("RData")
      )
    )
  )
}

#' @noRd
#' @keywords internal
m_RDataImport_Server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
        e_msg("Probably the format of 'rv' has changed. Please update 'data/CRM001.rda'")
      }
    }

    div_check_present <- function() {
      msg <- shiny::HTML("")
      if (any(rv$e_present())) {
        msg <- paste0(
          "You have data in the module", ifelse(sum(rv$e_present()) > 1, "s <u>", " <u>"), paste(names(which(rv$e_present())), collapse = "</u> and <u>"),
          "</u>.<br>This data will be overwritten if you proceed. Click 'Cancel' if you need to backup the current data."
        )
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
        shinyWidgets::ask_confirmation(
          inputId = "confirm_load_test_data",
          title = "Test Data", btn_labels = c("Cancel", "Load"), size = "xs", html = TRUE,
          text = shiny::tagList(div_check_present())
        )
      }
    })
    shiny::observeEvent(input$confirm_load_test_data, {
      if (input$confirm_load_test_data) {
        load_test_data()
      }
    })

    # Load Zenodo Data ---------------------------------------------------------
    shiny::observeEvent(input$load_zenodo_data, {
      shinyWidgets::ask_confirmation(
        inputId = "confirm_load_zenodo_data",
        title = "Zenodo Import", btn_labels = c("Cancel", "Load"), size = "xs", html = TRUE,
        text = shiny::tagList(
          div_check_present(),
          shiny::div(
            style = "display: block; margin-left: auto; margin-right: auto; width: 220px;",
            shiny::textInput(inputId = session$ns("z_id"), label = "Enter Zenodo Record ID", value = "8380870")
          )
        ),
      )
    })
    shiny::observeEvent(input$confirm_load_zenodo_data, {
      if (input$confirm_load_zenodo_data) {
        load_test_data(x = read_zenodo(input$z_id))
      }
    })

    # Load Backup Data ---------------------------------------------------------
    rdata <- shiny::eventReactive(input$in_file_ecerto_backup,
      {
        check_RData_with_res_object(x = input$in_file_ecerto_backup$datapath)
      },
      ignoreNULL = TRUE
    )

    shiny::observeEvent(rdata(), {
      if (all(!rv$e_present())) {
        load_test_data(x = rdata())
      } else {
        shinyWidgets::ask_confirmation(
          inputId = "confirm_load_rdata",
          title = "RData Import", btn_labels = c("Cancel", "Load"), size = "xs", html = TRUE,
          text = shiny::tagList(div_check_present()),
        )
      }
    })
    shiny::observeEvent(input$confirm_load_rdata, {
      if (input$confirm_load_zenodo_data) {
        load_test_data(x = rdata())
      }
    })
  })
}
