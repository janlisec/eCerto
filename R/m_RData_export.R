#' @title Modul RData-Export
#' @details not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv eCerto class object.
#'
#' @return Nothing.
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(m_RDataExport_UI(id = "test"), shiny::actionButton("insert", "insert rv")),
#'     server = function(input, output, session) {
#'       rv_test <- eCerto::eCerto$new(eCerto:::init_rv())
#'       eCerto:::m_RDataexport_Server(id = "test", rv = rv_test)
#'       observeEvent(input$insert, {
#'         shiny::isolate({
#'           setValue(rv_test, c("Certification", "data"), test_Certification_Excel())
#'         })
#'         shiny::isolate({
#'           setValue(rv_test, c("General", "user"), "FK4")
#'         })
#'       })
#'     }
#'   )
#' }
#' @noRd
m_RDataExport_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      id = ns("savepanel"),
      sub_header("Save Current Analysis"),
      shiny::textInput(inputId = ns("user"), label = "User", value = "Jan Lisec"),
      shiny::textInput(inputId = ns("study_id"), label = "Study ID", value = "CRM001"),
      shiny::downloadButton(outputId = ns("ecerto_backup"), label = "Backup")
    )
  )
}

#' @noRd
m_RDataexport_Server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    silent <- get_golem_config("silent") # messages

    shiny::observeEvent(input$user, {
      if (!identical(input$user, getValue(rv, c("General", "user")))) {
        if (!silent) message("RData-export: set rv$user to ", input$user)
        setValue(rv, c("General", "user"), input$user)
      }
    })
    shiny::observeEvent(getValue(rv, c("General", "user")), {
      if (!identical(input$user, getValue(rv, c("General", "user")))) {
        if (!silent) message("RData-export: user-input updated to ", getValue(rv, c("General", "user")))
        shiny::updateTextInput(
          session = session,
          inputId = "user",
          value = getValue(rv, c("General", "user"))
        )
      }
    })

    shiny::observeEvent(input$study_id, {
      if (!identical(input$study_id, getValue(rv, c("General", "study_id")))) {
        if (!silent) message("RData-export: set rv$study_id to ", input$study_id)
        setValue(rv, c("General", "study_id"), input$study_id)
      }
    })
    shiny::observeEvent(getValue(rv, c("General", "study_id")), {
      if (!identical(input$study_id, getValue(rv, c("General", "study_id")))) {
        if (!silent) message("RData-export: study_id-input updated to ", getValue(rv, c("General", "study_id")))
        shiny::updateTextInput(
          session = session,
          inputId = "study_id",
          value =  getValue(rv, c("General", "study_id"))
        )
      }
    })

    # DOWNLOAD
    output$ecerto_backup <- shiny::downloadHandler(
      filename = function() {
        paste0(ifelse(is.null(getValue(rv, c("General", "study_id"))), "TEST", getValue(rv, c("General", "study_id"))), ".RData")
      },
      content = function(file) {
        res <- sapply(rv$get(), function(x) {
          if (shiny::is.reactivevalues(x)) {
            shiny::reactiveValuesToList(x)
          } else {
            x
          }
        })
        res$General$dataformat_version <- "2021-05-27"
        save(res, file = file)
      },
      contentType = "RData"
    )
  })
}
