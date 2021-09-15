#' @title Modul RData-Export
#' @name mod_RDataExport
#' @aliases m_RDataExport_UI
#' @aliases m_RDataexport_Server
#'
#' @details
#' not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv ReavtiveValues $$.
#' @param silent Option to print or omit status messages.
#'
#' @return rdata A reactive, but only for notifying the navbarpanel to change
#'
#' @examples
#' if (interactive()) {
#'    
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(m_RDataExport_UI(id = "test")),
#'  server = function(input, output, session) {
#'  rv_test <- reactiveClass$new(init_rv())
#'     shiny::isolate({setValue(rv_test, c("Certification","data"), test_Certification_Excel()) })
#'     shiny::isolate({setValue(rv_test, c("General", "user"), "FK4") })
#'     shiny::isolate({set_uploadsource(rv_test, "Certification", uploadsource = "Excel") })
#'    m_RDataexport_Server(id = "test", rv = rv_test)
#'  }
#' )
#' }
#' @rdname mod_RDataExport
#' @export
m_RDataExport_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::wellPanel(
      id = ns("savepanel"),
      shiny::strong("Save"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::textInput(
            inputId = ns("user"),
            label = "User",
            value = "Jan Lisec"
          )
        ),
        shiny::column(
          width = 6,
          shiny::textInput(
            inputId = ns("study_id"),
            label = "Study ID",
            value = "TEST"
          )
        ),
        shiny::column(
          width = 3,
          shiny::downloadButton(outputId = ns('ecerto_backup'), label = "Backup")
        )
      )
    )
  )
}

#' @rdname mod_RDataExport
#' @export
m_RDataexport_Server = function(id, rv, silent=FALSE) {
  # stopifnot(shiny::is.reactivevalues(rv$get()))
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$user, {
      if (!silent) message("RData-export: set rv$user to ", input$user)
      setValue(rv,c("General","user"),input$user)
    })
    shiny::observeEvent(getValue(rv,c("General","user")) , {
      if (!silent) message("RData-export: user-input updated to ", getValue(rv,c("General","user")))
      shiny::updateTextInput(
        session = session,
        inputId = "user",
        value = getValue(rv,c("General","user"))
      )
    })
    
    shiny::observeEvent(input$study_id, {
      setValue(rv,c("General","study_id"),input$study_id)
    })
    shiny::observeEvent(getValue(rv,c("General", "study_id")), {
      # if (!silent) message("m_RDataImport_Server: observeEvent(input$study_id")
      shiny::updateTextInput(
        session = session,
        inputId = "study_id",
        value =  getValue(rv,c("General", "study_id"))
      )
    })
    # DOWNLOAD
    output$ecerto_backup <- shiny::downloadHandler(
      
      filename = function() {
        paste0(
          ifelse(
            test = is.null(getValue(rv,c("General", "study_id"))),
            yes =  "TEST",
            no =  getValue(rv,c("General", "study_id")) ) 
          , '.RData')
      },
      content = function(file) {
        #browser()
        res <- sapply(rv$get(), function(x) {
          if(shiny::is.reactivevalues(x)) shiny::reactiveValuesToList(x) else x
        })
        res$General$dataformat_version = "2021-05-27"
        save(res, file = file)
      },
      contentType = "RData"
    )
    
  })
  
}

