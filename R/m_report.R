#' @name mod_report
#' @aliases m_report_ui
#' @aliases m_report_server
#'
#' @title report
#'
#' @param id module ID
#' @param rv the rv eCerto object
#' @param selected_tab which analyte-tab is currently selected
#'
#' @return nothing
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       eCerto:::m_reportUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       rv <- eCerto:::test_rv(type = "SR3") # initiate persistent variables
#'       shiny::isolate({
#'         setValue(rv, c("General", "study_id"), "Jan")
#'       })
#'       shiny::addResourcePath("www", system.file("app", "www", package = "eCerto"))
#'       eCerto:::m_reportServer(
#'         id = "test",
#'         rv = rv
#'       )
#'     }
#'   )
#' }
#'
#' @importFrom knitr is_html_output kable
#' @importFrom magick image_read
#' @noRd
#' @keywords internal
m_reportUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::dropdown(
      inputId = ns("dropdown_report"),
      label = "Report",
      width = "335px",
      circle = FALSE,
      shiny::tagList(
        shiny::div(
          style = "float: left; padding-bottom: 0.5em;",
          sub_header(shiny::actionLink(inputId = ns("help_link"), label = "Download HTML Report")),
          shiny::downloadButton(outputId = ns("AnalyteReport"), label = "Analyte"),
          shiny::downloadButton(outputId = ns("MaterialReport"), label = "CRM")
        )
      )
    )
  )
}

#' @noRd
#' @keywords internal
m_reportServer <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    output$AnalyteReport <- shiny::downloadHandler(
      filename = function() {
        paste0(
          getValue(rv, c("General", "study_id")), "_", rv$cur_an, ".", "html"
          # switch(
          #   input$output_file_format,
          #   PDF = 'pdf',
          #   HTML = 'html',
          #   Word = 'docx'
          # )
        )
      },
      content = function(file) {
        rmdfile <- get_local_file("report_vorlage_analyt.[Rr][Mm][Dd]$")
        logofile <- "BAMLogo2015.png"
        # render the markdown file
        shiny::withProgress(
          expr = {
            incProgress(0.5)
            out <- rmarkdown::render(
              input = rmdfile,
              output_file = file,
              output_format = rmarkdown::html_document(),
              params = list(
                "General" = shiny::reactiveValuesToList(getValue(rv, "General")),
                "Certification" = shiny::reactiveValuesToList(getValue(rv, "Certification")),
                "Certification_processing" = shiny::reactiveValuesToList(getValue(rv, "Certification_processing")),
                "selected_tab" = rv$cur_an,
                "logo_file" = logofile
              ),
              envir = new.env(parent = globalenv()),
              runtime = c("auto", "shiny", "shinyrmd", "shiny_prerendered")[2]
            )
          },
          message = "Rendering Analyte Report.."
        )
        return(out)
      }
    )

    output$MaterialReport <- shiny::downloadHandler(
      filename = function() {
        paste0(
          getValue(rv, c("General", "study_id")), "_", "Material", ".", "html"
          # switch(
          #   input$output_file_format,
          #   PDF = 'pdf',
          #   HTML = 'html',
          #   Word = 'docx'
          # )
        )
      },
      content = function(file) {
        # Copy the report file to a temporary directory before processing it
        rmdfile <- get_local_file("report_vorlage_material.[Rr][Mm][Dd]$")
        logofile <- "BAMLogo2015.png"
        # render the markdown file
        shiny::withProgress(
          expr = {
            incProgress(0.5)
            rmarkdown::render(
              input = rmdfile,
              output_file = file,
              output_format = switch(
                # input$output_file_format,
                "HTML",
                PDF = rmarkdown::pdf_document(),
                HTML = rmarkdown::html_document(),
                Word = rmarkdown::word_document()
              ),
              params = list(
                "materialtabelle" = shiny::isolate(getValue(rv, c("General", "materialtabelle"))),
                "General" = shiny::reactiveValuesToList(getValue(rv, "General")),
                "logo_file" = logofile
              ),
              envir = new.env(parent = globalenv())
            )
          },
          message = "Rendering Material Report.."
        )
      }
    )

    shiny::observeEvent(input$help_link, {
      show_help("certification_report")
    })
  })
}
