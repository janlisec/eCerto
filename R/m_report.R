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
#'if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_report_ui(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- eCerto::eCerto$new(init_rv()) # initiate persistent variables
#'    shiny::isolate({setValue(rv, c("General","study_id"), "Jan") })
#'    shiny::isolate({setValue(rv, c("Certification","data"), eCerto:::test_Certification_Excel()) })
#'    shiny::isolate({set_uploadsource(rv, "Certification", uploadsource = "Excel") })
#'    m_report_server(
#'      id = "test",
#'      rv = rv,
#'      selected_tab = reactiveVal("Si")
#'    )
#'  }
#' )
#' }
#'
#' @rdname mod_report
#' @export
m_report_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(shiny::strong("Download Report")),
    shiny::p(),
    shiny::fluidRow(
      shiny::column(
        width = 5,
        shiny::radioButtons(
          inputId = ns("output_file_format"),
          label = NULL,
          choices = c('PDF', 'HTML', 'Word'),
          inline = FALSE
        )
      ),
      shiny::column(
        width = 7,
        shiny::downloadButton(outputId = ns('AnalyteReport'), label = "Analyte"),
        p(),
        shiny::downloadButton(outputId = ns('MaterialReport'), label = "Material")
      )
    )
  )
}

#' @rdname mod_report
#' @export
m_report_server <- function(id, rv, selected_tab) {

  shiny::moduleServer(id, function(input, output, session) {

    output$AnalyteReport <- shiny::downloadHandler(
      filename = function() {
        paste0(getValue(rv, c("General","study_id")), "_", selected_tab(), '.', switch(
          input$output_file_format,
          PDF = 'pdf',
          HTML = 'html',
          Word = 'docx'
        ))
      },
      content = function(file) {
        rmdfile <- get_local_file("report_vorlage_analyt.[Rr][Mm][Dd]$")
        logofile <- "BAMLogo2015.png"
        # render the markdown file
        shiny::withProgress(
          expr = {
            incProgress(0.5)
            #browser()
            out <- rmarkdown::render(
              input = rmdfile,
              output_file = file,
              output_format = switch(
                input$output_file_format,
                PDF = rmarkdown::pdf_document(),
                HTML = rmarkdown::html_document(),
                Word = rmarkdown::word_document()
              ),
              params = list(
                "General" = shiny::reactiveValuesToList(getValue(rv,"General")),
                "Certification" = shiny::reactiveValuesToList(getValue(rv,"Certification")),
                "Certification_processing" = shiny::reactiveValuesToList(getValue(rv,"Certification_processing")),
                "selected_tab" = selected_tab(),
                "logo_file" = logofile
              ),
              envir = new.env(parent = globalenv())
            )
          },
          message = "Rendering Analyte Report.."
        )
        return(out)
      }
    )

    output$MaterialReport <- shiny::downloadHandler(
      filename = function() {
        paste0(getValue(rv, c("General","study_id")), "_", "Material", '.', switch(
          input$output_file_format,
          PDF = 'pdf',
          HTML = 'html',
          Word = 'docx'
        ))
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
                input$output_file_format,
                PDF = rmarkdown::pdf_document(),
                HTML = rmarkdown::html_document(),
                Word = rmarkdown::word_document()
              ),
              params = list(
                "materialtabelle" = shiny::isolate(getValue(rv, c("General","materialtabelle"))),
                "General" = shiny::reactiveValuesToList(getValue(rv,"General")),
                "logo_file" = logofile
              ),
              envir = new.env(parent = globalenv())
            )
          },
          message = "Rendering Material Report.."
        )
      }
    )

  })

}