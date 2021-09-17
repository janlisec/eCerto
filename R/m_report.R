#' @name mod_report
#' @aliases m_report_ui
#' @aliases m_report_server
#'
#' @title report
#'
#' @param id module ID
#' @param rv the rv-reactiveClass object
#' @param selected_tab which analyte-tab is currently selected
#' @param silent default FALSE
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
#'   rv <- reactiveClass$new(init_rv()) # initiate persistent variables
#'    shiny::isolate({setValue(rv, c("General","study_id"), "Jan") })
#'   shiny::isolate({setValue(rv, c("Certification","data"), test_Certification_Excel()) })
#'   shiny::isolate({set_uploadsource(rv, "Certification", uploadsource = "Excel") })
#'   selected_tab = reactiveVal("Si")
#'
#'  m_report_server(
#'      id = "test",
#'      rv = rv,
#'      selected_tab = selected_tab
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
    shiny::wellPanel(
      shiny::fluidRow(shiny::strong("Download Report")),
      shiny::fluidRow(
        shiny::radioButtons(
          inputId = ns("output_file_format"),
          label = NULL,
          choices = c('PDF', 'HTML', 'Word'),
          inline = TRUE
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          align = "left",
          shiny::downloadButton(outputId = ns('FinalReport'), label = "Analyte")),
        shiny::column(
          width = 6,
          align = "right",
          shiny::downloadButton(outputId = ns('MaterialReport'), label = "Material")
        )
      )
    )
  )
}

#' @rdname mod_report
#' @export
m_report_server <- function(id, rv, selected_tab, silent=FALSE) {

  shiny::moduleServer(id, function(input, output, session) {

    output$FinalReport <- shiny::downloadHandler(
      filename = function() {
        paste0(getValue(rv, c("General","study_id")), "_", selected_tab(), '.', switch(
          input$output_file_format,
          PDF = 'pdf',
          HTML = 'html',
          Word = 'docx'
        ))
      },
      content = function(file) {
        # https://shiny.rstudio.com/gallery/download-knitr-reports.html
        # src <- normalizePath("report_vorlage.Rmd")
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        # file.copy(src, "report_vorlage.Rmd", overwrite = TRUE)
        
        out <- rmarkdown::render(
          input = fnc_get_local_file("report_vorlage.Rmd", copy_to_tempdir = FALSE),
          output_format = switch(
            input$output_file_format,
            PDF = rmarkdown::pdf_document(),
            HTML = rmarkdown::html_document(),
            Word = rmarkdown::word_document()
          ),
          params = list(
            "General" = shiny::reactiveValuesToList(getValue(rv,"General")),
            "Certification" = c(
              shiny::isolate(shiny::reactiveValuesToList(getValue(rv,"Certification"))),
              shiny::isolate(shiny::reactiveValuesToList(getValue(rv,"Certification_processing")))
            ),
            "selected_tab" = selected_tab()
          ),
          # !!! das ist die Liste mit Eingabewerten für die weitere Verarbeitung im Report
          # envir = new.env(parent = globalenv())
        )
        file.rename(out, file)
      }
    )


    # REPORT Material
    # output$MaterialReport <- downloadHandler(
    #   filename = function() {
    #     paste0(getValue(rv, c("General","study_id")), "_", getValue(rv, c("General","user")), '.', switch(
    #       input$output_file_format,
    #       PDF = 'pdf',
    #       HTML = 'html',
    #       Word = 'docx'
    #     ))
    #   },
    #   content = function(file) {
    #     # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
    #     owd <- setwd(tempdir(check = TRUE))
    #     on.exit(setwd(owd))
    #     writeLines(text = Report_Vorlage_Material(), con = 'tmp_Report.Rmd')
    #     out <- rmarkdown::render(
    #       input = 'tmp_Report.Rmd',
    #       output_format = switch(
    #         input$output_file_format,
    #         PDF = rmarkdown::pdf_document(),
    #         HTML = rmarkdown::html_document(),
    #         Word = rmarkdown::word_document()
    #       ),
    #       params = list("res" = c_res()),
    #       # !!! das ist die Liste mit Eingabewerten für die weitere Verarbeitung im Report
    #       envir = new.env(parent = globalenv())
    #     )
    #     file.rename(out, file)
    #   }
    # )

  })

}