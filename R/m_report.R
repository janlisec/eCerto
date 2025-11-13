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
          shiny::downloadButton(outputId = ns("AnalyteReport"), label = "Analyte", style = "width: 135px;"),
          shiny::downloadButton(outputId = ns("MaterialReport"), label = "CRM", style = "width: 135px;"),
          shiny::downloadButton(outputId = ns("DRMDSnippet"), label = "DRMD", style = "width: 135px;")
        )
      )
    )
  )
}

#' @noRd
#' @keywords internal
m_reportServer <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    output$DRMDSnippet <- shiny::downloadHandler(
      filename = function() {
        paste0(getValue(rv, c("General", "study_id")), "_DRMD.xml")
      },
      content = function(file) {
        # Laden der XSD-Datei für SI units von der PTB URL
        #xsd <- xml2::read_xml("https://ptb.de/si/v2.2.1/SI_Format.xsd")
        #flatten_list_to_df(xml2::as_list(xsd))

        mt <- styleTabC3(x = getValue(rv, c("General", "materialtabelle")), apm = getValue(rv, c("General", "apm")))$x$data
        mt_xml <- lapply(1:nrow(mt), function(i) {
          new_dcc_quantity_result(
            name = mt[i,"analyte"],
            label = mt[i,"analyte"],
            value = mt[i,"\u00B5<sub>c</sub>"],
            unit = mt[i,"unit"],
            uncertainty = mt[i,"U<sub>abs</sub>"],
            coverageFactor = mt[i,"k"]
          )
        })

        drmd_lst <- new_drmd_document(
          admin_data = new_drmd_admin_data(),
          result_data = new_drmd_measurementResult(quantities = mt_xml)
        )

        drmd_xml <- validate_drmd_xml(drmc = xml2::as_xml_document(x = drmd_lst))

        # write to xml file
        return(xml2::write_xml(x = drmd_xml, file = file))
      }
    )

    output$AnalyteReport <- shiny::downloadHandler(
      filename = function() { paste0(getValue(rv, c("General", "study_id")), "_", rv$cur_an, ".", "html") },
      content = function(file) {
        render_report_A(file = file, rv = rv)
      }
    )

    output$MaterialReport <- shiny::downloadHandler(
      filename = function() { paste0(getValue(rv, c("General", "study_id")), "_", "Material", ".", "html") },
      content = function(file) {
        render_report_M(file = file, "mt" = shiny::isolate(getValue(rv, c("General", "materialtabelle"))), "gen" = shiny::reactiveValuesToList(getValue(rv, "General")))
      }
    )

    shiny::observeEvent(input$help_link, { show_help("certification_report") })
  })
}
