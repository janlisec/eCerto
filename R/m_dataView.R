#' @title Data View Module
#' @name mod_DataView
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv eCerto R6 object, which includes a 'materialtabelle'.
#'
#' @return Nothing. Will show the imported data for one analyte from an eCerto R6 object.
#' @noRd
#' @keywords internal
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(m_DataViewUI(id = "test")),
#'     server = function(input, output, session) {
#'       rv <- eCerto:::test_rv()
#'       # set S_flt and L_flt for testing
#'       shiny::isolate(apm <- getValue(rv, c("General", "apm")))
#'       apm[["Si"]][["sample_filter"]] <- 4
#'       apm[["Si"]][["lab_filter"]] <- "L1"
#'       shiny::isolate(setValue(rv, c("General", "apm"), apm))
#'       m_DataViewServer(id = "test", rv = rv)
#'     }
#'   )
#' }
#'
m_DataViewUI <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::strong("Tab.C0 - Imported data from collaborative trial"),
      shiny::div(
        shiny::div(style = "float: right; margin-left: 15px; text-align: right;", shiny::checkboxInput(width = 140, inputId = ns("data_view_file"), label = "Show Filenames", value = TRUE)),
        shiny::div(style = "float: right; margin-left: 15px;", shiny::selectInput(width = 140, inputId = ns("data_view_select"), label = NULL, choices = c("kompakt", "standard")))
      )
    ),
    bslib::card_body(
      shiny::div(DT::dataTableOutput(ns("tab1")))
    )
  )
}

#' @noRd
#' @keywords internal
m_DataViewServer <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    # Generate an HTML table view of filtered single analyte data
    output$tab1 <- DT::renderDataTable({
      x <- get_input_data(rv = rv, type = input$data_view_select, excl_file = !input$data_view_file)
      styleTabC0(x = x, ap = getValue(rv, c("General", "apm"))[[rv$cur_an]], type = input$data_view_select)
    })
  })
}
