#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
xlsxload_parameter_UI = function(id) {
  shiny::tagList(shiny::tabsetPanel(
    id = shiny::NS(id, "params"),
    type = "hidden",
    shiny::tabPanel(
      "Certifications",
      shiny::sliderInput(
        shiny::NS(id, "rowslider"),
        "Rows",
        value = c(0, 20),
        min = 0,
        max = 100
      ),
      shiny::sliderInput(
        shiny::NS(id, "colslider"),
        "Columns",
        value = c(0, 1),
        min = 0,
        max = 2
      )
    ),
    shiny::tabPanel("Homogeneity"),
    shiny::tabPanel("Stability")
  ))
}

#' Title
#'
#' @param id 
#' @param dat 
#' @param excelformat 
#'
#' @return
#' @export
#'
#' @examples
xlsxload_parametertabsServer = function(id, dat, excelformat) {
  stopifnot(is.reactive(dat))
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(excelformat(), {
      shiny::updateTabsetPanel(session = session,
                               inputId = "params",
                               selected = excelformat())
      sliderupdate(session, dat)
    })
    
    # update slider when new data set
    shiny::observeEvent(dat(), {
      sliderupdate(session, dat)
    })
    
    # TODO validation part here
    
    # returns list with selected additional parameters (if any)
    list(
      param_format = shiny::reactive(excelformat()),
      start_row = shiny::reactive(input$rowslider[1]),
      end_row = shiny::reactive(input$rowslider[2]),
      start_col = shiny::reactive(input$colslider[1]),
      end_col = shiny::reactive(input$colslider[2])
    )
  })
}