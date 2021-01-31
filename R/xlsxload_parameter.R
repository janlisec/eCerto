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
    
    # cd creates a random number everytime data, excel format or rowsliders change
    # so that reactive gets invalidated even with unchanged
    cd = reactiveVal() 
    
    observeEvent(excelformat(), {
      updateTabsetPanel(session = session,
                        inputId = "params",
                        selected = excelformat())
      cd(rnorm(1))
      sliderupdate(session, dat)
    })
    
    # update slider when new data set
    observeEvent(dat(), {
      sliderupdate(session, dat)
    })
    
    observeEvent({
      input$rowslider
      input$colslider
    },{
      cd(rnorm(1))
    })
    
    # TODO validation part here
    
    # returns list with selected additional parameters (if any)
    list(
      change_detector = cd, # generate random number to trigger event even with unchanged inputs
      param_format = reactive(excelformat()),
      start_row = reactive(input$rowslider[1]),
      end_row = reactive(input$rowslider[2]),
      start_col = reactive(input$colslider[1]),
      end_col = reactive(input$colslider[2])
    )
  })
}