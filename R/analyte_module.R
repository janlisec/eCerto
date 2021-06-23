#' @name analyteModule
#' @aliases m_analyteModuleUI
#' @aliases m_analyteModuleServer
#'
#' @title Analyte-module
#'
#' @description \code{m_analyte} Module for organizing the analyte panel, which
#'  fill automatically after analytes are available and gives the user the
#'  opportunity to select analytes as well as precision and filter samples.
#'
#' @details not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param apm reactiveValues object, which gives available analytes, holds parameter, etc.
#'
#' @return the currently selected tab. Other parameter via apm reactiveValues()
#'
#' @rdname analyteModule
#' @export
#'
m_analyteModuleUI = function(id){
  # empty tabset panel, to be filled by the analytes in the server Module
  shiny::tagList(
    shinyjs::inlineCSS('.selct  {background: green; color: white;border: 5px solid black;}'),
    shiny::tabsetPanel(id = shiny::NS(id,"tabs"))
  )

}

#' @rdname analyteModule
#' @export
m_analyteServer = function(id, apm) {
  stopifnot(shiny::is.reactivevalues(apm))
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns # to get full namespace here in server function
    analytes = shiny::isolate(shiny::reactiveValuesToList(apm))$analytes
    # message("analyteModule; all analytes: ", analytes)

    # append/prepend a tab for each analyte available
    for (a.name in names(analytes)) {
      shiny::appendTab(inputId = "tabs",
          select = FALSE,
          shiny::tabPanel(
            title=a.name,
            shiny::fluidRow(
              shiny::column(6,
                            shiny::selectizeInput(
                 inputId = ns(paste0("flt_samples",a.name)),#NS(id,paste0("flt_samples",a.name)),
                 label = "Filter Sample IDs",
                 choices = apm$analytes[[a.name]]$sample_ids,
                 selected = apm$analytes[[a.name]]$sample_filter,
                 multiple = TRUE
               )
              ),
              shiny::column(6,
                            shiny::numericInput(
                 inputId =ns(paste0("precision",a.name)),
                 label = "Precision",
                 value = 4
               )
              ),
            ),
          )
      )

    }
    # select only first tab

    shiny::updateTabsetPanel(session = session, inputId = "tabs", selected = names(analytes)[1])

    shiny::observeEvent(input$tabs,{
      # change color of tab when selected by changing class
      s = paste0("#",ns("tabs")," li a[data-value=",input$tabs,"]")
      shinyjs::addClass(
        selector = s,
        class = "selct")
      # change the "selected tab" reactive in the reactiveValues when another tab
      # is selected
      # apm$selected_tab = input$tabs
    },ignoreInit = TRUE)

    selected_tab = shiny::eventReactive(input$tabs,{
      input$tabs
    })
    # update precision and the selected sample id filter in the reactiveValues
    # when their value change in the selected tab
    shiny::observe({
      lapply(names(analytes), function(i){
        apm$analytes[[i]]$precision = input[[paste0("precision",i)]]
        apm$analytes[[i]]$sample_filter = input[[paste0("flt_samples",i)]]
      })
    })

    return(selected_tab)
  })
}