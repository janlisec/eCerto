# ANALYTE MODULE----------------------------------------------------

#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
.analyteModuleUI = function(id){
  wellPanel(tabsetPanel(id = NS(id,"tabs")))
}

#' Title
#'
#' @param id 
#' @param analytelist 
#'
#' @return
#' @export
#'
#' @examples
.analyteModuleServer = function(id, analytelist) {
  stopifnot(is.reactivevalues(analytelist))
  moduleServer(id, function(input, output, session){
   
    ns <- session$ns # to get full namespace here in server function
    analytes = isolate(reactiveValuesToList(analytelist))$analytes
    for (a.name in names(analytes)) {
      # a.name = a$analytename
      prependTab(inputId = "tabs", select = TRUE,
          tabPanel(
            title=a.name, 
            fluidRow(
              column(6,
                     # tags$div(
                       # title="Filter samples by ID",
                       selectizeInput(
                         inputId = ns(paste0("flt_samples",a.name)),#NS(id,paste0("flt_samples",a.name)),
                         label = "Filter Sample IDs",
                         choices = analytelist$analytes[[a.name]]$sample_ids,
                         selected = analytelist$analytes[[a.name]]$sample_filter,
                         multiple = TRUE
                       )
                     # )
              ),
              column(6,
                     numericInput(
                       inputId =ns(paste0("precision",a.name)),
                       label = "Precision",
                       value = 4
                     )
              ),
            ),
          )
      )}
    
    observeEvent(input$tabs,{
      analytelist$selected_tab = input$tabs
    })
    
    observe({
      lapply(names(analytes), function(i){
        analytelist$analytes[[i]]$precision = input[[paste0("precision",i)]]
        analytelist$analytes[[i]]$sample_filter = input[[paste0("flt_samples",i)]]
      })
    })
    
  })
}