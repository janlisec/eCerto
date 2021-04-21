# ANALYTE MODULE----------------------------------------------------

#' Title
#'
#' @param id 
#'
#' @return
#' @export
.analyteModuleUI = function(id){
  # empty tabset panel, to be filled by the analytes in the server Module
  tagList(
    shinyjs::inlineCSS('.selct  {background: green; color: white;border: 5px solid black;}'),
    tabsetPanel(id = NS(id,"tabs"))
  )

}

#' Title
#'
#' @param id 
#' @param apm 
#'
#' @return
#' @export
.analyteModuleServer = function(id, apm) {
  stopifnot(is.reactivevalues(apm))
  moduleServer(id, function(input, output, session){
    ns <- session$ns # to get full namespace here in server function
    analytes = isolate(reactiveValuesToList(apm))$analytes
    # message("analyteModule; all analytes: ", analytes)
    
    # append/prepend a tab for each analyte available
    for (a.name in names(analytes)) {
      appendTab(inputId = "tabs", 
          select = FALSE,
          tabPanel(
            title=a.name, 
            fluidRow(
              column(6,
               selectizeInput(
                 inputId = ns(paste0("flt_samples",a.name)),#NS(id,paste0("flt_samples",a.name)),
                 label = "Filter Sample IDs",
                 choices = apm$analytes[[a.name]]$sample_ids,
                 selected = apm$analytes[[a.name]]$sample_filter,
                 multiple = TRUE
               )
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
      )
      
    }
    # select only first tab
    updateTabsetPanel(session = session, inputId = "tabs", selected = names(analytes)[1])
    
    
    observeEvent(input$tabs,{
      # change color of tab when selected by changing class 
      s = paste0("#",ns("tabs")," li a[data-value=",input$tabs,"]")
      shinyjs::addClass(
        selector = s,
        class = "selct")
      # change the "selected tab" reactive in the reactiveValues when another tab
      # is selected
      apm$selected_tab = input$tabs
    },ignoreInit = TRUE)
    
    # update precision and the selected sample id filter in the reactiveValues
    # when their value change in the selected tab
    observe({
      lapply(names(analytes), function(i){
        apm$analytes[[i]]$precision = input[[paste0("precision",i)]]
        apm$analytes[[i]]$sample_filter = input[[paste0("flt_samples",i)]]
      })
    })
    
  })
}