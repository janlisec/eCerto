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
#' @param renewTabs The order, to delete and renew Tabs, for example when new data is uploaded
#' @param tablist list of currently set tab (temporarily, see https://git.bam.de/fkress/ecerto/-/issues/46)
#'
#' @return the currently selected tab and Other parameter via apm reactiveValues()
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
m_analyteServer = function(id, apm, renewTabs, tablist) {
  stopifnot(shiny::is.reactive(apm))
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns # to get full namespace here in server function
    # analytes = apm
    

    shiny::observeEvent(renewTabs(),{
      message("analyte_module: Renew Tabs")
      tablist() %>% purrr::walk(~shiny::removeTab("tabs", .x)) # remove old tabs
      tablist(NULL)
      # append/prepend a tab for each analyte available
      for (a.name in names(isolate(apm()))) {
        tablist_tmp <- c(tablist(), a.name) # add to tablist for removing later
        tablist(tablist_tmp)
        shiny::appendTab(inputId = "tabs",
                         select = FALSE,
                          shiny::tabPanel(
                           title=a.name,
                           shiny::fluidRow(
                             shiny::column(6,
                                           shiny::selectizeInput(
                                             inputId = ns(paste0("flt_samples",a.name)),#NS(id,paste0("flt_samples",a.name)),
                                             label = "Filter Sample IDs",
                                             choices = isolate(apm())[[a.name]]$sample_ids,
                                             selected = isolate(apm())[[a.name]]$sample_filter,
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
      shiny::updateTabsetPanel(
        session = session, 
        inputId = "tabs", 
        selected = names(isolate(apm()))[1]
      )
      renewTabs(NULL)
      # message("analyte_module: Update Precision and sample filter")
      # lapply(names(isolate(apm())), function(i){
      #   analytes_tmp = isolate(apm())
      #   if(!is.null(input[[paste0("precision",i)]]))
      #     analytes_tmp[[i]]$precision = input[[paste0("precision",i)]]
      #   if(!is.null(input[[paste0("flt_samples",i)]]))
      #     analytes_tmp[[i]]$sample_filter = input[[paste0("flt_samples",i)]]
      #   apm(analytes_tmp)
      # })
    }, ignoreNULL = TRUE)
    

    shiny::observeEvent(input$tabs,{
      # change color of tab when selected by changing class
      s = paste0("#",ns("tabs")," li a[data-value=",input$tabs,"]")
      shinyjs::addClass(
        selector = s,
        class = "selct")

    },ignoreInit = TRUE)

    selected_tab = shiny::eventReactive(input$tabs,{
      input$tabs
    })
    
    # update precision 
    shiny::observeEvent(input[[paste0("precision",selected_tab())]],{
      message("analyte_module: Precision change")
      analytes_tmp = isolate(apm())
      if(!is.null(input[[paste0("precision",selected_tab())]]))
        analytes_tmp[[selected_tab()]]$precision = input[[paste0("precision",selected_tab())]]
      apm(analytes_tmp)
    })
    # update flt_samples 
    shiny::observeEvent(input[[paste0("flt_samples",selected_tab())]],{
      message("analyte_module: flt_samples change")
      analytes_tmp = isolate(apm())
      if(!is.null(input[[paste0("flt_samples",selected_tab())]]))
        analytes_tmp[[selected_tab()]]$sample_filter = input[[paste0("flt_samples",selected_tab())]]
      apm(analytes_tmp)
    })
    # and the selected sample id filter in the reactiveValues
    # when their value change in the selected tab
    # shiny::observeEvent(selected_tab(),{
    #   message("analyte_module: Update Precision and sample filter")
    #   lapply(names(isolate(apm())), function(i){
    #     analytes_tmp = isolate(apm())
    #     if(!is.null(input[[paste0("precision",i)]]))
    #       analytes_tmp[[i]]$precision = input[[paste0("precision",i)]]
    #     if(!is.null(input[[paste0("flt_samples",i)]]))
    #       analytes_tmp[[i]]$sample_filter = input[[paste0("flt_samples",i)]]
    #     apm(analytes_tmp)
    #   })
    # }, ignoreNULL = TRUE)

    # observe({
    #   message("analyte_module: apm changed")
    #   apm(analytes())
    # }) # update apm
    return(selected_tab)
  })
}