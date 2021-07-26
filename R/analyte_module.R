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
#' @param tablist list of current tabs (temporarily, see https://git.bam.de/fkress/ecerto/-/issues/46)
#'
#' @return the currently selected tab and Other parameter via apm reactiveValues()
#'
#' @rdname analyteModule
#' @export
#' @examples
#' if (interactive()) {
#' apm <- shiny::reactiveVal()
#' df <- data.frame("analyte"=gl(n = 2, k = 10, labels = c("A1","A2")))
#' apm(analyte_parameter_list(df))
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'  shinyjs::useShinyjs(),
#'    m_analyteModuleUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    m_analyteServer(
#'      id = "test",
#'      apm = apm,
#'      renewTab = reactiveVal(1),
#'      tablist = reactiveVal()
#'    )
#'  }
#' )
#' }
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

    selected_tab = shiny::eventReactive(input$tabs,{
      input$tabs
    })

    confirmedTabs = reactiveVal()

    shiny::observeEvent(renewTabs(),{
      message("analyte_module: Renew Tabs")
      tablist() %>% purrr::walk(~shiny::removeTab("tabs", .x)) # remove old tabs
      tablist(NULL)

      # append/prepend a tab for each analyte available
      for (a.name in names(isolate(apm()))) {
        message("append Tab: ", a.name)
        tablist_tmp <- c(tablist(), a.name) # add to tablist for removing later
        tablist(tablist_tmp)
        shiny::appendTab(
          inputId = "tabs",
          select = FALSE,
          shiny::tabPanel(
            title=a.name,
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::selectizeInput(
                  inputId = ns(paste0("flt_samples",a.name)),#NS(id,paste0("flt_samples",a.name)),
                  label = "Filter Sample IDs",
                  choices = isolate(apm())[[a.name]]$sample_ids,
                  selected = isolate(apm())[[a.name]]$sample_filter,
                  multiple = TRUE
                )
              ),
              shiny::column(
                width = 6,
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
      # select only first tab after tabs-creation
      firstTab = names(isolate(apm()))[1]
      shiny::updateTabsetPanel(
        session = session,
        inputId = "tabs",
        selected =firstTab
      )
      # set first selected Tab on TRUE
      analytes_tmp = isolate(apm())
      analytes_tmp[[firstTab]]$confirmed = TRUE
      apm(analytes_tmp)

      # Make confirmed Tabs (geht auch bestimmt schöner mit map() oder so)
      l = c()
      for (i in isolate(apm())) {
        if(i$confirmed == TRUE) {
          l = append(l,i$analytename)
          markConfirmed(i$analytename)
        }
      }
      confirmedTabs(l)
      renewTabs(NULL) # reset variable to NULL for next renew-command
    }, ignoreNULL = TRUE)

    # change color of tab when selected by changing class
    markConfirmed = function(tab) {
      message("color tab: ", tab)
      # s = paste0("#",ns("tabs")," li a[data-value=",tab,"]")
      s = paste0(" li a[data-value=",tab,"]")
      shinyjs::addClass(
        selector = s,
        class = "selct")
    }

    observeEvent(confirmedTabs(),{

      for (i in confirmedTabs()) {
        markConfirmed(i)
      }
    })


    shiny::observeEvent(selected_tab(),{
      if(!selected_tab() %in% confirmedTabs()) {
        ct = confirmedTabs()
        ct = append(ct,selected_tab())
        confirmedTabs(ct)
      }
      analytes_tmp = isolate(apm())
      analytes_tmp[[selected_tab()]]$confirmed = TRUE
      apm(analytes_tmp)
    },ignoreInit = TRUE, ignoreNULL = TRUE)



    # update precision
    shiny::observe({
      req(selected_tab())
      message("analyte_module: Precision change")
      analytes_tmp = isolate(apm())
      if(!is.null(input[[paste0("precision",selected_tab())]]))
        analytes_tmp[[selected_tab()]]$precision = input[[paste0("precision",selected_tab())]]
      apm(analytes_tmp)
    })

    # update flt_samples (the sample filter)
    shiny::observe({
      req(selected_tab())
      message("analyte_module: flt_samples change")
      analytes_tmp = isolate(apm())
      if(!is.null(input[[paste0("flt_samples",selected_tab())]]))
        analytes_tmp[[selected_tab()]]$sample_filter = input[[paste0("flt_samples",selected_tab())]]
      apm(analytes_tmp)
    })

    return(selected_tab) # module returns currently selected analyte-tab
  })
}