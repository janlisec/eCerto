#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
#'
#' @examples
app_server = function(input, output, session) {

  # Upload Controller -------------------------------------------------------
  upld.cntrller = list(
    "Certifications" = list("data" = NULL, "uploadsource" = NULL),
    "Homogeneity" = list("data" = NULL, "uploadsource" = NULL),
    "Stability" = list("data" = NULL, "uploadsource" = NULL)
  )
  rv = do.call("reactiveValues", upld.cntrller)
  
  .ImportCntrlServer("excelfile", rv)
  
  observeEvent(input$link_to_start, {
    updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "Start")
  })
  
  # when certification was uploaded
  observeEvent(rv$Certifications,{
    # when source is Excel
    if(get_listUploadsource(rv, "Certifications")=="Excel"){
      updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification")
    }

  }, ignoreInit = TRUE)

  .CertificiationServer(id = "certification", d = reactive({rv$Certifications}) )
  
  .longtermstabilityServer("lts")
  
  # TODO hier müssen die Ergebnisse aus rv() verteilt werden!
  # observeEvent(rv(),
  #              switch(xlsxfile()$xlsx_format,
  #                     Certifications = {
  #                       output$cert <- renderPrint(xlsxfile()$xlsx, width = 40)
  #                     }))
  
  
  # output$out = shiny::renderPrint(shiny::reactiveValuesToList(rv), width = 40)
  # output$out = renderPrint(is.reactivevalues(rv()))
}