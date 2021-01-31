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
server = function(input, output, session) {

  # Upload Controller -------------------------------------------------------
  upld.cntrller = list(
    "Certifications" = NULL,
    "Homogeneity" = NULL,
    "Stability" = NULL
  )
  rv = do.call("reactiveValues", upld.cntrller)
  xlsxload_ImportCntrlServer("excelfile", rv)
  
  
  observeEvent(input$link_to_start, {
    updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "Start")
  })
  
  # when certification was uploaded
  # TODO what to do when RData is uploaded?
  observeEvent(rv$Certifications,{
    print("Cert was uploaded")
    updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "tP_certification")
  })

  .CertificiationServer(id = "certification", d = reactive({rv$Certifications}) )
  
  # TODO hier müssen die Ergebnisse aus rv() verteilt werden!
  # observeEvent(rv(),
  #              switch(xlsxfile()$xlsx_format,
  #                     Certifications = {
  #                       output$cert <- renderPrint(xlsxfile()$xlsx, width = 40)
  #                     }))
  
  
  # output$out = shiny::renderPrint(shiny::reactiveValuesToList(rv), width = 40)
  # output$out = renderPrint(is.reactivevalues(rv()))
}