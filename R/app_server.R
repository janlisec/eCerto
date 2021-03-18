#' Title
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
app_server = function(input, output, session) {

  # Upload Controller -------------------------------------------------------
  upld.cntrller = list(
    "Certifications" = list("data" = NULL, "uploadsource" = NULL),
    "Homogeneity" = list("data" = NULL, "uploadsource" = NULL),
    "Stability" = list("data" = NULL, "uploadsource" = NULL)
  )
  rv = do.call("reactiveValues", upld.cntrller)
  
  # --- --- --- --- --- --- --- --- ---
  .ImportCntrlServer("excelfile", rv)
  # --- --- --- --- --- --- --- --- ---
  
  observeEvent(input$link_to_start, {
    updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "Start")
  })
  
  # when certification was uploaded
  observeEvent(rv$Certifications,{
    # when source is Excel, switch to Certification Tab automatically
    if(get_listUploadsource(rv, "Certifications")=="Excel"){
      updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification")
    }
  }, ignoreInit = TRUE)
  
  # when Homogeneity was uploaded
  observeEvent(rv$Homogeneity,{
    # when source is Excel, switch to Homogeneity Tab automatically
    if(get_listUploadsource(rv, "Homogeneity")=="Excel"){
      updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_homogeneity")
    }
  }, ignoreInit = TRUE)

  # datreturn contains the selected sub-frame for updating the Materialtabelle
  # when another analyte is selected. Because a reactive from another module
  # inside .CertificationServer is returned, storing it in reactiveValues()
  # worked so far.
  datreturn = reactiveValues(dat = NULL) 
  
  # --- --- --- --- --- --- --- --- ---
  .CertificiationServer(id = "certification", d = reactive({rv$Certifications}), datreturn)
  .HomogeneityServer(id = "Homogeneity", rv)
  # --- --- --- --- --- --- --- --- ---
  
  # datreturn$dat hoffentlich nur temporär
  observeEvent(datreturn$dat,{
    # --- --- --- --- --- --- --- --- --- --- ---
    .materialtabelleServer("mat_cert", reactive(datreturn$dat))
    # --- --- --- --- --- --- --- --- --- --- ---
  }, ignoreNULL = TRUE)
  
  
  .longtermstabilityServer("lts")
}