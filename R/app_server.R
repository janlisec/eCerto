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
    print("observer: certification was uploaded")
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
    print("observer: Homogeneity was uploaded")
    if(get_listUploadsource(rv, "Homogeneity")=="Excel"){
      updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_homogeneity")
    }
  }, ignoreInit = TRUE)

  # datreturn contains the by an analyte selected sub-frame for updating the
  # Materialtabelle. Because a reactive from another module inside
  # CertificationServer is returned, storing it in reactiveValues() worked so
  # far.
  datreturn = reactiveValues(
    selectedAnalyteDataframe = NULL,    # The selected Analyte-correspondng df for materialtabelle
    h_vals = NULL,                      # values from Homogeneity module
    mater_table = NULL,                 # materialtabelle, formerly cert_vals
    lab_statistics = NULL               # lab statistics (mean,sd) for materialtabelle
  ) 
  
  # --- --- --- --- --- --- --- --- ---
  .CertificiationServer(id = "certification", d = reactive({rv$Certifications}), datreturn)
  .HomogeneityServer(id = "Homogeneity", rv, datreturn)
  # --- --- --- --- --- --- --- --- ---
  
  # --- --- --- --- --- --- --- --- --- --- ---
  .materialtabelleServer(id = "mat_cert", datreturn = datreturn)
  # --- --- --- --- --- --- --- --- --- --- ---

  
  # observeEvent(datreturn$h_vals, {
    # print(datreturn$h_vals)
    .TransferHomogeneityServer("trH", datreturn)
  # })
  
  
  .longtermstabilityServer("lts")
}