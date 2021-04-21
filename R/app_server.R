#' Main Server
#'
#' @param input 
#' @param output 
#' @param session 
#'
#' @return
#' @export
app_server = function(input, output, session) {

  # Upload Controller -------------------------------------------------------

  rv = do.call("reactiveValues",
    list(
      "Certifications" = list("data" = NULL, "uploadsource" = NULL),
      "Homogeneity" = list("data" = NULL, "uploadsource" = NULL),
      "Stability" = list("data" = NULL, "uploadsource" = NULL)
    )
  )
  
  # --- --- --- --- --- --- --- --- ---
  .ExcelUploadControllServer("excelfile", rv)
  # --- --- --- --- --- --- --- --- ---
  
  observeEvent(input$navbarpage, {
    # when Homogeneity is clicked but has no been uploaded yet --> change to
    # Upload page
    if (input$navbarpage == "tP_homogeneity" &&
        is.null(get_listUploadsource(rv, "Homogeneity"))) {
      to_startPage(session)
    } 
    # ... same for Certification ...
    if (input$navbarpage == "tP_certification" &&
        is.null(get_listUploadsource(rv, "Certifications"))) {
      to_startPage(session)
    }
    # ... and Stability
    if (input$navbarpage == "tP_Stability" &&
        is.null(get_listUploadsource(rv, "Stability"))) {
      to_startPage(session)
    }
  })
  
  
  observeEvent(input$link_to_start, {
    to_startPage(session)
  })
  
  # when certification was uploaded
  observeEvent(rv$Certifications,{
    # when source is Excel, switch to Certification Tab automatically
    message("observer: certification was uploaded")
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
  # material table. Because a reactive from another module inside
  # CertificationServer is returned, storing it in reactiveValues() worked so
  # far.
  datreturn = reactiveValues(
    selectedAnalyteDataframe = NULL,    # The dataframe corresp. to the selected analyte
    h_vals = NULL,                      # values from Homogeneity module
    mater_table = NULL,                 # material table, formerly 'cert_vals', READ-ONLY*
    t_H = NULL,                         # when Homogeneity is transferred
    lab_statistics = NULL               # lab statistics (mean,sd) for materialtabelle
  ) 
  
  # * --> All values for material table should be set/written in the designated module
  
  # --- --- --- --- --- --- --- --- ---
  .CertificationServer(id = "certification", d = reactive({rv$Certifications}), datreturn)
  .HomogeneityServer(id = "Homogeneity", rv, datreturn)
  # --- --- --- --- --- --- --- --- ---
  
  # # --- --- --- --- --- --- --- --- --- --- ---
  # # moved to --> CertificationServer
  # .materialtabelleServer(id = "mat_cert", datreturn = datreturn)
  # # --- --- --- --- --- --- --- --- --- --- ---

  
  # observeEvent(datreturn$h_vals, {
    # print(datreturn$h_vals)
    .TransferHomogeneityServer("trH", datreturn)
  # })
  
  
  .longtermstabilityServer("lts")
}

to_startPage = function(session) {
  updateNavbarPage(
    session = session,
    inputId = "navbarpage",
    selected = "Start")
}