#' Main Server
#'
#' @param input input.
#' @param output output.
#' @param session session.
#'
#' @return
#' @export
app_server = function(input, output, session) {

  
  rv = reactiveClass$new(init_rv()) # initiate persistent variables
  datreturn = reactiveClass$new(init_datreturn()) # initiate runtime variables
  

  # Certification, Homogeneity, Stability -----------------------------------
  excelformat = reactive({input$moduleSelect})
  updateSelectInput(inputId = "moduleSelect",
                    session = session,
                    choices = rv$names(),
                    selected = rv$names()[1]
  )


  # Upload Controller -------------------------------------------------------

  t <- m_ExcelUploadControl_Server(
    id = "excelfile",
    excelformat = excelformat,
    check = reactive({is.null( get_listUploadsource(rv, excelformat()) )})
  )
  upload_noti = m_RDataImport_Server("Rdata", rv)

  # page turners -------------------------------------------------------------
  
  # when Start Button was clicked
  observeEvent(input$link_to_start, {
    to_startPage(session, value="Certifications")
  })
  # when RData was uploaded
  observeEvent(upload_noti(),{
    message("observer: certification was uploaded")
    updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "tP_certification")
    
  })
  # when Excel was uploaded...
  observeEvent(t(),{
    setValue(rv, c(excelformat(),"data"), t())
    set_listUploadsource(rv, excelformat(), uploadsource = "Excel")
    if(excelformat() == "Certifications"){
      message("observer: certification was uploaded")
      updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification")
    } else if (excelformat() == "Homogeneity") {
      updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_homogeneity")
    } else if (excelformat() == "Stability") {
      
    }
  })
  observeEvent(input$navbarpage, {
    # when Homogeneity is clicked but has no been uploaded yet --> change to
    # Upload page
    if (input$navbarpage == "tP_homogeneity" &&
        is.null(get_listUploadsource(rv, "Homogeneity"))) {
      to_startPage(session, value="Homogeneity")
    }
    # ... same for Certification ...
    if (input$navbarpage == "tP_certification" &&
        is.null(get_listUploadsource(rv, "Certifications"))) {
      to_startPage(session, value="Certifications")
    }
    # ... and Stability
    if (input$navbarpage == "tP_Stability" &&
        is.null(get_listUploadsource(rv, "Stability"))) {
      to_startPage(session, value="Stability")
    }
  })

  # # when certification was uploaded
  # # observeEvent(getValue(rv,c("Certifications","data")),{
  # observeEvent(get_listUploadsource(rv, "Certifications"), {
  #   # when source is Excel, switch to Certification Tab automatically
  #   if(get_listUploadsource(rv, "Certifications")=="Excel"){
  #     message("observer: certification was uploaded")
  #     updateNavbarPage(
  #       session = session,
  #       inputId = "navbarpage",
  #       selected = "tP_certification")
  #   }
  #   if(get_listUploadsource(rv, "Certifications")=="RData"){
  #     # message("observer: certification was uploaded")
  #     updateNavbarPage(
  #       session = session,
  #       inputId = "navbarpage",
  #       selected = "tP_certification")
  #   }
  # }, ignoreInit = TRUE, once = TRUE)

  # # when Homogeneity was uploaded
  # # observeEvent(getValue(rv,c("Homogeneity","data")),{
  # observeEvent(get_listUploadsource(rv, "Homogeneity"),{
  #   # when source is Excel, switch to Homogeneity Tab automatically
  #   message("app_server: observeEvent(getValue(rv,Homogeneity)): Homogeneity was uploaded")
  #   if (get_listUploadsource(rv, "Homogeneity")=="Excel") {
  #     updateNavbarPage(
  #       session = session,
  #       inputId = "navbarpage",
  #       selected = "tP_homogeneity")
  #   }
  # 
  # }, ignoreInit = TRUE, ignoreNULL = TRUE)




  # * --> All values for material table should be set/written in the designated module


# Panels ------------------------------------------------------------------
  m_CertificationServer(id = "certification", certification = reactive({getValue(rv,"Certifications")}), datreturn)
  # --- --- --- --- --- --- --- --- --- --- ---
  h_vals = m_HomogeneityServer(
    id = "Homogeneity",
    homog = shiny::reactive({getValue(rv,"Homogeneity")}),
    cert = shiny::reactive({getValue(rv,"Certifications")})
  )


  # --- --- --- --- --- --- --- --- --- --- ---
  .longtermstabilityServer("lts")
  # --- --- --- --- --- --- --- --- --- --- ---

  trh = m_TransferHomogeneityServer(
    id = "trH",
    homogData = reactive({getValue(datreturn,"h_vals")}),
    matTab_col_code = reactive({attr(getValue(datreturn,"mater_table"), "col_code")}),
    matTab_analytes = reactive({as.character(getValue(datreturn,"mater_table")[, "analyte"])})
  )
  # --- --- --- --- --- --- --- --- --- --- ---

  # to Certification page after Transfer of Homogeneity Data
  observeEvent(trh(),{
    setValue(datreturn,"t_H",trh())
      updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification")
  })

  # After Homogeneity values have been uploaded
  shiny::observeEvent(h_vals(),{
    print("m_HomogeneityServer - h_vals added")
    setValue(datreturn, "h_vals", h_vals())
  },ignoreInit = TRUE)
  
}
