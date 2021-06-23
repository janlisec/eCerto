#' Main Server
#'
#' @param input input.
#' @param output output.
#' @param session session.
#'
#' @return the Server
#' @export
app_server = function(input, output, session) {

  
  rv = ecerto::reactiveClass$new(init_rv()) # initiate persistent variables
  datreturn = ecerto::reactiveClass$new(init_datreturn()) # initiate runtime variables
  

  # Certification, Homogeneity, Stability -----------------------------------
  excelformat = shiny::reactive({input$moduleSelect})
  shiny::updateSelectInput(inputId = "moduleSelect",
                    session = session,
                    choices = rv$names(),
                    selected = rv$names()[1]
  )


  # Upload Controller -------------------------------------------------------

  t <- m_ExcelUploadControl_Server(
    id = "excelfile",
    excelformat = excelformat,
    check = shiny::reactive({is.null( ecerto::get_listUploadsource(rv, excelformat()) )})
  )
  upload_noti = ecerto::m_RDataImport_Server("Rdata", rv)

  # page turners -------------------------------------------------------------
  
  # when Start Button was clicked
  shiny::observeEvent(input$link_to_start, {
    to_startPage(session, value="Certifications")
  })
  # when RData was uploaded
  shiny::observeEvent(upload_noti(),{
    message("observer: certification was uploaded")
    shiny::updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "tP_certification")
    
  })
  # when Excel was uploaded...
  shiny::observeEvent(t(),{
    ecerto::setValue(rv, c(excelformat(),"data"), t())
    ecerto::set_listUploadsource(rv, excelformat(), uploadsource = "Excel")
    if(excelformat() == "Certifications"){
      message("observer: certification was uploaded")
      shiny::updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification")
    } else if (excelformat() == "Homogeneity") {
      shiny::updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_homogeneity")
    } else if (excelformat() == "Stability") {
      
    }
  })
  shiny::observeEvent(input$navbarpage, {
    # when Homogeneity is clicked but has no been uploaded yet --> change to
    # Upload page
    if (input$navbarpage == "tP_homogeneity" &&
        is.null(ecerto::get_listUploadsource(rv, "Homogeneity"))) {
      to_startPage(session, value="Homogeneity")
    }
    # ... same for Certification ...
    if (input$navbarpage == "tP_certification" &&
        is.null(ecerto::get_listUploadsource(rv, "Certifications"))) {
      to_startPage(session, value="Certifications")
    }
    # ... and Stability
    if (input$navbarpage == "tP_Stability" &&
        is.null(ecerto::get_listUploadsource(rv, "Stability"))) {
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
  ecerto::m_CertificationServer(id = "certification", certification = shiny::reactive({getValue(rv,"Certifications")}), datreturn)
  # --- --- --- --- --- --- --- --- --- --- ---
  h_vals = ecerto::m_HomogeneityServer(
    id = "Homogeneity",
    homog = shiny::reactive({getValue(rv,"Homogeneity")}),
    cert = shiny::reactive({getValue(rv,"Certifications")})
  )


  # --- --- --- --- --- --- --- --- --- --- ---
  ecerto::.longtermstabilityServer("lts")
  # --- --- --- --- --- --- --- --- --- --- ---

  trh = ecerto::m_TransferHomogeneityServer(
    id = "trH",
    homogData = shiny::reactive({ecerto::getValue(datreturn,"h_vals")}),
    matTab_col_code = shiny::reactive({attr(ecerto::getValue(datreturn,"mater_table"), "col_code")}),
    matTab_analytes = shiny::reactive({as.character(ecerto::getValue(datreturn,"mater_table")[, "analyte"])})
  )
  # --- --- --- --- --- --- --- --- --- --- ---

  # to Certification page after Transfer of Homogeneity Data
  shiny::observeEvent(trh(),{
    ecerto::setValue(datreturn,"t_H",trh())
      shiny::updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification")
  })

  # After Homogeneity values have been uploaded
  shiny::observeEvent(h_vals(),{
    print("m_HomogeneityServer - h_vals added")
    ecerto::setValue(datreturn, "h_vals", h_vals())
  },ignoreInit = TRUE)
  
}
