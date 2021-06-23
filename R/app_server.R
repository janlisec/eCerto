#' Main Server
#'
#' @param input input.
#' @param output output.
#' @param session session.
#'
#' @return
#' @export
app_server = function(input, output, session) {

  
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
  
  
  # Upload Controller -------------------------------------------------------


  rv = reactiveClass$new(init_rv())

  updateSelectInput(inputId = "moduleSelect",
                    session = session,
                    choices = rv$names(),
                    selected = rv$names()[1]
  )

  excelformat = reactive({input$moduleSelect})
  # --- --- --- --- --- --- --- --- ---
  t <- m_ExcelUploadControl_Server(
    id = "excelfile",
    excelformat = excelformat,
    check = reactive({is.null( get_listUploadsource(rv, excelformat()) )})
  )
 # --- --- --- --- --- --- --- --- ---
  observeEvent(excelformat(),{
  },ignoreInit = TRUE)

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
  # --- --- --- --- --- --- --- --- ---
  upload_noti = m_RDataImport_Server("Rdata", rv)
  # when Rdata was uploaded
  observeEvent(upload_noti(),{
      message("observer: certification was uploaded")
      updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification")
    
  })
  # # --- --- --- --- --- --- --- --- ---

  observeEvent(input$link_to_start, {
    to_startPage(session, value="Certifications")
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

  # datreturn contains the by an analyte selected sub-frame for updating the
  # material table. Because a reactive from another module inside
  # CertificationServer is returned, storing it in reactiveValues() worked so
  # far.
  datreturn = reactiveClass$new(init_datreturn())


  # * --> All values for material table should be set/written in the designated module


  # --- --- --- --- --- --- --- --- --- --- ---

  m_CertificationServer(id = "certification", certification = reactive({getValue(rv,"Certifications")}), datreturn)
  # --- --- --- --- --- --- --- --- --- --- ---
  h_vals = m_HomogeneityServer(
    id = "Homogeneity",
    homog = shiny::reactive({getValue(rv,"Homogeneity")}),
    cert = shiny::reactive({getValue(rv,"Certifications")})
  )
  shiny::observeEvent(h_vals(),{
    print("m_HomogeneityServer - h_vals added")
    setValue(datreturn, "h_vals", h_vals())
  },ignoreInit = TRUE)

  # --- --- --- --- --- --- --- --- --- --- ---

  trh = m_TransferHomogeneityServer(
    id = "trH",
    homogData = reactive({getValue(datreturn,"h_vals")}),
    matTab_col_code = reactive({attr(getValue(datreturn,"mater_table"), "col_code")}),
    matTab_analytes = reactive({as.character(getValue(datreturn,"mater_table")[, "analyte"])})
  )
  # --- --- --- --- --- --- --- --- --- --- ---

  # get to Certification page after Transfer of Homogeneity Data
  observeEvent(trh(),{
    setValue(datreturn,"t_H",trh())
      updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification")
  })

  .longtermstabilityServer("lts")
}

to_startPage = function(session, value="Certification") {
  updateNavbarPage(
    session = session,
    inputId = "navbarpage",
    selected = "Start"
  )
  updateSelectInput(
    session = session,
    inputId = "moduleSelect",
    selected = value
  )
}