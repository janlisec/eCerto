#' Main Server
#'
#' @param input input.
#' @param output output.
#' @param session session.
#'
#' @return
#' @export
app_server = function(input, output, session) {

  # Upload Controller -------------------------------------------------------
  
  rv = init_rv()
  updateSelectInput(inputId = "moduleSelect",
                    session = session,
                    choices =  shiny::isolate(names(rv)),
                    selected = shiny::isolate(names(rv))[1])

  excelformat = reactive({input$moduleSelect})
  # --- --- --- --- --- --- --- --- ---
   t = m_ExcelUploadControl_Server("excelfile", excelformat, check = reactive({is.null(get_listelem(rv,excelformat()))}))
  # --- --- --- --- --- --- --- --- ---

  observeEvent(t(),{
    set_listelem(rv, excelformat(), t)
    set_listUploadsource(rv, excelformat(), uploadsource = "Excel")
  })

  # --- --- --- --- --- --- --- --- ---
  .RDataImport_Server("Rdata", rv)
  # --- --- --- --- --- --- --- --- ---

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


  observeEvent(input$link_to_start, {
    to_startPage(session, value="Certifications")
  })

  # when certification was uploaded
  observeEvent(rv$Certifications,{
    # when source is Excel, switch to Certification Tab automatically

    if(get_listUploadsource(rv, "Certifications")=="Excel"){
      message("observer: certification was uploaded")
      updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification")
    }
    if(get_listUploadsource(rv, "Certifications")=="RData"){
      # message("observer: certification was uploaded")
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
  datreturn = init_datreturn()
  

  # * --> All values for material table should be set/written in the designated module

  
  # --- --- --- --- --- --- --- --- --- --- ---

  m_CertificationServer(id = "certification", certification = reactive({rv$Certifications}), datreturn)
  # --- --- --- --- --- --- --- --- --- --- ---
  .HomogeneityServer(id = "Homogeneity", rv, datreturn)

  # --- --- --- --- --- --- --- --- --- --- ---

  trh = m_TransferHomogeneityServer(
    id = "trH",
    homogData = reactive({datreturn$h_vals}),
    matTab_col_code = reactive({attr(datreturn$mater_table, "col_code")}),
    matTab_analytes = reactive({as.character(datreturn$mater_table[, "analyte"])})
  )
  # --- --- --- --- --- --- --- --- --- --- ---

  # get to Certification page after Transfer of Homogeneity Data
  observeEvent(trh(),{
    browser()
    datreturn$t_H = trh()
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