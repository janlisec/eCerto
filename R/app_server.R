#' Main Server
#'
#' @param input input.
#' @param output output.
#' @param session session.
#'
#' @return the Server
#' @export
app_server = function(input, output, session) {

  
  rv = reactiveClass$new(init_rv()) # initiate persistent variables
  datreturn = reactiveClass$new(init_datreturn()) # initiate runtime variables
  

  # Certification, Homogeneity, Stability -----------------------------------
  excelformat = shiny::reactive({input$moduleSelect})
  shiny::updateSelectInput(inputId = "moduleSelect",
                    session = session,
                    choices = rv$names(),
                    selected = rv$names()[1]
  )


# Upload Controller -------------------------------------------------------

  ExcelUp <- m_ExcelUploadControl_Server(
    id = "excelfile",
    excelformat = excelformat,
    check = shiny::reactive({is.null( get_listUploadsource(rv, excelformat()) )})
  )
  upload_notif = m_RDataImport_Server("Rdata", rv)

# page turners (and more) -------------------------------------------------------------
  
  # when Start Button was clicked
  shiny::observeEvent(input$link_to_start, {
    to_startPage(session, value="Certifications")
  })
  # when RData was uploaded
  shiny::observeEvent(upload_notif(),{
    # message("observer: certifCication was uploaded")
    shiny::updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "tP_certification")
    
  }, ignoreNULL = TRUE)
  # when Excel was uploaded with LOAD-Button...
  shiny::observeEvent(ExcelUp(),{
    message("app_server: Excel Upload, set rv.Data")
    setValue(rv, c(excelformat(),"data"), ExcelUp())
    set_listUploadsource(rv, excelformat(), uploadsource = "Excel")
    if(excelformat() == "Certifications"){
      # message("observer: certification was uploaded")
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
  

# Restart App -------------------------------------------------------------
  # Open confirmation dialog
  observeEvent(input$session_restart, {
    showModal(modalDialog(
      easyClose = FALSE,
      title="Sure you want to restart the session?",
      "This will erase all non-saved inputs!",
      footer = tagList(actionButton("confirmRestart", "Restart"),
                       modalButton("Cancel")
      )
    ))
  })
  observeEvent(input$confirmRestart, {
    session$reload()
    removeModal()
  })

# Panels ------------------------------------------------------------------
  apm = m_CertificationServer(
    id = "certification", 
    certification = shiny::reactive({getValue(rv,c("Certifications"))}), 
    apm.input = shiny::reactive({getValue(rv,c("General","apm"))}),
    datreturn = datreturn
  )
  observeEvent(apm(),{
    # whereami::cat_where("app_server: apm changed, set rv.apm",color = "blue")
    setValue(rv,c("General","apm"), apm()) # getValue(rv,c("General","apm"))
  }, ignoreNULL = TRUE)
  
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
    homogData = shiny::reactive({getValue(datreturn,"h_vals")}),
    matTab_col_code = shiny::reactive({attr(getValue(datreturn,"mater_table"), "col_code")}),
    matTab_analytes = shiny::reactive({as.character(getValue(datreturn,"mater_table")[, "analyte"])})
  )
  # --- --- --- --- --- --- --- --- --- --- ---

  # to Certification page after Transfer of Homogeneity Data
  shiny::observeEvent(trh(),{
    message("app_server: trh() changed, set datreturn.t_H")
    setValue(datreturn,"t_H",trh())
      shiny::updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification")
  })

  # After Homogeneity values have been uploaded
  shiny::observeEvent(h_vals(),{
    message("app_server: h_vals() changed, set datreturn.h_vals")
    setValue(datreturn, "h_vals", h_vals())
  },ignoreInit = TRUE)
  
}
