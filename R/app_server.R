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
                    choices = getValue(rv,"modules"),
                    selected = getValue(rv,"modules")[1]
  )

# Upload Controller -------------------------------------------------------

  ExcelUp <- m_ExcelUpload_Server(
    id = "excelfile",
    excelformat = excelformat,
    check = shiny::reactive({is.null(getValue(rv, c(excelformat(),"uploadsource")) )})
  )
  upload_notif = m_RDataImport_Server("Rdata", rv)

# page turners (and more) -------------------------------------------------------------

  # when Start Button was clicked
  shiny::observeEvent(input$link_to_start, {
    to_startPage(session, value="Certification")
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
    message("app_server: Excel Upload, set rv.Data; set rv.Uploadsource")
    ex_intern = shiny::isolate(excelformat())
    setValue(rv, c(ex_intern,"data"), ExcelUp())
    setValue(rv, c(ex_intern, "uploadsource"),value =  "Excel")
    if(ex_intern == "Certification"){
      # message("observer: certification was uploaded")
      shiny::updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_certification")
    } else if (ex_intern == "Homogeneity") {
      shiny::updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_homogeneity")
    } else if (ex_intern == "Stability") {
      shiny::updateNavbarPage(
        session = session,
        inputId = "navbarpage",
        selected = "tP_Stability")
    }
  })
  shiny::observeEvent(getValue(datreturn,"t_H"),{
    shiny::updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "tP_certification")
  })
  shiny::observeEvent(input$navbarpage, {
    # when Homogeneity is clicked but has no been uploaded yet --> change to
    # Upload page
    if (input$navbarpage == "tP_homogeneity" &&
        is.null(getValue(rv, c("Homogeneity","uploadsource"))) ) {
      to_startPage(session, value="Homogeneity")
    }
    # ... same for Certification ...
    if (input$navbarpage == "tP_certification" &&
        is.null(getValue(rv, c("Certification","uploadsource"))) ) {
      to_startPage(session, value="Certification")
    }
    # ... and Stability
    if (input$navbarpage == "tP_Stability" &&
        is.null(getValue(rv, c("Stability","uploadsource"))) ) {
      to_startPage(session, value="Stability")
    }
  })


# Restart App -------------------------------------------------------------
  # Open confirmation dialog
  shiny::observeEvent(input$session_restart, {
    shiny::showModal(shiny::modalDialog(
      easyClose = FALSE,
      title="Sure you want to restart the session?",
      "This will erase all non-saved inputs!",
      footer = shiny::tagList(
        shiny::actionButton("confirmRestart", "Restart"),
        shiny::modalButton("Cancel")
      )
    ))
  })
  shiny::observeEvent(input$confirmRestart, {
    session$reload()
    shiny::removeModal()
  })

# Panels ------------------------------------------------------------------

   m_CertificationServer(
    id = "certification",
    rv = rv,
    # apm.input = shiny::reactive({getValue(rv,c("General","apm"))}),
    datreturn = datreturn
  )



  # --- --- --- --- --- --- --- --- --- --- ---

  h_vals = m_HomogeneityServer(
    id = "Homogeneity",
    homog = shiny::reactive({getValue(rv,"Homogeneity")}),
    cert = shiny::reactive({getValue(rv,"Certification")}),
    datreturn = datreturn
  )
  m_StabilityServer(id = "Stability", rv = rv, datreturn = datreturn)

  # --- --- --- --- --- --- --- --- --- --- ---
  .longtermstabilityServer("lts")
  # --- --- --- --- --- --- --- --- --- --- ---


  shiny::observeEvent(getValue(datreturn,"mater_table"),{
    message("app_server: datreturn.mater_table changed; set rv.materialtabelle")
    setValue(rv,"materialtabelle", getValue(datreturn,"mater_table"))
  })



  # After Homogeneity values have been uploaded
  shiny::observeEvent(h_vals(),{
    message("app_server: h_vals() changed, set datreturn.h_vals")
    setValue(datreturn, "h_vals", h_vals())
  },ignoreInit = TRUE)

  shiny::observeEvent(input$moduleUploadHelp, {
    switch (excelformat(),
      "Certification" = help_the_user("certification_dataupload", modal=TRUE),
      "Homogeneity" = help_the_user("homogeneity_dataupload", modal=TRUE),
      "Stability" = help_the_user("stability_dataupload", modal=TRUE)
    )

  })

}
