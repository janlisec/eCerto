#' Main Server
#'
#' @param input input.
#' @param output output.
#' @param session session.
#'
#' @return the Server
#' @export
app_server = function(input, output, session) {

  rv <- reactiveClass$new(init_rv()) # initiate persistent variables
  datreturn <- reactiveClass$new(init_datreturn()) # initiate runtime variables
  silent = FALSE ## ToDo: make silent a global option of the package like suggested by Golem-package devel

  page_startServer(id="Start", rv=rv)

  shiny::observeEvent(input$navbarpage, {
    # when a tab for an empty dataset is selected --> jump to upload page
    if (input$navbarpage == "tP_homogeneity" && is.null(getValue(rv, c("Homogeneity","uploadsource"))) ) {
      to_startPage(session, value="Homogeneity")
    }
    if (input$navbarpage == "tP_certification" && is.null(getValue(rv, c("Certification","uploadsource"))) ) {
      to_startPage(session, value="Certification")
    }
    if (input$navbarpage == "tP_stability" && is.null(getValue(rv, c("Stability","uploadsource"))) ) {
      to_startPage(session, value="Stability")
    }
  })

# Panels ------------------------------------------------------------------

  page_CertificationServer(
    id = "certification",
    rv = rv,
    datreturn = datreturn
  )

  # Homogeneity Modul
  h_vals <- page_HomogeneityServer(
    id = "Homogeneity",
    homog = shiny::reactive({getValue(rv,"Homogeneity")}),
    cert = shiny::reactive({getValue(rv,"Certification")}),
    datreturn = datreturn
  )

  # Stability Modul
  page_StabilityServer(id = "Stability", rv = rv, datreturn = datreturn)

  # LTS Modul
  .longtermstabilityServer("lts")


# more observers ---------------------------------------------------------------

  # when the user initiates a transfer of U values from H or S Modules --> show material_table
  shiny::observeEvent(getValue(datreturn,"transfer"), {
    shiny::updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "tP_certification")
  })

  # when the user uploaded excel data on S Modul --> change Tab to modified dataset
  shiny::observeEvent(getValue(rv, c("Certification", "input_files")), {
    shiny::updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "tP_certification"
    )
  })
  shiny::observeEvent(getValue(rv, c("Homogeneity", "input_files")), {
    shiny::updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "tP_homogeneity"
    )
  })
  shiny::observeEvent(getValue(rv, c("Stability", "input_files")), {
    shiny::updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "tP_stability"
    )
  })

  shiny::observeEvent(getValue(datreturn, "mater_table"),{
    if (!silent) message("app_server: (datreturn.mater_table) set rv.materialtabelle")
    setValue(rv, "materialtabelle", getValue(datreturn, "mater_table"))
  })

  # After Homogeneity values have been uploaded
  shiny::observeEvent(h_vals(),{
    if(!silent) message("app_server: (h_vals()), set datreturn.h_vals")
    setValue(datreturn, "h_vals", h_vals())
  }, ignoreInit = TRUE)

}