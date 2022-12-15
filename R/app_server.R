#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  # set up new R6 object upon app start
  rv <- eCerto$new(init_rv()) # initiate persistent variables

  # register the R6 object for app testing
  shiny::exportTestValues(
    rv = rv
  )

  # Panels --------------------------------------------------------------------
  # start page
  page_startServer(id="Start", rv=rv)

  # Certification Modul
  page_CertificationServer(id = "certification", rv = rv)

  # Homogeneity Modul
  page_HomogeneityServer(id = "Homogeneity", rv = rv)

  # Stability Modul
  page_StabilityServer(id = "Stability", rv = rv)

  # LTS Modul (will be removed to an independent app at some point)
  m_longtermstabilityServer("lts")

  # some observers ------------------------------------------------------------
  # mainly to use 'updateNavbarPage' depending on user action -----------------

  # when the user initiates a transfer of U values from H or S Modules --> show material_table
  shiny::observeEvent(getValue(rv, c("General", "materialtabelle")), {
    shiny::updateNavbarPage(
      session = session,
      inputId = "navbarpage",
      selected = "tP_certification")
  })

  # when a tab for an empty dataset is selected --> jump to upload page
  shiny::observeEvent(input$navbarpage, {
    if (input$navbarpage == "tP_homogeneity" && is.null(getValue(rv, c("Homogeneity","uploadsource")))) {
      to_startPage(session, value="Homogeneity")
    }
    if (input$navbarpage == "tP_certification" && is.null(getValue(rv, c("Certification","uploadsource")))) {
      to_startPage(session, value="Certification")
    }
    if (input$navbarpage == "tP_stability" && is.null(getValue(rv, c("Stability","uploadsource")))) {
      to_startPage(session, value="Stability")
    }
  })

  # when the user uploaded excel data for a module --> set focus on this page
  shiny::observeEvent(getValue(rv, c("Certification", "input_files")), {
    shiny::updateNavbarPage(session = session, inputId = "navbarpage", selected = "tP_certification")
  })
  shiny::observeEvent(getValue(rv, c("Homogeneity", "input_files")), {
    shiny::updateNavbarPage(session = session, inputId = "navbarpage", selected = "tP_homogeneity")
  })
  shiny::observeEvent(getValue(rv, c("Stability", "input_files")), {
    shiny::updateNavbarPage(session = session, inputId = "navbarpage", selected = "tP_stability")
  })

}
