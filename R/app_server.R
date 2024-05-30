#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # set up new R6 object upon app start
  rv <- eCerto$new(init_rv()) # initiate persistent variables

  # register the R6 object for app testing
  shiny::exportTestValues(
    rv = rv
  )

  # Panels --------------------------------------------------------------------
  # start page
  page_startServer(id = "Start", rv = rv, msession = session)

  # Certification Modul
  page_CertificationServer(id = "certification", rv = rv)

  # Homogeneity Modul
  page_HomogeneityServer(id = "Homogeneity", rv = rv)

  # Stability Modul
  page_StabilityServer(id = "Stability", rv = rv)

  # LTS Modul (will be removed to an independent app at some point)
  m_longtermstabilityServer("lts")

  # Validation Modul
  page_validationServer(id = "Validation")

  # some observers ------------------------------------------------------------
  # mainly to use 'updateNavbarPage' depending on user action -----------------

  # when the user initiates a transfer of U values from H or S Modules --> show material_table
  shiny::observeEvent(getValue(rv, c("General", "materialtabelle")), {
    shiny::updateNavbarPage(session = session, inputId = "navbarpage", selected = "tP_certification")
  })

  # when a tab for an empty data set is selected --> jump to upload page
  shiny::observeEvent(input$navbarpage,
    {
      tP <- switch(input$navbarpage,
        "tP_certification" = "Certification",
        "tP_homogeneity" = "Homogeneity",
        "tP_stability" = "Stability",
        NA
      )
      if (tP %in% getValue(rv, c("modules")) && !rv$e_present()[tP]) to_startPage(session, value = tP)
    },
    ignoreInit = TRUE
  )

  # when the user uploaded excel data for a module --> set focus on this page
  shiny::observeEvent(getValue(rv, c("Certification", "input_files")), {
    shiny::updateNavbarPage(session = session, inputId = "navbarpage", selected = "tP_certification")
  })
  shiny::observeEvent(getValue(rv, c("Stability", "input_files")), {
    shiny::updateNavbarPage(session = session, inputId = "navbarpage", selected = "tP_stability")
  })
  shiny::observeEvent(getValue(rv, c("Homogeneity", "input_files")), {
    shiny::updateNavbarPage(session = session, inputId = "navbarpage", selected = "tP_homogeneity")
  })
}
