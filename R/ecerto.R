#'@title ecerto.
#'@description Starts the ecerto app.
#'@return A shinyApp.
#'@export
ecerto <- function(...) {

  # some parameters which are set in global.R for online app but here for package version
  fun_args <- as.list(match.call())
  if (is.null(fun_args$status_line)) {
    status_line <- paste0("ver ", utils::packageVersion("ecerto"), " (", utils::packageDate("ecerto"), ") jan.lisec@bam.de")
    if (file.exists("inst/app/www")) shiny::addResourcePath(prefix = "ecerto", directoryPath = "inst/app/www")
  } else {
    status_line <- fun_args$status_line
    if (file.exists("www")) shiny::addResourcePath(prefix = 'ecerto', directoryPath = 'www')
  }

  app_ui <- function() {

    shiny::tagList(
      # tagList to make useShinyjs independent from tabs
      shinyjs::useShinyjs(),
      shinyalert::useShinyalert(),

      shiny::navbarPage(
        id = "navbarpage",
        title = shiny::div(
          class = "verticalhorizontal",
          shiny::img(
            #src = fnc_get_local_file("bam_logo_20pt.gif", copy_to_tempdir = FALSE),
            src = "ecerto/bam_logo_20pt.gif",
            #src = "www/bam_logo_20pt.gif",
            #src = "bam_logo_20pt.gif",
            position = "absolute",
            margin = "auto",
            alt="BAM Logo"
          ),
          shiny::strong("BAM"),
          shiny::em("eCerto"),
          position="relative"
        ),
        selected = "Start",
        windowTitle = "BAM eCerto",
        position = "static-top",
        footer = shiny::pre(status_line),
        shiny::tabPanel(
          id = "start",
          title = "Start",
          icon = shiny::icon("angle-right"),
          page_startUI("Start")
        ),
        shiny::tabPanel(
          id = "certif_tab",
          title = "Certification",
          value = "tP_certification",
          icon = shiny::icon("angle-right"),
          page_CertificationUI("certification"),
        ),
        shiny::tabPanel(
          id = "homog_tab",
          title = "Homogeneity",
          icon = shiny::icon("angle-right"),
          value = "tP_homogeneity",
          page_HomogeneityUI("Homogeneity")
        ),
        shiny::tabPanel(
          title = "Stability",
          icon = shiny::icon("angle-right"),
          value = "tP_stability",
          page_StabilityUI("Stability")
        ),
        # Long term stability
        shiny::tabPanel(
          title = "LTS",
          icon = shiny::icon("angle-right"),
          value = "tP_LTS",
          .longtermstabilityUI("lts")
        ),
        shiny::tabPanel(
          title = "Help",
          icon = shiny::icon("angle-right"),
          value = "tP_help",
          help_the_user(filename = "help_start", format = "html", modal = FALSE)
        )
      )
    )
  }

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

    # some observers, mainly to use 'updateNavbarPage' depending on user action
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

  # start app with app ui and server
  shiny::shinyApp(ui = app_ui(), server = app_server)

}