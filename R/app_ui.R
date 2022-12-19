#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # the following lines can be used to check for problems with the 'www' folder on different App places
    #message("UI, 'www': ", shiny::resourcePaths()["www"]),
    #message("UI, app_sys: ", app_sys('app/www')),
    #message("UI, tempdir: ", tempdir()),

    shiny::navbarPage(
      # use an alternative theme
      # currently difficult, as 'hidden' feature of tabPanel is not supported by bslib
      # theme = bslib::bs_theme(
      #   version = 3,
      #   bootswatch = "readable",
      #   base_font = bslib::font_google(c("Assistant", "Anonymous Pro")[1])
      # ),
      id = "navbarpage",
      title = shiny::div(
        style = "position: relative;",
        class = "verticalhorizontal",
        shiny::img(src = "www/bam_logo_20pt.gif", position = "absolute", margin = "auto", alt="BAM Logo"),
        shiny::strong("BAM"),
        shiny::em(get_golem_config("golem_name"))
      ),
      selected = "Start",
      windowTitle = paste("BAM", get_golem_config("golem_name")),
      position = "fixed-top",
      footer = shiny::div(
        style="position: fixed; bottom: 0px; left: 0px; width: 100%; padding-left: 15px; padding-top: 5px; padding-bottom: 5px; background-color: #f8f8f8; font-family: Lucida Console, monospace;",
        #shiny::pre(
          shiny::HTML(
            get_golem_config("golem_name"), "|",
            get_golem_config("app_version"), "|",
            get_golem_config("app_date"), "|",
            '<a href="mailto:jan.lisec@bam.de">jan.lisec@bam.de</a>',
            ifelse(get_golem_config("bam_server"), '| <a href="https://www.bam.de/Navigation/DE/Services/Datenschutz/datenschutz.html" target="_blank" rel="noopener noreferrer">BAM Datenschutzerkl\u00e4rung</a>', '')
          )
        #)
      ),
      shiny::tabPanel(
        id = "start",
        title = "Start",
        icon = shiny::icon("angle-right"),
        shiny::div(style="padding-top: 60px;", page_startUI("Start"))
      ),
      shiny::tabPanel(
        id = "certif_tab",
        title = "Certification",
        value = "tP_certification",
        icon = shiny::icon("angle-right"),
        shiny::div(style="padding-top: 60px;", page_CertificationUI("certification"))
      ),
      shiny::tabPanel(
        id = "homog_tab",
        title = "Homogeneity",
        icon = shiny::icon("angle-right"),
        value = "tP_homogeneity",
        shiny::div(style="padding-top: 60px;", page_HomogeneityUI("Homogeneity"))
      ),
      shiny::tabPanel(
        title = "Stability",
        icon = shiny::icon("angle-right"),
        value = "tP_stability",
        shiny::div(style="padding-top: 60px;", page_StabilityUI("Stability"))
      ),
      # Long term stability
      shiny::tabPanel(
        title = "LTS",
        icon = shiny::icon("angle-right"),
        value = "tP_LTS",
        shiny::div(style="padding-top: 60px;", m_longtermstabilityUI("lts"))
      ),
      shiny::tabPanel(
        title = "Help",
        icon = shiny::icon("angle-right"),
        value = "tP_help",
        shiny::div(style="padding-top: 60px; float: left", shiny::withMathJax(shiny::includeCSS(path = get_local_file("help_start.html"))))
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @importFrom shinyjs useShinyjs
#' @noRd
golem_add_external_resources <- function() {

  # copy www files from app_sys() to tempdir() and map app/www folder to this temp/www
  # this is required for Report rendering to work on a ShinyServer where writing
  # permission is only granted in a temp dir
  file.copy(from = app_sys('app/www'), to = tempdir(), recursive = TRUE)
  golem::add_resource_path(
    'www', paste(normalizePath(tempdir(), "/"), "www", sep="/")
  )

  # add further resources to the <head> of the HTML page
  shiny::tags$head(
    golem::bundle_resources(
      path = app_sys('app/www'),
      app_title = get_golem_config("golem_name")
    ),
    golem::favicon(ico = "BAMLogo"),
    # Add here other external resources
    shinyjs::useShinyjs()
  )

  # include JS for setting up tracking via Matomo
  # for CRAN, including this tracking script can be omitted by changing config.yml
  if (get_golem_config("bam_server")) {
    shiny::tags$head(
      shiny::HTML('<noscript><p><img src="https://agw1.bam.de/piwik/matomo.php?idsite=24&amp;rec=1" style="border:0;" alt="" /></p></noscript>'),
      shiny::HTML('<script type="text/javascript" src="https://agw1.bam.de/piwik/piwik.js" async defer></script>'),
      shiny::includeScript(app_sys("app/www/js/tracking-live.js"))
    )
  }

}
