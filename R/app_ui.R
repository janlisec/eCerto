#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  # htis is the padding definition for all panels to respect navbar and footer
  navbar_padding <- "56px"
  footer_padding <- "48px"
  nps <- paste0("padding-top: ", navbar_padding, "; padding-bottom: ", footer_padding)

  # here a bslin them can be defined; however, they all fail at some point with the eCerto layout
  # and are not used in the app
  eCerto_theme <- bslib::bs_theme(
    preset = "shiny",
    base_font = bslib::font_google(c("Assistant", "Inter", "Open Sans")[3]),
    #heading_font = bslib::font_google(c("Assistant", "Inter", "Open Sans")[3]),
    #"--_sidebar-bg" = "#86cecb",
    #"navbar-bg" = "#000000",
    #"body-bg" = "#EEEEEE",
    #"font-family-base" = "monospace",
    #"font-size-base" = "1.4rem",
    #"btn-padding-y" = ".16rem",
    #"btn-padding-x" = "2rem"
    font_scale = 1
  )

  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # the following lines can be used to check for problems with the 'www' folder on different App places
    # message("UI, 'www': ", shiny::resourcePaths()["www"]),
    # message("UI, app_sys: ", app_sys('app/www')),
    # message("UI, tempdir: ", tempdir()),

    bslib::page_navbar(
      id = "navbarpage",
      #theme = eCerto_theme,
      title = list(
        shiny::img(src = "www/bam_logo_200px_transparent.png", height = "40px", position = "absolute", margin = "auto", alt = "BAM Logo"),
        shiny::strong("BAM", style = "color: rgb(210,0,30);"),
        shiny::em(get_golem_config("golem_name"), style = "color: rgb(0,175,240);")
      ),
      selected = "Start",
      bg = "black",
      position = "fixed-top",
      footer = shiny::div(
        style = "padding-left: var(--bslib-spacer, 1rem); font-family: var(--bs-font-monospace); position: fixed; bottom: 0; background-color: black; color: white; width: 100%",
        shiny::HTML(
          get_golem_config("golem_name"), "|",
          get_golem_config("app_version"), "|",
          get_golem_config("app_date"), "|",
          '<a href="mailto:jan.lisec@bam.de">jan.lisec@bam.de</a>',
          ifelse(get_golem_config("bam_server"), '| <a href="https://www.bam.de/Navigation/EN/Services/Privacy-Policy/privacy-policy.html" target="_blank" rel="noopener noreferrer">BAM Privacy Policy</a>', "")
        )
      ),
      bslib::nav_panel(
        id = "start",
        title = "Start",
        icon = shiny::icon("angle-right"),
        shiny::div(style = nps, page_startUI("Start"))
      ),
      bslib::nav_panel(
        id = "homog_tab",
        title = "Homogeneity",
        icon = shiny::icon("angle-right"),
        value = "tP_homogeneity",
        shiny::div(style = nps, page_HomogeneityUI("Homogeneity"))
      ),
      bslib::nav_panel(
        id = "stab_tab",
        title = "Stability",
        icon = shiny::icon("angle-right"),
        value = "tP_stability",
        shiny::div(style = nps, page_StabilityUI("Stability"))
      ),
      bslib::nav_panel(
        id = "certif_tab",
        title = "Certification",
        value = "tP_certification",
        icon = shiny::icon("angle-right"),
        shiny::div(style = nps, page_CertificationUI("certification"))
      ),
      # Long term stability
      bslib::nav_panel(
        title = "LTS",
        icon = shiny::icon("angle-right"),
        value = "tP_LTS",
        shiny::div(style = nps, m_longtermstabilityUI("lts"))
      ),
      bslib::nav_panel(
        title = "Validation",
        icon = shiny::icon("angle-right"),
        value = "tP_Validation",
        shiny::div(style = nps, page_validationUI("Validation"))
      ),
      bslib::nav_panel(
        title = "Help",
        icon = shiny::icon("angle-right"),
        value = "tP_help",
        shiny::div(
          style = nps,
          # don't render Help page in testing mode
          if (getOption("eCerto.renderHelp", default = TRUE)) {
            shiny::withMathJax(shiny::includeCSS(rmarkdown::render(input = get_local_file("help_start.Rmd"), runtime = c("auto", "shiny", "shinyrmd", "shiny_prerendered")[2])))
          } else {
            shiny::div("No help page because App is in testing mode currently.")
          }
        )
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
  file.copy(from = app_sys("app/www"), to = tempdir(), recursive = TRUE)
  golem::add_resource_path(
    "www", paste(normalizePath(tempdir(), "/"), "www", sep = "/")
  )

  # add further resources to the <head> of the HTML page
  shiny::tags$head(
    golem::bundle_resources(
      path = app_sys("app/www"),
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
