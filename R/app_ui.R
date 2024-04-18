#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  navbar_padding <- 56
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    tags$head(
      tags$style(HTML(".control-label{ font-weight: bold; }"))
    ),

    # the following lines can be used to check for problems with the 'www' folder on different App places
    # message("UI, 'www': ", shiny::resourcePaths()["www"]),
    # message("UI, app_sys: ", app_sys('app/www')),
    # message("UI, tempdir: ", tempdir()),

    bslib::page_navbar(
      ## use an alternative theme
      # theme = bslib::bs_theme(
      #   version = 5,
      #   bootswatch = c("sandstone","zephyr")[1],
      #   base_font = bslib::font_google(c("Assistant", "Anonymous Pro")[2])
      # ),

      id = "navbarpage",
      title = list(
        shiny::img(src = "www/bam_logo_20pt.gif", position = "absolute", margin = "auto", alt = "BAM Logo", style="background-color: black;"),
        shiny::strong("BAM", style = "color: rgb(210,0,30);"),
        shiny::em(get_golem_config("golem_name"), style = "color: rgb(0,175,240);")
      ),
      selected = "Start",
      window_title = paste("BAM", get_golem_config("golem_name")),
      position = "fixed-top",
      footer = shiny::div(
        style = "position: fixed; bottom: 0px; left: 0px; width: 100%; padding-left: 15px; padding-top: 2px; padding-bottom: 2px; background-color: #f8f8f8; font-family: Lucida Console, monospace;",
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
        shiny::div(style = paste0("padding-top: ", navbar_padding, "px;"), page_startUI("Start"))
      ),
      bslib::nav_panel(
        id = "homog_tab",
        title = "Homogeneity",
        icon = shiny::icon("angle-right"),
        value = "tP_homogeneity",
        shiny::div(style = paste0("padding-top: ", navbar_padding, "px;"), page_HomogeneityUI("Homogeneity"))
      ),
      bslib::nav_panel(
        id = "stab_tab",
        title = "Stability",
        icon = shiny::icon("angle-right"),
        value = "tP_stability",
        shiny::div(style = paste0("padding-top: ", navbar_padding, "px;"), page_StabilityUI("Stability"))
      ),
      bslib::nav_panel(
        id = "certif_tab",
        title = "Certification",
        value = "tP_certification",
        icon = shiny::icon("angle-right"),
        shiny::div(style = paste0("padding-top: ", navbar_padding, "px;"), page_CertificationUI("certification"))
      ),
      # Long term stability
      bslib::nav_panel(
        title = "LTS",
        icon = shiny::icon("angle-right"),
        value = "tP_LTS",
        shiny::div(style = paste0("padding-top: ", navbar_padding, "px;"), m_longtermstabilityUI("lts"))
      ),
      # bslib::nav_panel(
      #   title = "Help",
      #   icon = shiny::icon("angle-right"),
      #   value = "tP_help",
      #   shiny::div(
      #     style = paste0("padding-top: ", navbar_padding, "px; float: left"),
      #     shiny::withMathJax(shiny::includeCSS(path = get_local_file("help_start.html")))
      #   )
      # ),
      bslib::nav_panel(
        title = "Help",
        icon = shiny::icon("angle-right"),
        value = "tP_help",
        shiny::div(
          style = paste0("padding-top: ", navbar_padding, "px;"),
          shiny::withMathJax(shiny::includeCSS(rmarkdown::render(input = get_local_file("help_start.Rmd"), runtime = c("auto", "shiny", "shinyrmd", "shiny_prerendered")[1])))
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
