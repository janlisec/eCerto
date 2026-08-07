#' The application User-Interface
#'
#' @param ... A list of bslib::nav_panel objects.
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#' @keywords internal
app_shell <- function(..., request = NULL) {
  shiny::tagList(
    # initializing shinyjs
    shinyjs::useShinyjs(),

    # adding external resources
    golem_add_external_resources(),

    # the following lines can be used to check for problems with the 'www' folder on different App places
     # message("UI, 'www': ", shiny::resourcePaths()["www"]),
     # message("UI, app_sys: ", app_sys('app/www')),
     # message("UI, tempdir: ", tempdir()),

    bslib::page_navbar(
      id = "navbarpage",
      title = app_header(),
      navbar_options = bslib::navbar_options(bg = "black", position = "fixed-top"),
      fillable = TRUE,
      #padding = c(56+16, 16, 24+16), # top, left/right, bottom
      footer = app_footer(),
      ...
    )
  )
}

app_header <- function() {
  list(
    shiny::img(src = "www/bam_logo_200px_transparent.png", height = "40px", position = "absolute", margin = "auto", alt = "BAM Logo"),
    shiny::strong("BAM", style = "color: rgb(210,0,30);"),
    shiny::em(get_golem_config("golem_name"), style = "color: rgb(0,175,240);")
  )
}
app_footer <- function() {
  shiny::div(
    style = "padding-left: var(--bslib-spacer, 1rem); font-family: var(--bs-font-monospace); position: fixed; bottom: 0; background-color: black; color: white; width: 100%",
    shiny::HTML(
      get_golem_config("golem_name"), "|",
      get_golem_config("app_version"), "|",
      get_golem_config("app_date"), "|",
      '<a href="mailto:jan.lisec@bam.de">jan.lisec@bam.de</a>',
      ifelse(get_golem_config("bam_server"), '| <a href="https://www.bam.de/Navigation/EN/Services/Privacy-Policy/privacy-policy.html" target="_blank" rel="noopener noreferrer">BAM Privacy Policy</a>', "")
    )
  )
}

app_panels <- function() {
  list(
    "St" = bslib::nav_panel(
      id = "start",
      title = "Start",
      icon = shiny::icon("angle-right"),
      shiny::div(
        class = "main-content",
        page_startUI("Start")
      )
    ),
    "H" = bslib::nav_panel(
      id = "homog_tab",
      title = "Homogeneity",
      icon = shiny::icon("angle-right"),
      value = "tP_homogeneity",
      shiny::div(
        class = "main-content",
        page_HomogeneityUI("Homogeneity")
      )
    ),
    "S" = bslib::nav_panel(
      id = "stab_tab",
      title = "Stability",
      icon = shiny::icon("angle-right"),
      value = "tP_stability",
      shiny::div(
        class = "main-content",
        page_StabilityUI("Stability")
      )
    ),
    "C" = bslib::nav_panel(
      id = "certif_tab",
      title = "Certification",
      value = "tP_certification",
      icon = shiny::icon("angle-right"),
      shiny::div(
        class = "main-content",
        page_CertificationUI("Certification")
      )
    ),
    "L" = bslib::nav_panel(
      title = "LTS",
      icon = shiny::icon("angle-right"),
      value = "tP_LTS",
      shiny::div(
        class = "main-content",
        m_longtermstabilityUI("lts")
      )
    ),
    "V" = page_validationUI("Validation"),
    "D" = bslib::nav_panel(
      title = "DRMD",
      icon = shiny::icon("angle-right"),
      value = "tP_DRDM",
      shiny::div(
        class = "main-content",
        page_DRMDUI("DRMD")
      )
    ),
    "Hp" = bslib::nav_panel(
      title = "Help",
      icon = shiny::icon("angle-right"),
      value = "tP_help",
      shiny::div(
        class = "main-content",
        page_helpUI("Help")
      )
    )
  )
}

test_nav_panel_app <- function(panel, server) {
  shiny::shinyApp(
    ui = app_shell(panel),
    server = server
  )
}

app_ui <- function(request = NULL) {
  do.call(app_shell, c(unname(app_panels())))
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
  www_tmp <- fs::path(tempdir(), "www")
  if (!base::dir.exists(www_tmp)) {
    base::file.copy(from = app_sys("app/www"), to = tempdir(), recursive = TRUE)
  }
  rps <- shiny::resourcePaths()
  if ("www" %in% names(rps) && !identical(fs::path(rps[["www"]]), www_tmp)) {
    golem::add_resource_path("www", www_tmp)
  }

  # add further resources to the <head> of the HTML page
  shiny::tags$head(
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = get_golem_config("golem_name")
    ),
    golem::favicon(ico = "BAMLogo")
  )

  # include JS for setting up tracking via Matomo
  # for CRAN, including this tracking script can be omitted by changing config.yml
  if (get_golem_config("bam_server")) {
    shiny::tags$head(
      shiny::HTML('<noscript><p><img src="https://agw1.bam.de/piwik/matomo.php?idsite=24&amp;rec=1" style="border:0;" alt="" /></p></noscript>'),
      shiny::HTML('<script type="text/javascript" src="https://agw1.bam.de/piwik/piwik.js" async defer></script>'),
      shiny::includeScript(app_sys("app/www/js/tracking-live.js")),
      shiny::includeCSS(app_sys("app/www/app.css"))
      #tags$link(rel = "stylesheet", type = "text/css", href = "ecerto/app.css")
    )
  }
}
