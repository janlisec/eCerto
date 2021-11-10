#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    shiny::navbarPage(
      id = "navbarpage",
      title = shiny::div(
        class = "verticalhorizontal",
        shiny::img(
          src = "www/bam_logo_20pt.gif",
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
      footer = shiny::pre(shiny::HTML(
        get_golem_config("golem_name"),
        "( ver.", get_golem_config("golem_version"),")",
        "jan.lisec@bam.de"
      )),
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

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  golem::add_resource_path(
    'www', app_sys('app/www')
  )
  shiny::tags$head(
    golem::bundle_resources(
      path = app_sys('app/www'),
      app_title = 'eCerto'
    ),
    golem::favicon(ico = "BAMLogo"),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert()
  )
}

