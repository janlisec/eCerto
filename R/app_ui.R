#' Main User Interface
#'
#' @return the UI
#' @export
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
          #src = fnc_get_local_file("bam_logo_20pt.gif"),
          src = "ecerto/bam_logo_20pt.gif",
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
      footer = shiny::pre(" 2021-09-23, Jan Lisec (jan.lisec@bam.de), v.0.0.0.900"),

      shiny::tabPanel(
        id = "start",
        title = "Start",
        icon = shiny::icon("angle-right"),
        m_startUI("Start")
      ),
      shiny::tabPanel(
        id = "certif_tab",
        title = "Certification",
        value = "tP_certification",
        icon = shiny::icon("angle-right"),
        m_CertificationUI("certification"),
      ),
      shiny::tabPanel(
        id = "homog_tab",
        title = "Homogeneity",
        icon = shiny::icon("angle-right"),
        value = "tP_homogeneity",
        m_HomogeneityUI("Homogeneity")
      ),
      shiny::tabPanel(
        title = "Stability",
        icon = shiny::icon("angle-right"),
        value = "tP_Stability",
        m_StabilityUI("Stability")
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