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
      selected = "Start",
      title = shiny::div(
        class = "verticalhorizontal",
        shiny::img(
          src = fnc_get_local_file("bam_logo_20pt.gif"),
          position = "absolute",
          margin = "auto"
        ),
        shiny::strong("BAM"),
        shiny::em("eCerto"),
        position="relative"
      ),
      windowTitle = "BAM eCerto",
      position = "static-top",
      footer = shiny::pre(" 2021-09-23, Jan Lisec (jan.lisec@bam.de), v.0.0.0.900"),

      shiny::tabPanel(
        id = "start",
        title = "Start",
        icon = shiny::icon("angle-right"),
        # shiny::fluidRow(
        #   shiny::column(
        #     width = 3,
        #     shiny::wellPanel(
        #       shiny::wellPanel(
        #       shiny::strong("Restart Session"),
        #       shiny::br(),
        #       shiny::actionButton(inputId = "session_restart", label = "Restart")),
        #       m_RDataImport_UI("Rdatain"),
        #       m_RDataExport_UI("Rdataex")
        #       ),
        #   ),
        #   shiny::column(
        #     width = 9,
        #     shiny::wellPanel(
        #         shiny::selectInput(
        #           inputId = "moduleSelect",
        #           choices = NULL,
        #           label = shiny::actionLink(inputId = "moduleUploadHelp",label = "Module"),
        #           width = "50%"
        #
        #     ),
        #       m_ExcelUpload_UI("excelfile")
        #     )
        #   )
        # )
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
        help_the_user(filename = "help_start",format = "html", modal = FALSE)

      )
    )
  ) # end taglist
}