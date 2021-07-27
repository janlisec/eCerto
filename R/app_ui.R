#' Main User Interface
#'
#' @return the UI
#' @export
app_ui = function(){

  shiny::tagList(
    # tagList to make useShinyjs independent from tabs
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    shiny::navbarPage(
      id = "navbarpage",

      title = shiny::div(
        class = "verticalhorizontal",
        shiny::img(
          src = "bam_logo_20pt.gif",
          position = "absolute",
          margin = "auto"
        ),
        shiny::strong("BAM"),
        shiny::em("ecerto"),
        position = "relative"
      ),

      shiny::tabPanel(
        "Home",
        shiny::sidebarPanel(width = 3,
          # shiny::wellPanel(
            shiny::strong("Restart Session"),
            shiny::br(),
            shiny::actionButton(inputId = "session_restart", label = "Restart")
          # )
        ),
        shiny::mainPanel(
          shiny::h1("Introducing eCerto"),
          shiny::p("Certifications are..."),
          shiny::p("Homogeneities are..."),
          shiny::p("Stabilities are..."),
          shiny::actionLink("link_to_start", "Click here to start"),
          # bookmarkButton()
        )

      ),
      shiny::navbarMenu(
        title = "eCerto",
        icon = shiny::icon("angle-right"),
        # tabs
        shiny::tabPanel(
          id = "start",
          title = "Start",

          shiny::fluidRow(
            shiny::column(
              width = 3,
              
              shiny::wellPanel(m_RDataImport_UI("Rdata")),
            ),
            shiny::column(
              width = 9,
              shiny::wellPanel(
                shiny::selectInput(
                  inputId = "moduleSelect",
                  choices = NULL,
                  label = "module",
                  width = "50%"
                ),
                m_ExcelUploadControl_UI("excelfile")
              )
            )
          )
        ),
        shiny::tabPanel(
          id = "certif_tab",
          title = "Certification",
          value = "tP_certification",
          icon = shiny::icon("angle-right"),
          m_CertificationUI("certification"),
          # wellPanel(
          #   .materialtabelleUI("mat_cert")
          # )
        ),
        shiny::tabPanel(
          id = "homog_tab",
          title = "Homogeneity",
          icon = shiny::icon("angle-right"),
          value = "tP_homogeneity",
          shiny::wellPanel(m_TransferHomogeneityUI("trH")),
          m_HomogeneityUI("Homogeneity")
        ),
        shiny::tabPanel(
          title = "Stability",
          icon = shiny::icon("angle-right"),
          value = "tP_Stability",
          shiny::verbatimTextOutput("stab")
        )
      ),
      # eCerto Ende

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
        shiny::includeCSS(system.file(package = "ecerto","help","help_start.html")) # currently includeCSS, since includeHTML blocking Navbar
      )
    )
  ) # end taglist
}