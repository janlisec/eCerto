#' Title
#'
#' @return
#' @export
#' @import shiny
app_ui = function(){

  shiny::tagList(
    # tagList to make useShinyjs independent from tabs
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    shiny::navbarPage(
      id = "navbarpage",

      title = div(
        class = "verticalhorizontal",
        img(
          src = "bam_logo_20pt.gif",
          position = "absolute",
          margin = "auto"
        ),
        strong("BAM"),
        em("ecerto"),
        position = "relative"
      ),

      shiny::tabPanel(
        "Home",

        mainPanel(
          h1("Introducing eCerto"),
          p("Certifications are..."),
          p("Homogeneities are..."),
          p("Stabilities are..."),
          actionLink("link_to_start", "Click here to start"),
          # bookmarkButton()
        )

      ),
      shiny::navbarMenu(
        title = "eCerto",
        icon = icon("angle-right"),
        # tabs
        tabPanel(
          id = "start",
          title = "Start",

          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::wellPanel(
                strong("start new session (numb)"),
                br(),
                actionButton(inputId = "sessionstart", label = "new")
              ),
              wellPanel(.RDataImport_UI("Rdata")),
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
                .ExcelUploadControllUI("excelfile")
              )
            )
          )
        ),
        tabPanel(
          id = "certif_tab",
          title = "Certification",
          value = "tP_certification",
          icon = icon("angle-right"),
          .CertificationUI("certification"),
          # wellPanel(
          #   .materialtabelleUI("mat_cert")
          # )
        ),
        tabPanel(
          id = "homog_tab",
          title = "Homogeneity",
          icon = icon("angle-right"),
          value = "tP_homogeneity",
          wellPanel(.TransferHomogeneityUI("trH")),
          .HomogeneityUI("Homogeneity")
        ),
        tabPanel(
          title = "Stability",
          icon = icon("angle-right"),
          value = "tP_Stability",
          verbatimTextOutput("stab")
        )
      ),
      # eCerto Ende

      # Long term stability
      tabPanel(
        title = "LTS",
        icon = icon("angle-right"),
        value = "tP_LTS",
        .longtermstabilityUI("lts")
      ),

      tabPanel(
        title = "Help",
        icon = icon("angle-right"),
        value = "tP_help",
        # source(
        #   file = "ui_tabPanel_help.R",
        #   local = TRUE,
        #   verbose = FALSE
        # )$value
      )
    )
  ) # end taglist
}