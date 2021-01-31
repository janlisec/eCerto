#' Title
#'
#' @return
#' @export
#'
#' @examples



ui = function(){
  
  # ui = function() {
  #   shiny::fluidPage(
  #     shinyjs::useShinyjs(),
  #     shiny::wellPanel(xlsxload_ImportCntrlUI("excelfile")),
  #     shiny::wellPanel(shiny::verbatimTextOutput("out")) )
  # }
  
  tagList(
  # tagList to make useShinyjs independent from tabs
  shinyjs::useShinyjs(),
  navbarPage(id = "navbarpage",
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
    tabPanel(
      "Home",
      mainPanel(
        h1("Introducing eCerto"),
        p("Certifications are..."),
        p("Homogeneities are..."),
        p("Stabilities are..."),
        actionLink("link_to_start", "Click here to start"),
      )
      
    ),
    navbarMenu(
      title = "eCerto",
      icon = icon("angle-right"),
      
      # tabs
      tabPanel(
        id = "start",
        title = "Start",
        #sidebarPanel(
        fluidRow(
          column(width = 3,
                 wellPanel(
                   strong("start new session (numb)"),
                   br(),
                   actionButton(inputId = "sessionstart", label = "new")
                 ),
                 wellPanel(
                   fileInput(
                     inputId = "in_file_ecerto_backup",
                     label = "Load Previous Analysis",
                     multiple = FALSE,
                     accept = c("RData")
                   )
                 ),
                 shinyjs::disabled(
                   wellPanel(
                     id = "savepanel",
                     strong("Save"),
                     fluidRow(column(
                       5,
                       textInput(
                         inputId = "user",
                         label = "User",
                         value = "JL"
                       )
                     ),
                     column(
                       5,
                       textInput(
                         inputId = "study_id",
                         label = "Study ID",
                         value = "TEST"
                       )
                     )),
                     downloadButton(outputId = 'ecerto_backup',
                                    label = "Save")
                     
                   )
                 )
          ),
          #############################
          column(width = 9,
                 wellPanel(
                   shiny::wellPanel(xlsxload_ImportCntrlUI("excelfile")) 
                 )
          #######################################       
          )
        )
      ),
      tabPanel(
        id = "certif_tab",
        title = "Certification",
        value = "tP_certification",
        icon = icon("angle-right"),
        .CertificationUI("certification")
        # source(
        #   file = "ui_tabPanel_certification.R",
        #   local = TRUE,
        #   verbose = FALSE
        # )$value
      ),
      
      tabPanel(
        title = "Homogeneity",
        icon = icon("angle-right"),
        value = "tP_homogeneity",
        verbatimTextOutput("homog")
        # source(
        #   file = "ui_tabPanel_homogeneity.R",
        #   local = TRUE,
        #   verbose = FALSE
        # )$value
      ),
      tabPanel(
        title = "Stability",
        icon = icon("angle-right"),
        value = "tP_Stability",
        verbatimTextOutput("stab")
        # source(
        #   file = "ui_tabPanel_stability.R",
        #   local = TRUE,
        #   verbose = FALSE
        # )$value
      )
    ),
    
    # Long term stability
    tabPanel(
      title = "LTS",
      icon = icon("angle-right"),
      value = "tP_LTS",
      # source(
      #   file = "ui_tabPanel_LTS.R",
      #   local = TRUE,
      #   verbose = FALSE
      # )$value
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
)
}