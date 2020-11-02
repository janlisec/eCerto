#'library(shiny)
#'setwd("C:/Users/jlisec/Documents/Projects/BAMTool/")
#'#setwd("C:/Users/fkress/R/Portable_Work_Directory/BAMTool")
#'shiny::runApp()
#'options(shiny.reactlog=TRUE)
#'options(shiny.trace = TRUE)
#'shiny::runApp(display.mode="showcase")
#'shinytest::recordTest("C:/Users/jlisec/Documents/Projects/BAMTool/")
#'rsconnect::deployApp()
shinyUI(tagList(
  # initialize shinyjs
  shinyjs::useShinyjs(),
  
  # use navbar layout
  navbarPage(
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
    windowTitle = "BAM ecerto",
    position = "static-top",
    footer = pre(" 2020, Jan Lisec (jan.lisec@bam.de), v.29"),
    
    tabPanel(
      title = "eCerto",
      value = "tP_ToolCert",
      icon = icon("angle-right"),
      
      # top panel to define user, id, load and save
      wellPanel(
        fluidRow(
          column(3, textInput(
            inputId = "user",
            label = "User",
            value = "JL"
          )),
          column(
            3,
            textInput(
              inputId = "study_id",
              label = "Study ID",
              value = "TEST"
            )
          ),
          column(
            3,
            fileInput(
              inputId = "in_file_ecerto_backup",
              label = "Load Previous Analysis",
              multiple = FALSE,
              accept = c("RData")
            )
          ),
          column(
            3,
            strong("Save Current Analysis"),
            p(),
            downloadButton(outputId = 'ecerto_backup', label = "Backup")
          ),
          tags$style(type = "text/css", "#ecerto_backup {margin-top:-1%;}"),
          style = "margin-bottom:-35px;"
        )
      ),
      # style = "padding: 15px;"),
      
      # tabs
      tabsetPanel(
        id = "tsP_certification",
        tabPanel(
          title = "Certification",
          value = "tP_certification",
          icon = icon("angle-right"),
          source(
            file = "ui_tabPanel_certification.R",
            local = TRUE,
            verbose = FALSE
          )$value
        ),
        tabPanel(
          title = "Homogeneity",
          icon = icon("angle-right"),
          value = "tP_homogeneity",
          source(
            file = "ui_tabPanel_homogeneity.R",
            local = TRUE,
            verbose = FALSE
          )$value
        ),
        tabPanel(
          title = "Stability",
          icon = icon("angle-right"),
          value = "tP_Stability",
          source(
            file = "ui_tabPanel_stability.R",
            local = TRUE,
            verbose = FALSE
          )$value
        )
      )
    ),
    
    # Long term stability
    tabPanel(
      title = "LTS",
      icon = icon("angle-right"),
      value = "tP_LTS",
      source(
        file = "ui_tabPanel_LTS.R",
        local = TRUE,
        verbose = FALSE
      )$value
    ),
    
    tabPanel(
      title = "Help",
      icon = icon("angle-right"),
      value = "tP_help",
      source(
        file = "ui_tabPanel_help.R",
        local = TRUE,
        verbose = FALSE
      )$value
    )
    
  )
))
