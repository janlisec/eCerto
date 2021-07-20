#' @name mod_Certification
#' @aliases m_CertificationUI
#' @aliases m_CertificationServer
#'
#' @title Certification.
#'
#' @description \code{m_CertificationServer} is the module for handling the
#'  Certification part but also contains the materialtabelle (until further
#'  changes).
#'
#' @details not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param certification reactive({getValue(rv,"Certifications")})
#' @param apm.input analyteParameterList, when uploaded from RData (reactive)
#' @param datreturn the session data object
#'
#' @return analyte parameter
#'
#' @examples
#' if (interactive()) {
#' rv <- ecerto::init_rv()
#' datreturn <- ecerto::init_datreturn()
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_CertificationUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    m_CertificationServer(
#'      id = "test",
#'      certification = shiny::reactive({rv$Certifications}),
#'      datreturn = datreturn
#'    )
#'  }
#' )
#' }
#'
#' @rdname mod_Certification
#' @export
m_CertificationUI = function(id) {
  ns <- shiny::NS(id)
  shiny::tabsetPanel(
    id = ns("certificationPanel"),
    type = "hidden",
    # when nothing is loaded
    shiny::tabPanel(
      title = "standby-Panel",
      value  = "standby",
      "empty panel content" # Platzhalter, falls aus Versehen leere Seite aufgerufen wird
    ),
    # when something is loaded
    shiny::tabPanel(
      title = "active-Panel",
      value = "loaded",
      shiny::fluidRow(
        shiny::column(
          width=4,
          shiny::wellPanel(
            shiny::checkboxGroupInput(
              inputId = shiny::NS(id,"certification_view"),
              label = "Select View:",
              choices = c("boxplot"="boxplot",
                          "Statistics 1" = "stats",
                          "Statistics 2" = "stats2",
                          "QQ-Plot" = "qqplot",
                          "material table" = "mt"),
              selected = c("boxplot","mt")
            )
          )
        ),
        # --- --- --- --- --- --- --- --- ---
        shiny::column(width=8, shiny::wellPanel(m_analyteModuleUI(ns("analyteModule"))))
        # --- --- --- --- --- --- --- --- ---
      ),
      shiny::conditionalPanel(
        # check if checkBoxes are marked for material table
        condition = "input.certification_view.indexOf('boxplot') > -1",
        ns = shiny::NS(id), # namespace of current module,
        shiny::fluidRow(
          # --- --- --- --- --- --- ---
          shiny::column(width = 10, shiny::wellPanel(shiny::fluidRow(m_CertLoadedUI(ns("loaded"))))),
          # --- --- --- --- --- --- ---
          ##### Download-Teil
          shiny::column(
            width = 2,
            shiny::wellPanel(
              shiny::fluidRow(shiny::strong("Download Report")),
              shiny::fluidRow(
                shiny::radioButtons(
                  inputId = ns("output_file_format"),
                  label = NULL,
                  choices = c('PDF', 'HTML', 'Word'),
                  inline = TRUE
                )
              ),
              shiny::fluidRow(
                shiny::column(width = 6, align = "center", shiny::downloadButton('FinalReport', label = "Analyte")),
                shiny::column(
                  width = 6,
                  align = "center",
                  shiny::downloadButton('MaterialReport', label = "Material")
                )
              ),
              shiny::fluidRow(
                shiny::checkboxInput(inputId = ns("show_code"), label = "Show Code in Report")
              )
            )
          )
        )
      ),
      # Stats (on Lab distributions)
      shiny::conditionalPanel(
        condition = "input.certification_view.indexOf('stats') > -1",
        ns = shiny::NS(id), # namespace of current module
        shiny::wellPanel(
          shiny::fluidRow(
            shiny::column(
              width = 9,
              shiny::strong("Statistics regarding lab means, lab variances and outlier detection")
            ),
            DT::dataTableOutput(ns("overview_stats"))
          ),
        )
      ),
      # Stats2 (on Lab means)
      shiny::conditionalPanel(
        condition = "input.certification_view.indexOf('stats2') > -1",
        ns = shiny::NS(id),
        DT::dataTableOutput(ns("overview_mstats")),
        shiny::hr(),
        shiny::fluidRow(shiny::column(9, shiny::textOutput(outputId = ns("normality_statement")))),
        shiny::conditionalPanel(
          condition = "input.certification_view.indexOf('qqplot') > -1",
          ns = shiny::NS(id),
          shiny::plotOutput(ns("qqplot"))
        )
      ),
      # materialtabelle
      shiny::conditionalPanel(
        # check if checkBox is marked for material table
        condition = "input.certification_view.indexOf('mt') > -1",
        ns = shiny::NS(id), # namespace of current module
        # --- --- --- --- --- --- --- --- --- --- ---
        m_materialtabelleUI(ns("mat_cert"))
        # --- --- --- --- --- --- --- --- --- --- ---
      ),
    )
  )
}

#' @rdname mod_Certification
#' @export
m_CertificationServer = function(id, rv, apm.input, datreturn) {
  
  # stopifnot(shiny::is.reactive(certification))
  shiny::moduleServer(id, function(input, output, session) {
    # whereami::cat_where("Certification")
    # shiny::exportTestValues(CertificationServer.d = { try(certification) }) # for shinytest
    
    # certification = reactive({getValue(rv,"Certifications")})
  
    # observeEvent(apm.input(),{
    #   message("---- apm.input! --------")
    # })
    
    # certification.data <- shiny::reactive({certification()$data})
    apm_return <- shiny::reactiveVal(NULL)
    apm = reactiveVal()
    renewTabs = shiny::reactiveVal(NULL)
    dat <- shiny::reactiveVal(NULL)
    
    # temp
    observeEvent(apm.input(),{
      message("---- apm.input! --------")
      apm(apm.input())
    })
    
    shiny::observeEvent(getValue(rv,c("Certifications","data")), {
      message("Certification: Certification-data changed")
      shiny::updateTabsetPanel(session = session,"certificationPanel", selected = "loaded")
    })
    
    observeEvent(getValue(rv,c("Certifications","uploadsource")),{
      # when uploadsource changed, renew Analyte Tabs
      message("Certification: Uploadsource changed to ", isolate(getValue(rv,c('Certifications','uploadsource'))), "; initiate apm")
      # Creation of AnalyteParameterList.
      # Note: Can not be R6 object so far, since indices [[i]] are used in analyte_module
      if(getValue(rv,c("Certifications","uploadsource"))=="Excel") {
        apm(analyte_parameter_list(isolate(getValue(rv,c("Certifications","data")))))
      } else if(getValue(rv,c("Certifications","uploadsource"))=="RData") {
        if(!is.null(isolate(apm.input()))) { # RData contained "apm"
          apm(isolate(apm.input())) #do.call(shiny::reactiveValues, apm.input())
        } else { # RData did not contain "apm" --> create
          apm(analyte_parameter_list(isolate(getValue(rv,c("Certifications","data")))))
        }
      } else {
        stop("unknown Upload Type")
      }
      message("... and renew TABS")
      renewTabs(1)
    })

    # only forward rData Upload after RData was uploaded
    rdataupload = shiny::reactive({
      # shiny::req(getValue(rv,"Certifications"))
      us = isolate(getValue(rv,c("Certifications","uploadsource")))
      if(!is.null(us) && us=="RData") {
        message("Certifications: forward RData to Materialtabelle")
        return(getValue(rv,c("materialtabelle")))
      } #else {
      #  return(NULL)
      #}
    })
    
    # --- --- --- --- --- --- --- --- --- --- ---
    # Materialtabelle is in Certification-UI, that's why it is here
    m_materialtabelleServer(
      id = "mat_cert",
      rdataUpload = rdataupload,
      datreturn = datreturn
    )
    # --- --- --- --- --- --- --- --- --- --- ---
    # --- --- --- --- --- --- --- --- --- --- ---
    # selected analyte, sample filter, precision
    tablist = reactiveVal(NULL) # store created tabs; to be replaced
    selected_tab <- ecerto::m_analyteServer("analyteModule", apm, renewTabs, tablist)
    # --- --- --- --- --- --- --- --- --- --- ---
    observeEvent(apm()[[isolate(selected_tab())]],{
      message("Certifications: apm changed for ", isolate(selected_tab()))
      apm_return(apm())
      # message("app_server: apm changed, set rv.apm")
      # setValue(rv,c("General","apm"), apm()) # getValue(rv,c("General","apm"))
    })
    # --- --- --- --- --- --- --- --- --- --- ---
    dat <- ecerto::m_CertLoadedServer(
      id = "loaded",
      rv = rv,
      apm = apm,
      selected_tab =  selected_tab
    )
    # --- --- --- --- --- --- --- --- --- --- ---
    # shiny::exportTestValues(CertLoadedServer.output = { try(dat()) }) # for shinytest
    
    
    # Calculates statistics for all available labs
    # formerly: lab_means()
    # Format example:
    # Lab       mean    sd       n
    # L1 0.04551667 0.0012560520 6
    # L2 0.05150000 0.0007563068 6
    # L3 0.05126667 0.0004926121 6
    lab_statistics = shiny::reactive({
      # data <- dat()
      shiny::req(dat())
      message("CertificationServer: dat() changed; lab_statistics changed")
      out <-
        plyr::ldply(split(dat()$value, dat()$Lab), function(x) {
          data.frame(
            "mean" = mean(x, na.rm = T),
            "sd" = stats::sd(x, na.rm = T),
            "n" = sum(is.finite(x))
          )
        }, .id = "Lab")
      rownames(out) <- out$Lab
      
      return(out)
    })
    
    
    output$normality_statement <- shiny::renderText({
      l = lab_statistics()
      suppressWarnings(
        KS_p <- stats::ks.test(x = l$mean, y = "pnorm", mean = mean(l$mean), sd = stats::sd(l$mean))$p.value
      )
      normality_statement <- paste0(
        "The data is",
        ifelse(KS_p < 0.05, " not ", " "),
        "normally distributed (KS_p=",
        formatC(KS_p, format = "E", digits = 2),
        ")."
      )
      # getData("normality_statement")
    })
    
    shiny::observeEvent(dat(),{
      message("Certification: dat() changed, set datreturn.selectedAnalyteDataframe")
      ecerto::setValue(datreturn, "selectedAnalyteDataframe", dat())
    })
    
    shiny::observeEvent(lab_statistics(),{
      message("Certification: lab_statistics() changed, set datreturn.lab_statistics")
      ecerto::setValue(datreturn, "lab_statistics", lab_statistics())
    })
    
    
    # Box "QQ-Plot" clickable? Depends in state of Box above it
    shiny::observeEvent(input$certification_view, {
      shinyjs::disable(selector = "#certification-certification_view input[value='qqplot']")
      if("stats2" %in% input$certification_view)
        shinyjs::enable(selector = "#certification-certification_view input[value='qqplot']")
    })
    
    output$overview_stats <- DT::renderDataTable({
      Stats(data = dat(), precision = apm()[[selected_tab()]]$precision)
    }, options = list(paging = FALSE, searching = FALSE), rownames = NULL)
    
    # mStats
    output$overview_mstats <- DT::renderDataTable({
      mstats(data = dat(), precision = apm()[[selected_tab()]]$precision)
    }, options = list(paging = FALSE, searching = FALSE), rownames = NULL)
    
    output$qqplot <- shiny::renderPlot({
      shiny::req(lab_statistics())
      y <- lab_statistics()[, "mean"]
      stats::qqnorm(y = y)
      stats::qqline(y = y, col = 2)
    }, height = 400, width = 400)
    
    
    
    #     ### LOADED END ###s
    #   } else {
    #     # else if nothing is loaded, keep Panel empty
    #     shiny::updateTabsetPanel(session = session,"certificationPanel", selected = "standBy")
    #   }
    # # }, ignoreInit = TRUE)
    # })
    
    return(apm)
  })
}