#'@title CERTIFICATION MODULE
#'
#'@description \code{m_CertificationServer} is the module for handling the
#'  Certification part but also contains the materialtabelle (until further
#'  changes).
#'
#'@details not yet
#'
#'@param certification = reactive({rv$Certifications})
#'@param datreturn the session data object
#'
#'@return nothing directly, works over apm parameter
#'@export
m_CertificationUI = function(id) {
  tabsetPanel(
    id = NS(id, "certificationPanel"),
    type = "hidden",
    # when nothing is loaded
    tabPanel(title = "standby-Panel", value  = "standby", "empty panel content"),
    # when something is loaded
    tabPanel(
      title = "active-Panel",
      value = "loaded",
      fluidRow(
        column(4,
               wellPanel(
                 checkboxGroupInput(
                   inputId = NS(id,"certification_view"),
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
        column(8,
               wellPanel(
                 # --- --- --- --- --- --- --- --- ---
                 m_analyteModuleUI(NS(id, "analyteModule")),
                 # --- --- --- --- --- --- --- --- ---
               )
        )
      ),
      conditionalPanel(
        # check if checkBoxes are marked for material table
        condition = "input.certification_view.indexOf('boxplot') > -1",
        ns = NS(id), # namespace of current module,
        fluidRow(
          column(
            10,
            wellPanel(
              fluidRow(
                # --- --- --- --- --- --- ---
                m_CertLoadedUI(NS(id,"loaded"))
                # --- --- --- --- --- --- ---
              )
            ) ),
          
          ##### Download-Teil
          column(
            2,
            wellPanel(
              fluidRow(strong("Download Report")),
              fluidRow(
                radioButtons(
                  inputId = NS(id, 'output_file_format'),
                  label = NULL,
                  choices = c('PDF', 'HTML', 'Word'),
                  inline = TRUE
                )
              ),
              fluidRow(
                column(6, align = "center", downloadButton('FinalReport', label = "Analyte")),
                column(
                  6,
                  align = "center",
                  downloadButton('MaterialReport', label =
                                   "Material")
                )
              ),
              fluidRow(
                checkboxInput(
                  inputId = NS(id, "show_code"), label = "Show Code in Report"
                )
              )
            )
          ) )
      ),
      # Stats
      conditionalPanel(
        condition = "input.certification_view.indexOf('stats') > -1",
        ns = NS(id), # namespace of current module
        wellPanel(
          fluidRow(
            column(
              width = 9,
              strong(
                "Statistics regarding lab means, lab variances and outlier detection"
              )
            ),
            DT::dataTableOutput(NS(id,"overview_stats"))
          ),
        )
      ),
      conditionalPanel(
        condition = "input.certification_view.indexOf('stats2') > -1",
        ns = NS(id),
        DT::dataTableOutput(NS(id, "overview_mstats")),
        hr(),
        fluidRow(
          column(9, textOutput(outputId = NS(id,"normality_statement"))),
        ),
        conditionalPanel(
          condition = "input.certification_view.indexOf('qqplot') > -1",
          ns = NS(id),
          plotOutput("qqplot")
        )
      ),
      conditionalPanel( 
        # check if checkBoxes are marked for material table
        condition = "input.certification_view.indexOf('mt') > -1",
        ns = NS(id), # namespace of current module
        # --- --- --- --- --- --- --- --- --- --- ---
        # wellPanel(
          m_materialtabelleUI(NS(id,"mat_cert"))
        # ),
        # --- --- --- --- --- --- --- --- --- --- ---
      ),
    )
  )
}

#' @export
m_CertificationServer = function(id, certification, datreturn) {
  stopifnot(is.reactive(certification))
  moduleServer(id, function(input, output, session) {
    exportTestValues(CertificationServer.d = { try(certification()) })
    
    d_act = reactiveVal("Haha nope")
    
    certification_data = reactive({data_of_godelement(certification())})
    
    apm = analyte_parameter_list()
    dat = reactiveVal(NULL)
    
    observeEvent(certification_data(), {
      #if loaded (successfully), make area visible
      # AGAIN: SUCCESSFULLY LOADED HERE!
      if(!is.null(certification_data())){
        d_act("TRUE")
        message("Certification Module start")
        updateTabsetPanel(session = session,"certificationPanel", selected = "loaded")
       apm = analyte_parameter_list(certification_data())
        
        # selected analyte, sample filter, precision
        # --- --- --- --- --- --- --- --- --- --- ---
        m_analyteServer("analyteModule", apm)
        # --- --- --- --- --- --- --- --- --- --- ---
        
        # --- --- --- --- --- --- --- --- --- --- ---
        dat = m_CertLoadedServer("loaded",certification = certification, apm = apm)
        # --- --- --- --- --- --- --- --- --- --- ---
        exportTestValues(CertLoadedServer.output = { try(dat()) })

        # Calculates statistics for all available labs
        # formerly: lab_means()
        # Format example:
        # Lab       mean    sd       n
        # L1 0.04551667 0.0012560520 6
        # L2 0.05150000 0.0007563068 6
        # L3 0.05126667 0.0004926121 6
        lab_statistics = reactive({
          # data <- dat()
          req(dat())
          message("CertificationServer : lab_statistics created")
          out <-
            plyr::ldply(split(dat()$value, dat()$Lab), function(x) {
              data.frame(
                "mean" = mean(x, na.rm = T),
                "sd" = sd(x, na.rm = T),
                "n" = sum(is.finite(x))
              )
            }, .id = "Lab")
          rownames(out) <- out$Lab

          return(out)
        })
        
        output$normality_statement <- renderText({
          l = lab_statistics()
          suppressWarnings(KS_p <-
                             stats::ks.test(
                               x = l$mean,
                               y = "pnorm",
                               mean = mean(l$mean),
                               sd = sd(l$mean)
                             )$p.value)
          normality_statement  = paste0(
            "The data is",
            ifelse(KS_p < 0.05, " not ", " "),
            "normally distributed (KS_p=",
            formatC(KS_p, format = "E", digits = 2),
            ")."
          )
          # getData("normality_statement")
        })

        observe({
          datreturn$lab_statistics = lab_statistics()
          # message("m_CertificationServer -- lab_statistics created")
          datreturn$selectedAnalyteDataframe = dat()
          # console log
          # message(paste0(".CertificiationServer -- analyte selected: ",dat()[1,"analyte"]))
        })


        observeEvent(input$certification_view, {
          shinyjs::disable(selector = "#certification-certification_view input[value='qqplot']")
          if("stats2" %in% input$certification_view)
            shinyjs::enable(selector = "#certification-certification_view input[value='qqplot']")

          })
        
        output$overview_stats <- DT::renderDataTable({
          
          message("stats 1")
          message(dat())
          Stats(data = dat(), precision = apm$analytes[[apm$selected_tab]]$precision)
        }, options = list(paging = FALSE, searching = FALSE), rownames = NULL)
        
        # mStats
        output$overview_mstats <- DT::renderDataTable({
          mstats(data = dat(), precision = apm$analytes[[apm$selected_tab]]$precision)
        }, options = list(paging = FALSE, searching = FALSE), rownames = NULL)
        
        ### LOADED END ###s
      } else { 
        # else if nothing is loaded, keep Panel empty
        updateTabsetPanel(session = session,"certificationPanel", selected = "standBy")
      }
    # }, ignoreInit = TRUE)
    })

    
    # --- --- --- --- --- --- --- --- --- --- ---
    m_materialtabelleServer(
      id = "mat_cert", 
      rdataUpload = reactive({certification()$materialtabelle}), 
      datreturn = datreturn
    )
    # --- --- --- --- --- --- --- --- --- --- ---
    
 
    
  })
}