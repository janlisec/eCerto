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
#' @param datreturn the session data object
#'
#' @return nothing directly, works over apm parameter
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
#'      certification = reactive({rv$Certifications}),
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
      # @Frederick: what is this non-named parameter for?
      "empty panel content"
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
          shiny::column(width = 10, shiny::wellPanel(fluidRow(m_CertLoadedUI(ns("loaded"))))),
          # --- --- --- --- --- --- ---
          ##### Download-Teil
          shiny::column(
            width = 2,
            wellPanel(
              shiny::fluidRow(strong("Download Report")),
              shiny::fluidRow(
                shiny::radioButtons(
                  inputId = ns("output_file_format"),
                  label = NULL,
                  choices = c('PDF', 'HTML', 'Word'),
                  inline = TRUE
                )
              ),
              shiny::fluidRow(
                shiny::column(width = 6, align = "center", downloadButton('FinalReport', label = "Analyte")),
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
          shiny::plotOutput("qqplot")
        )
      ),
      # materialtabelle
      shiny::conditionalPanel(
        # check if checkBoxes are marked for material table
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
m_CertificationServer = function(id, certification, datreturn) {

  stopifnot(shiny::is.reactive(certification))

  shiny::moduleServer(id, function(input, output, session) {

    exportTestValues(CertificationServer.d = { try(certification()) })

    d_act <- shiny::reactiveVal("Haha nope")

    certification_data <- shiny::reactive({data_of_godelement(certification())})

    apm <- analyte_parameter_list()
    dat <- shiny::reactiveVal(NULL)

    shiny::observeEvent(certification_data(), {
      #if loaded (successfully), make area visible
      # AGAIN: SUCCESSFULLY LOADED HERE!
      if(!is.null(certification_data())){
        d_act("TRUE")
        message("Certification Module start")
        shiny::updateTabsetPanel(session = session,"certificationPanel", selected = "loaded")
        apm <- analyte_parameter_list(certification_data())

        # selected analyte, sample filter, precision
        # --- --- --- --- --- --- --- --- --- --- ---
        selected_tab <- m_analyteServer("analyteModule", apm)
        # --- --- --- --- --- --- --- --- --- --- ---

        # --- --- --- --- --- --- --- --- --- --- ---
        dat <- m_CertLoadedServer(
          id = "loaded",
          certification = certification,
          apm = apm,
          selected_tab =  selected_tab
        )
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
          shiny::req(dat())
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

        output$normality_statement <- shiny::renderText({
          l = lab_statistics()
          suppressWarnings(
            KS_p <- stats::ks.test(x = l$mean, y = "pnorm", mean = mean(l$mean), sd = sd(l$mean))$p.value
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

        # observe({
        #   setValue(datreturn,"lab_statistics",lab_statistics())
        #   # datreturn$set("lab_statistics",lab_statistics())
        #   # datreturn$lab_statistics = lab_statistics()
        #   # message("m_CertificationServer -- lab_statistics created")
        #   # datreturn$set("selectedAnalyteDataframe",dat())
        #   setValue(datreturn,"selectedAnalyteDataframe",dat())
        #   # datreturn$selectedAnalyteDataframe = dat()
        #   # console log
        #   # message(paste0(".CertificiationServer -- analyte selected: ",dat()[1,"analyte"]))
        # })

        shiny::observeEvent(dat(),{
          setValue(datreturn, "selectedAnalyteDataframe", dat())
        })

        shiny::observeEvent(lab_statistics(),{
          setValue(datreturn, "lab_statistics", lab_statistics())
        })


        observeEvent(input$certification_view, {
          shinyjs::disable(selector = "#certification-certification_view input[value='qqplot']")
          if("stats2" %in% input$certification_view)
            shinyjs::enable(selector = "#certification-certification_view input[value='qqplot']")
          })

        output$overview_stats <- DT::renderDataTable({
          message("stats 1")
          message(dat())
          Stats(data = dat(), precision = apm$analytes[[selected_tab()]]$precision)
        }, options = list(paging = FALSE, searching = FALSE), rownames = NULL)

        # mStats
        output$overview_mstats <- DT::renderDataTable({
          mstats(data = dat(), precision = apm$analytes[[selected_tab()]]$precision)
        }, options = list(paging = FALSE, searching = FALSE), rownames = NULL)

        ### LOADED END ###s
      } else {
        # else if nothing is loaded, keep Panel empty
        shiny::updateTabsetPanel(session = session,"certificationPanel", selected = "standBy")
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