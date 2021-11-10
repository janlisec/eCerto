#' @name page_Certification
#' @aliases page_CertificationUI
#' @aliases page_CertificationServer
#'
#' @title Certification page
#'
#' @description \code{page_Certification} is the module for handling the
#'  Certification part but also contains the materialtabelle (until further
#'  changes).
#'
#' @details not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv The whole R6 object.
#'
#' @return nothing
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    shinyalert::useShinyalert(),
#'    shinyjs::useShinyjs(),
#'    page_CertificationUI(id = "test")
#'  ),
#'  server = function(input, output, session) {

#'   rv <- eCerto:::test_rv()
#'  page_CertificationServer(id = "test", rv = rv)
#'  }
#' )
#' }
#'
#' @rdname page_Certification
#' @export
page_CertificationUI = function(id) {
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
          width=2,
          shiny::wellPanel(
            style = "height:172px",
            shiny::checkboxGroupInput(
              inputId = ns("certification_view"),
              label = "Select to show",
              choices = c(
                "Imported Data" = "dataview",
                "Outlier Tests" = "stats",
                "Lab-Means Tests" = "mstats",
                #"QQ-Plot" = "qqplot",
                "Certified Values Plot" = "CertValPlot"
              ),
              selected = c("CertValPlot")
            )
          )
        ),
        # Analyte Modul
        shiny::column(
          width=8,
          shiny::wellPanel(style = "height:172px", m_analyteUI(ns("analyteModule")))
        ),
        # Report-Teil
        shiny::column(
          width = 2,
          shiny::wellPanel(style = "height:172px", m_report_ui(ns("report")))
        )
      ),
      # Data View
      shiny::conditionalPanel(
        condition = "input.certification_view.indexOf('dataview') > -1",
        ns = ns,
        m_DataViewUI(ns("dv"))
      ),
      # Stats (on Lab distributions)
      shiny::conditionalPanel(
        condition = "input.certification_view.indexOf('stats') > -1",
        ns = ns, # namespace of current module
        shiny::strong(
          shiny::actionLink(
            inputId = ns("stat_link"),
            label = "Tab.1 Statistics regarding lab variances and outlier detection"
          )
        ),
        DT::dataTableOutput(ns("overview_stats"))
      ),
      # mstats (on Lab means)
      shiny::conditionalPanel(
        condition = "input.certification_view.indexOf('mstats') > -1",
        ns = shiny::NS(id),
        shiny::strong(
          shiny::actionLink(
            inputId = ns("stat2_link"),
            label = "Tab.2 Statistics regarding lab mean distribution"
          )
        ),
        DT::dataTableOutput(ns("overview_mstats")),
        shiny::uiOutput(outputId = ns("tab2_statement")),
      ),
      # CertValPlot
      shiny::conditionalPanel(
        condition = "input.certification_view.indexOf('CertValPlot') > -1",
        ns = shiny::NS(id),
        shiny::fluidRow(
          shiny::column(
            width = 10,
            shiny::strong(
              shiny::actionLink(
                inputId = ns("certifiedValuePlot_link"),
                label = "Fig.1 Certified Value Plot"
              )
            ),
            shiny::plotOutput(ns("overview_CertValPlot"), inline = FALSE)
          ),
          shiny::column(
            width = 2,
            shiny::wellPanel(
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::numericInput(
                    inputId = ns("Fig01_width"),
                    label = "width",
                    value = 400
                  )
                ),
                shiny::column(
                  width = 6,
                  shiny::numericInput(
                    inputId = ns("Fig01_height"),
                    label = "height",
                    value = 400
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  shiny::strong("Download"),
                  shiny::br(),
                  shiny::downloadButton(outputId = ns('Fig01'), label = "Figure")
                ),
                shiny::column(
                  width = 6,
                  shiny::checkboxInput(inputId = ns("annotate_id"), label = "Show IDs", value = FALSE)
                )
              ),
              shiny::p(),
              shiny::fluidRow(
                shiny::column(width = 6, shiny::strong("mean")),
                shiny::column(width = 6, shiny::strong("sd"))
              ),
              shiny::fluidRow(
                shiny::column(width = 6, shiny::textOutput(ns("cert_mean"))),
                shiny::column(width = 6, shiny::textOutput(ns("cert_sd")))
              )
            )
          )
        )
      ),
      # materialtabelle (mandatory)
      m_materialtabelleUI(ns("mat_cert"))
    )
  )
}

#' @rdname page_Certification
#' @export
page_CertificationServer = function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    # Materialtabelle is embedded in Certification-UI, that's why it is here
    selected_tab <- m_materialtabelleServer(id = "mat_cert", rv = rv)

    shiny::observeEvent(getValue(rv, c("Certification", "data")), {
      if (is.null(getValue(rv, c("Certification", "data")))) {
        shiny::updateTabsetPanel(session = session, "certificationPanel", selected = "standby")
      } else {
        shiny::updateTabsetPanel(session = session, "certificationPanel", selected = "loaded")
        shiny::updateNumericInput(
          session=session,
          inputId = "Fig01_width",
          value = 150 + 40 * length(levels(factor(getValue(rv, c("Certification","data"))[, "Lab"])))
        )
      }
    }, ignoreNULL = FALSE)

    # --- --- --- --- --- --- --- --- --- --- ---
    # selected analyte, sample filter, precision
    m_analyteServer("analyteModule", rv, selected_tab, allow_selection=FALSE)

    # --- --- --- --- --- --- --- --- --- --- ---
    # report module
    m_report_server(id = "report", rv = rv, selected_tab = selected_tab)

    # --- --- --- --- --- --- --- --- --- --- ---
    precision <- shiny::reactive({
      shiny::req(selected_tab())
      getValue(rv, c("General","apm"))[[selected_tab()]][["precision"]]
    })

    # this data.frame contains the following columns for each analyte:
    # --> [ID, Lab, analyte, replicate, value, unit, S_flt, L_flt]
    dat <- shiny::reactive({
      shiny::req(selected_tab())
      return(fnc_filter_data(rv=rv, an=selected_tab()))
    })

    # -- -- -- -- -- -- --
    dataset_komp <- m_DataViewServer("dv", dat, precision)
    shiny::observeEvent(dataset_komp(), {
      setValue(rv,c("Certification_processing","data_kompakt"), dataset_komp())
    })

    # store Fig options
    shiny::observeEvent(input$Fig01_width, {
      setValue(rv, c("Certification_processing","CertValPlot","Fig01_width"), input$Fig01_width)
    })

    shiny::observeEvent(input$Fig01_height, {
      setValue(rv, c("Certification_processing","CertValPlot","Fig01_width"), input$Fig01_height)
    })

    shiny::observeEvent(getValue(rv, c("Certification_processing","CertValPlot")), {
      shiny::updateNumericInput(
        session=session,
        inputId = "Fig01_width",
        value = getValue(rv, c("Certification_processing","CertValPlot","Fig01_width"))
      )
      shiny::updateNumericInput(
        session=session,
        inputId = "Fig01_height",
        value = getValue(rv, c("Certification_processing","CertValPlot","Fig01_height"))
      )
    }, ignoreNULL = TRUE)

    output$cert_mean <- shiny::renderText({
      getValue(rv, c("Certification_processing","cert_mean"))
    })

    output$cert_sd <- shiny::renderText({
      getValue(rv, c("Certification_processing","cert_sd"))
    })

    # CertVal Plot
    output$overview_CertValPlot <- shiny::renderPlot({
      CertValPlot(data = dat(), annotate_id=input$annotate_id)
    }, height = shiny::reactive({input$Fig01_height}), width = shiny::reactive({input$Fig01_width}))

    CertValPlot_list <- shiny::reactive({
      shiny::req(input$Fig01_width)
      shiny::req(input$Fig01_height)
      list(
        "show" = TRUE,
        "fnc" = deparse(CertValPlot),
        "call" = str2lang('CertValPlot(data=data)'),
        "Fig01_width" = input$Fig01_width,
        "Fig01_height" = input$Fig01_height
      )
    })

    shiny::observeEvent(CertValPlot_list(),{
      message("CertValPlot_list changed; set rv.CertValPlot")
      setValue(rv,c("Certification_processing","CertValPlot"), CertValPlot_list())
    }, ignoreInit = TRUE)

    # FIGURE DOWNLOAD
    output$Fig01 <- shiny::downloadHandler(
      filename = function() {
        paste0(getValue(rv, c("General","study_id")), "_", getValue(rv, c("General","apm"))[[selected_tab()]][["name"]], "_Fig01.pdf")
      },
      content = function(file) {
        grDevices::pdf(file = file, width = input$Fig01_width/72, height = input$Fig01_height/72)
        CertValPlot(data = dat(), annotate_id=input$annotate_id)
        grDevices::dev.off()
      },
      contentType = "image/pdf"
    )

    shiny::observeEvent(input$qqplot_link, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::plotOutput(session$ns("qqplot")),
          size = "m",
          easyClose = TRUE,
          title = paste("QQ Plot", getValue(rv, c("General","apm"))[[selected_tab()]][["name"]], sep=" - ")
        )
      )
    })

    # Tab.1 Outlier statistics
    overview_stats_pre <- shiny::reactive({
      shiny::req(dat())
      fnc_outlier_stats(data = dat(), precision = precision())
    })
    shiny::observeEvent(overview_stats_pre(), {
      setValue(rv, c("Certification_processing","stats"), overview_stats_pre())
    })
    output$overview_stats <- DT::renderDataTable({
      overview_stats_pre()
    }, options = list(dom = "t", pageLength=100, scrollX = TRUE), selection=list(mode = 'single', target = 'row'), rownames = NULL)

    # Tab.2 Labmean statistics
    labmean_stats_pre <- shiny::reactive({
      shiny::req(dat())
      fnc_labmean_stats(data = dat(), precision = precision())
    })
    shiny::observeEvent(labmean_stats_pre(), {
      setValue(rv, c("Certification_processing","mstats"), labmean_stats_pre())
    })
    output$overview_mstats <- DT::renderDataTable({
      labmean_stats_pre()
    }, options = list(dom = "t", pageLength=1, scrollX = TRUE), selection=list(mode = 'single', target = 'row'), rownames = NULL)

    # Normality statement
    output$tab2_statement <- shiny::renderUI({
      KS_p <- labmean_stats_pre()[,"KS_p"]
      return(
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::HTML(paste0("The data is", ifelse(as.numeric(KS_p) < 0.05, " not ", " "), "normally distributed (KS_p=", KS_p, ").")),
            shiny::HTML("Show "),
            shiny::actionLink(inputId = session$ns("qqplot_link"), label = "QQ-Plot"),
            shiny::HTML(" of Lab means."),
            shiny::p()
          )
        )
      )
    })

    # QQ Plot
    output$qqplot <- shiny::renderPlot({
      y <- overview_stats_pre()[, "mean"]
      stats::qqnorm(y = y)
      stats::qqline(y = y, col = 2)
    }, height = 400, width = 400)

    shiny::observeEvent(input$stat_link,{
      help_the_user("certification_laboratoryStatistics", format = "rmd_with_link")
    })
    shiny::observeEvent(input$stat2_link,{
      help_the_user("certification_meanDistribution", format = "rmd_with_link")
    })
    shiny::observeEvent(input$certifiedValuePlot_link, {
      help_the_user("certification_boxplot")
    })

  })
}
