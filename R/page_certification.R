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
#' @importFrom gargoyle init watch
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    shinyjs::useShinyjs(),
#'    eCerto:::page_CertificationUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- eCerto:::test_rv()
#'    eCerto:::page_CertificationServer(id = "test", rv = rv)
#'  }
#' )
#' }
#'
#' @noRd
#' @keywords internal
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
        shiny::wellPanel(
          style = "height: 112px;",
          shiny::div(
            style = "float:left; width: 280px;",
            shiny::p("Select to show panel", style = "font-weight: 700"),
            shiny::div(
              style = "float:left;",
              shiny::checkboxGroupInput(
                inputId = ns("certification_view"), label = NULL,
                choices = c(
                  "Imported Data" = "dataview",
                  "Outlier Tests" = "stats"
                ),
                selected = "stats"
              )
            ),
            shiny::div(
              style="float:left; margin-left: 10px",
              shiny::checkboxGroupInput(
                inputId = ns("certification_view2"), label = NULL,
                choices = c(
                  "Lab-Means Tests" = "mstats",
                  "Certified Values Plot" = "CertValPlot"
                ),
                selected = c("mstats", "CertValPlot")
              )
            )
          ),
          # Analyte Modul
          m_analyteUI(ns("analyteModule")),
          # Report-Section
          m_reportUI(ns("report"))
      ),
      # Data View
      shiny::conditionalPanel(
        condition = "input.certification_view.indexOf('dataview') > -1",
        ns = ns,
        m_DataViewUI(ns("dv"))
      ),
      # collapsible_box(m_DataViewUI(ns("dv")), title = "Imported data"),
      # Stats (on Lab distributions)
      shiny::conditionalPanel(
        condition = "input.certification_view.indexOf('stats') > -1",
        ns = ns, # namespace of current module
        shiny::strong(
          shiny::actionLink(
            inputId = ns("stat_link"),
            label = "Tab.C1 - Statistics regarding lab variances and outlier detection"
          )
        ),
        DT::dataTableOutput(ns("overview_stats"))
      ),
      # collapsible_box(
      #   title = shiny::strong(
      #     shiny::actionLink(
      #       inputId = ns("stat_link"),
      #       label = "Tab.C1 - Statistics regarding lab variances and outlier detection"
      #     )
      #   ),
      #   DT::dataTableOutput(ns("overview_stats"))
      # ),
      # mstats (on Lab means)
      shiny::conditionalPanel(
        condition = "input.certification_view2.indexOf('mstats') > -1",
        ns = shiny::NS(id),
        shiny::fluidRow(
          shiny::column(
            width = 10,
            shiny::strong(
              shiny::actionLink(
                inputId = ns("stat2_link"),
                label = "Tab.C2 - Statistics regarding lab mean distribution"
              )
            ),
            DT::dataTableOutput(ns("overview_mstats"))
          ),
          shiny::column(
            width = 2,
            shiny::wellPanel(shiny::uiOutput(outputId = ns("tab2_statement")))
          )
        )
      ),
      # CertValPlot
      shiny::conditionalPanel(
        condition = "input.certification_view2.indexOf('CertValPlot') > -1",
        ns = shiny::NS(id),
        shiny::fluidRow(
          shiny::column(
            width = 10,
            shiny::div(style="width=100%; margin-bottom: 5px;",
              shiny::strong(
                shiny::actionLink(
                  inputId = ns("certifiedValuePlot_link"),
                  label = "Fig.C1 - Certified Value Plot"
                )
              )
            ),
            shiny::plotOutput(ns("overview_CertValPlot"), inline = TRUE)
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
                    value = 300
                  )
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 6,
                  #shiny::div(style="margin-bottom: 5px", shiny::strong("Download")),
                  #shiny::br(),
                  shiny::p(shiny::strong("Download")),
                  shiny::downloadButton(outputId = ns('Fig01'), label = "Figure")
                ),
                shiny::column(
                  width = 6,
                  shiny::checkboxInput(inputId = ns("annotate_id"), label = "Show IDs", value = FALSE),
                  shiny::checkboxInput(inputId = ns("filename_labels"), label = "Filenames as labels", value = FALSE)
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

#' @noRd
#' @keywords internal
page_CertificationServer = function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    gargoyle::init("update_c_analyte")

    # Materialtabelle is embedded in Certification-UI, that's why it is here
    m_materialtabelleServer(id = "mat_cert", rv = rv)

    # --- --- --- --- --- --- --- --- --- --- ---
    # selected analyte, sample filter, precision
    m_analyteServer(id = "analyteModule", rv = rv)

    # --- --- --- --- --- --- --- --- --- --- ---
    # report module
    m_reportServer(id = "report", rv = rv)

    selected_tab <- reactiveVal(NULL)
    shiny::observeEvent(gargoyle::watch("update_c_analyte"), {
      selected_tab(rv$c_analyte)
    }, ignoreInit = TRUE)

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
    precision <- shiny::reactive({
      shiny::req(selected_tab())
      getValue(rv, c("General","apm"))[[selected_tab()]][["precision"]]
    })

    # this data.frame contains the following columns for each analyte:
    # --> [ID, Lab, analyte, replicate, value, unit, S_flt, L_flt]
    dat <- shiny::reactive({
      shiny::req(selected_tab())
      return(c_filter_data(x = getValue(rv,c("Certification","data")), c_apm = getValue(rv,c("General","apm"))[[selected_tab()]]))
    })

    # -- -- -- -- -- -- --
    m_DataViewServer(id = "dv", rv = rv)

    # store Fig options
    shiny::observeEvent(input$Fig01_width, {
      setValue(rv, c("Certification_processing","CertValPlot","Fig01_width"), input$Fig01_width)
    })

    shiny::observeEvent(input$Fig01_height, {
      setValue(rv, c("Certification_processing","CertValPlot","Fig01_width"), input$Fig01_height)
    })

    shiny::observeEvent(getValue(rv, c("Certification_processing","CertValPlot")), {
      shiny::updateNumericInput(
        session = session,
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
      shiny::req(dat())
      CertValPlot(data = dat(), annotate_id=input$annotate_id, filename_labels=input$filename_labels)
    }, height = shiny::reactive({input$Fig01_height}), width = shiny::reactive({input$Fig01_width}))

    CertValPlot_list <- shiny::reactive({
      shiny::req(input$Fig01_width)
      shiny::req(input$Fig01_height)
      list(
        "show" = TRUE,
        "fnc" = deparse(CertValPlot),
        "call" = str2lang(paste0('CertValPlot(data=data, annotate_id=', input$annotate_id, ', filename_labels=', input$filename_labels, ')')),
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
          CertValPlot(data = dat(), annotate_id=input$annotate_id, filename_labels=input$filename_labels)
        grDevices::dev.off()
      },
      contentType = "image/pdf"
    )

    # Tab.1 Outlier statistics
    overview_stats_pre <- shiny::reactive({
      shiny::req(dat())
      #getValue(rv, c("General","apm"))
      #gargoyle::watch("update_lab_means")
      #shiny::validate(shiny::need(expr = rv$c_analyte==selected_tab(), message = "analyte selection is inconsistent"))
      #lab_means <- rv$c_lab_means(data = rv$c_fltData(recalc = TRUE), analyte_name = rv$c_analyte)
      lab_means <- rv$c_lab_means(data=dat(), analyte_name=selected_tab())
      out <- data.frame(
        lab_means,
        Scheffe(data = dat()),
        Dixon(lab_means = lab_means),
        Grubbs(lab_means = lab_means),
        Nalimov(lab_means = lab_means),
        Cochran(data = dat()),
        stringsAsFactors = FALSE
      )
      return(out[order(out[, "mean"]), ])
    })
    shiny::observeEvent(overview_stats_pre(), {
      setValue(rv, c("Certification_processing","stats"), overview_stats_pre())
    })
    output$overview_stats <- DT::renderDataTable({
      dt <- DT::datatable(
        data = overview_stats_pre(),
        options = list(dom = "t", pageLength=100, scrollX = TRUE),
        selection=list(mode = 'single', target = 'row'),
        rownames = NULL
      )
      dt <- DT::formatCurrency(
        table = dt, columns = c(2,3), currency = "", digits = getValue(rv, c("General","apm"))[[selected_tab()]][["precision"]]
      )
      return(dt)
    })

    # Tab.2 Labmean statistics
    labmean_stats_pre <- shiny::reactive({
      shiny::req(dat())
      fnc_labmean_stats(data = dat(), precision = precision())
    })
    shiny::observeEvent(labmean_stats_pre(), {
      setValue(rv, c("Certification_processing","mstats"), labmean_stats_pre())
    })
    output$overview_mstats <- DT::renderDataTable({
      dt <- DT::datatable(
        data = labmean_stats_pre(),
        options = list(dom = "t", pageLength=1, scrollX = TRUE),
        selection=list(mode = 'single', target = 'row'),
        rownames = NULL
      )
      dt <- DT::formatCurrency(
        table = dt, columns = c(1,2,3), currency = "", digits = getValue(rv, c("General","apm"))[[selected_tab()]][["precision"]]
      )
      return(dt)
    })

    # Normality statement and QQ plot
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
    shiny::observeEvent(input$qqplot_link, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::plotOutput(session$ns("qqplot")),
          size = "m", easyClose = TRUE, title = NULL, footer = NULL
        )
      )
    })
    output$qqplot <- shiny::renderPlot({
      y <- overview_stats_pre()[, "mean"]
      stats::qqnorm(y = y, main = paste("QQ plot for analyte", rv$c_analyte))
      stats::qqline(y = y, col = 2)
    }, height = 400, width = 400)

    # Help Files
    shiny::observeEvent(input$stat_link,{
      show_help("certification_laboratoryStatistics")
    })
    shiny::observeEvent(input$stat2_link,{
      show_help("certification_meanDistribution")
    })
    shiny::observeEvent(input$certifiedValuePlot_link, {
      show_help("certification_boxplot")
    })

  })
}
