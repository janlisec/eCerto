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
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyjs::useShinyjs(),
#'       eCerto:::page_CertificationUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       rv <- eCerto:::test_rv("SR3")
#'       eCerto:::page_CertificationServer(id = "test", rv = rv)
#'     }
#'   )
#' }
#'
#' @noRd
#' @keywords internal
page_CertificationUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabsetPanel(
    id = ns("certificationPanel"),
    type = "hidden",
    # when nothing is loaded
    shiny::tabPanel(
      title = "standby-Panel",
      value = "standby",
      "empty panel content" # Platzhalter, falls aus Versehen leere Seite aufgerufen wird
    ),
    # when something is loaded
    shiny::tabPanel(
      title = "active-Panel",
      value = "loaded",
      shiny::wellPanel(
        style = "height: 88px; padding-top: 6px; padding-bottom: 6px; ",
        shiny::div(
          style = "float:left; width: 280px;",
          sub_header("Select to show panel"),
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
            style = "float:left; margin-left: 10px",
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
        shiny::fluidRow(
          shiny::column(
            width = 10,
            shiny::strong(
              shiny::actionLink(
                inputId = ns("tabC1_link"),
                label = "Tab.C1 - Statistics regarding lab variances and outlier detection"
              )
            ),
            DT::dataTableOutput(ns("overview_stats"))
          ),
          shiny::column(
            width = 2,
            shiny::wellPanel(
              sub_header(shiny::actionLink(inputId = ns("tabC1opt_link"), label = "Tab.C1 options"), b = 0),
              shiny::checkboxInput(inputId = ns("tabC1_opt"), label = "Exclude filtered Labs", value = FALSE),
              shiny::selectInput(inputId = ns("tabC1_opt2"), label = "Select values to show", choices = c("Significance level", "P-value", "Test statistic", "Critical value a=0.05", "Critical value a=0.01"), selected = "Significance level")
            )
          )
        )
      ),
      # collapsible_box(
      #   title = shiny::strong(
      #     shiny::actionLink(
      #       inputId = ns("tabC1_link"),
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
            DT::dataTableOutput(ns("TabC2"))
          ),
          shiny::column(
            width = 2,
            shiny::wellPanel(
              shiny::uiOutput(outputId = ns("tab2_statement")),
              shiny::checkboxInput(inputId = ns("tabC2_opt"), label = "Exclude filtered Labs", value = FALSE)
            )
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
            shiny::div(
              style = "width=100%; margin-bottom: 5px;",
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
              shiny::checkboxGroupInput(
                inputId = ns("C1_opt"), label = "Fig.C1 options",
                choices = list(
                  "Show sample IDs" = "annotate_id",
                  "Filenames as axis labels" = "filename_labels",
                  "Automatic width" = "auto_width",
                  "Show legend" = "show_legend"
                ),
                selected = c("auto_width", "show_legend")
              ),
              shiny::fluidRow(
                shiny::div(style = "float: left; width: 30%; max-width: 95px; padding-left: 15px;", shinyjs::disabled(shiny::numericInput(inputId = ns("Fig01_width"), label = "width", value = 400))),
                shiny::div(style = "float: left; width: 30%; max-width: 80px;", shiny::numericInput(inputId = ns("Fig01_height"), label = "height", value = 300)),
                shiny::div(
                  style = "float: left; width: 40%; padding-right: 15px;",
                  sub_header("Download"),
                  shiny::downloadButton(outputId = ns("Fig01"), label = "Figure")
                )
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
page_CertificationServer <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    # Materialtabelle is embedded in Certification-UI, that's why it is here
    m_materialtabelleServer(id = "mat_cert", rv = rv)

    # --- --- --- --- --- --- --- --- --- --- ---
    # selected analyte, sample filter, precision
    m_analyteServer(id = "analyteModule", rv = rv)

    # --- --- --- --- --- --- --- --- --- --- ---
    # report module
    m_reportServer(id = "report", rv = rv)

    # selected_tab() holds the locally selected analyte in the C module
    # if possible this is similar to the global current analyte (rv$cur_an)
    selected_tab <- shiny::reactiveVal(NULL)
    # selected_tab_valid <- shiny::reactive({ selected_tab() %in% rv$a_p("name") })
    shiny::observeEvent(rv$cur_an,
      {
        req(rv$e_present()["Certification"])
        # ensure that App works in case that S and C data don't match
        if (rv$cur_an %in% rv$a_p("name")) {
          selected_tab(rv$cur_an)
        } else {
          message("[page_certification] cant change local selected_tab to rv$cur_an as it is not in C list")
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(getValue(rv, c("Certification", "data")),
      {
        if (is.null(getValue(rv, c("Certification", "data")))) {
          shiny::updateTabsetPanel(session = session, "certificationPanel", selected = "standby")
        } else {
          shiny::updateTabsetPanel(session = session, "certificationPanel", selected = "loaded")
          if (is.null(selected_tab())) {
            # ensure that App works in case that S and C data don't match
            if (rv$cur_an %in% rv$a_p("name")) {
              selected_tab(rv$cur_an)
            } else {
              message("[page_certification] cant change local selected_tab to rv$cur_an as it is not in C list")
            }
          }
        }
      },
      ignoreNULL = FALSE
    )

    # --- --- --- --- --- --- --- --- --- --- ---
    precision <- shiny::reactive({
      shiny::req(selected_tab(), selected_tab() %in% rv$a_p("name"))
      getValue(rv, c("General", "apm"))[[selected_tab()]][["precision"]]
    })

    # this data.frame contains the following columns for each analyte:
    # --> [ID, Lab, analyte, replicate, value, unit, S_flt, L_flt]
    dat <- shiny::reactive({
      shiny::req(selected_tab(), selected_tab() %in% rv$a_p("name"))
      return(c_filter_data(x = getValue(rv, c("Certification", "data")), c_apm = getValue(rv, c("General", "apm"))[[selected_tab()]]))
    })

    shiny::observeEvent(dat(), {
      if ("auto_width" %in% input$C1_opt) {
        n_Labs <- length(unique(dat()$Lab))
        shiny::updateNumericInput(session = session, inputId = "Fig01_width", value = calc_bxp_width(n = n_Labs))
      }
    })

    # -- -- -- -- -- -- --
    m_DataViewServer(id = "dv", rv = rv)

    # store Fig options
    shiny::observeEvent(input$Fig01_width, {
      setValue(rv, c("Certification_processing", "CertValPlot", "Fig01_width"), input$Fig01_width)
    })

    shiny::observeEvent(input$Fig01_height, {
      setValue(rv, c("Certification_processing", "CertValPlot", "Fig01_height"), input$Fig01_height)
    })

    shiny::observeEvent(input$C1_opt,
      {
        shiny::req(dat())
        if ("auto_width" %in% input$C1_opt) {
          shiny::updateNumericInput(session = session, inputId = "Fig01_width", value = calc_bxp_width(n = length(unique(dat()$Lab))))
          shinyjs::disable(id = "Fig01_width")
        } else {
          shinyjs::enable(id = "Fig01_width")
        }
      },
      ignoreNULL = FALSE
    )

    shiny::observeEvent(getValue(rv, c("Certification_processing", "CertValPlot")),
      {
        shiny::req(input$C1_opt)
        w <- ifelse(
          "auto_width" %in% input$C1_opt,
          calc_bxp_width(n = length(unique(dat()$Lab))),
          getValue(rv, c("Certification_processing", "CertValPlot", "Fig01_width"))
        )
        shiny::updateNumericInput(
          session = session,
          inputId = "Fig01_width",
          value = w
        )
        shiny::updateNumericInput(
          session = session,
          inputId = "Fig01_height",
          value = getValue(rv, c("Certification_processing", "CertValPlot", "Fig01_height"))
        )
      },
      ignoreNULL = TRUE
    )

    # CertVal Plot
    output$overview_CertValPlot <- shiny::renderPlot(
      {
        shiny::req(dat())
        CertValPlot(
          data = dat(),
          annotate_id = "annotate_id" %in% input$C1_opt,
          filename_labels = "filename_labels" %in% input$C1_opt,
          show_legend = "show_legend" %in% input$C1_opt,
          dp = rv$a_p()[as.character(dat()[1, "analyte"])]
        )
      },
      height = shiny::reactive({
        input$Fig01_height
      }),
      width = shiny::reactive({
        input$Fig01_width
      })
    )

    CertValPlot_list <- shiny::reactive({
      shiny::req(input$Fig01_width)
      shiny::req(input$Fig01_height)
      list(
        "show" = TRUE,
        "fnc" = deparse(CertValPlot),
        "call" = str2lang(paste0(
          "CertValPlot(data=data, annotate_id=", "annotate_id" %in% input$C1_opt,
          ", filename_labels=", "filename_labels" %in% input$C1_opt,
          ", show_legend=", "show_legend" %in% input$C1_opt,
          ", dp=", rv$a_p()[as.character(dat()[1, "analyte"])], ")"
        )),
        "Fig01_width" = input$Fig01_width,
        "Fig01_height" = input$Fig01_height
      )
    })

    shiny::observeEvent(CertValPlot_list(),
      {
        message("CertValPlot_list changed; set rv.CertValPlot")
        setValue(rv, c("Certification_processing", "CertValPlot"), CertValPlot_list())
      },
      ignoreInit = TRUE
    )

    # FIGURE DOWNLOAD
    output$Fig01 <- shiny::downloadHandler(
      filename = function() {
        paste0(getValue(rv, c("General", "study_id")), "_", getValue(rv, c("General", "apm"))[[selected_tab()]][["name"]], "_Fig01.pdf")
      },
      content = function(file) {
        grDevices::pdf(file = file, width = input$Fig01_width / 72, height = input$Fig01_height / 72)
        CertValPlot(
          data = dat(),
          annotate_id = "annotate_id" %in% input$C1_opt,
          filename_labels = "filename_labels" %in% input$C1_opt,
          show_legend = "show_legend" %in% input$C1_opt,
          dp = rv$a_p()[as.character(dat()[1, "analyte"])]
        )
        grDevices::dev.off()
      },
      contentType = "image/pdf"
    )

    # Tab.1 Outlier statistics
    overview_stats_pre <- shiny::reactive({
      shiny::req(dat(), selected_tab(), selected_tab() %in% rv$a_p("name"), input$tabC1_opt2)
      prepTabC1(dat = dat(), lab_means = rv$c_lab_means(data = dat()), excl_labs = input$tabC1_opt, fmt = encode_fmt(input$tabC1_opt2))
    })
    shiny::observeEvent(overview_stats_pre(), {
      setValue(rv, c("Certification_processing", "stats"), overview_stats_pre())
    })
    output$overview_stats <- DT::renderDataTable({
      req(selected_tab() %in% rv$a_p("name"))
      styleTabC1(x = overview_stats_pre(), n = rv$a_p()[selected_tab()], fmt = encode_fmt(input$tabC1_opt2))
    })

    # Tab.2 Labmean statistics
    TabC2_pre <- shiny::reactive({
      shiny::req(dat(), !is.null(input$tabC2_opt))
      prepTabC2(dat = dat(), excl_labs = input$tabC2_opt)
    })
    shiny::observeEvent(TabC2_pre(), {
      setValue(rv, c("Certification_processing", "mstats"), TabC2_pre())
    })
    output$TabC2 <- DT::renderDataTable({
      req(selected_tab() %in% rv$a_p("name"))
      styleTabC2(x = TabC2_pre(), n = getValue(rv, c("General", "apm"))[[selected_tab()]][["precision"]])
    })

    # Normality statement and QQ plot
    output$tab2_statement <- shiny::renderUI({
      KS_p <- as.numeric(TabC2_pre()[, "KS_p"]) < 0.05
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::HTML(paste0("The data is", ifelse(KS_p, " not ", " "), "normally distributed (KS<sub>p</sub>", ifelse(KS_p, "<", "&ge;"), "0.05).")),
          shiny::HTML("Show "), shiny::actionLink(inputId = session$ns("qqplot_link"), label = "QQ-Plot"), shiny::HTML(" of Lab means.")
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
    output$qqplot <- shiny::renderPlot(
      {
        req(selected_tab() %in% rv$a_p("name"))
        y <- overview_stats_pre()[, "mean"]
        stats::qqnorm(y = y, main = paste("QQ plot for analyte", selected_tab()))
        stats::qqline(y = y, col = 2)
      },
      height = 400,
      width = 400
    )

    # Help Files
    shiny::observeEvent(input$tabC1_link, {
      show_help("certification_laboratoryStatistics")
    })
    shiny::observeEvent(input$tabC1opt_link, {
      show_help("certification_laboratoryStatistics_options")
    })
    shiny::observeEvent(input$stat2_link, {
      show_help("certification_meanDistribution")
    })
    shiny::observeEvent(input$certifiedValuePlot_link, {
      show_help("certification_boxplot")
    })
  })
}
