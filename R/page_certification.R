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
  sidebar_width <- 280

  # Data View
  # tab_C0_panel <- shiny::conditionalPanel(
  #   condition = "input.certification_view.indexOf('dataview') > -1",
  #   ns = ns,
  #   m_DataViewUI(ns("dv"), sidebar_width = sidebar_width)
  # )

  # Stats (on Lab distributions)
  tab_C1_panel <- bslib::card(
    id = ns("tab_C1_panel"),
    min_height = 200, max_height = 700, fill = FALSE,
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::strong(shiny::actionLink(inputId = ns("tabC1_link"), label = "Tab.C1 - Statistics regarding lab variances and outlier detection")),
      shiny::div(
        shiny::div(style = "float: right; margin-left: 15px; text-align: right;", shiny::checkboxInput(width = 170, inputId = ns("tabC1_opt"), label = "Exclude filtered Labs", value = FALSE)),
        shiny::div(style = "float: right; margin-left: 15px;", shiny::selectInput(width = 200, inputId = ns("tabC1_opt2"), label = NULL, choices = c("Significance level", "P-value", "Test statistic", "Critical value a=0.05", "Critical value a=0.01"), selected = "Significance level"))
      )
    ),
    bslib::card_body(
      shiny::div(DT::dataTableOutput(ns("overview_stats")))
    )
  )

  # mstats (on Lab means)
  tab_C2_panel <- bslib::card(
    min_height = 160, max_height = 240, fill = FALSE,
    id = ns("tab_C2_panel"),
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::strong(shiny::actionLink(inputId = ns("stat2_link"), label = "Tab.C2 - Statistics regarding lab mean distribution")),
      shiny::div(style = "float: right; margin-left: 15px; text-align: right;", shiny::checkboxInput(width = 170, inputId = ns("tabC2_opt"), label = "Exclude filtered Labs", value = FALSE))
    ),
    bslib::card_body(
      shiny::div(DT::dataTableOutput(ns("TabC2")))
    ),
    bslib::card_footer(
      shiny::uiOutput(outputId = ns("tab2_statement"))
    )
  )

  # CertValPlot
  fig_C1_panel <- bslib::card(
    id = ns("fig_C1_panel"),
    #height = 450,
    style = "resize:vertical;",
    bslib::card_header(
      shiny::strong(shiny::actionLink(inputId = ns("certifiedValuePlot_link"), label = "Fig.C1 - Certified Value Plot"))
    ),
    bslib::card_body(
      min_height = 300,
      fill = TRUE,
      bslib::layout_sidebar(
        padding = 0,
        sidebar = bslib::sidebar(
          #position = "right", open = "open", padding = c(0,0,0,16), bg = "white", width = sidebar_width,
          position = "right", open = "open", width = sidebar_width,
          shiny::div(
            sub_header("Fig.C1 options"),
            shiny::checkboxGroupInput(
              inputId = ns("C1_opt"), label = NULL,
              choices = list(
                "Show sample IDs" = "annotate_id",
                "Filenames as axis labels" = "filename_labels",
                "Automatic width" = "auto_width",
                "Show legend" = "show_legend"
              ),
              selected = c("auto_width", "show_legend")
            ),
            shiny::fluidRow(
              shiny::div(style = "float: left; max-width: 95px; padding-left: 15px;", shinyjs::disabled(shiny::numericInput(inputId = ns("Fig01_width"), label = "width", value = 400))),
              shiny::div(style = "float: left; max-width: 95px;", shiny::numericInput(inputId = ns("Fig01_height"), label = "height", value = 400))
            ),
            shiny::div(style = "padding-bottom: 15px;", sub_header("Download"), shiny::downloadButton(outputId = ns("Fig01"), label = "Figure"))
          )
        ),
        # $JL$ the surrounding div is required to include the empty HTML as a way to allow shinking and prevent the figure to be resized horizontally otherwise
        shiny::div(
          shiny::div(style = "display: inline-block;", shiny::plotOutput(ns("overview_CertValPlot"))),
          shiny::div(style = "display: inline-block;", shiny::HTML(""))
        )
      )
    )
  )

  select_panel_div <- shiny::div(
    sub_header("Select to show panel"),
    shiny::div(
      style = "float: left;",
      shiny::checkboxGroupInput(
        inputId = ns("certification_view"), label = NULL, width = "85px",
        choices = c(
          "Tab.C0" = "dataview",
          "Tab.C1" = "stats"
        ),
        selected = "stats"
      )
    ),
    shiny::div(
      #style = "float:left; padding-left: 10px",
      style = "float:left;",
      shiny::checkboxGroupInput(
        inputId = ns("certification_view2"), label = NULL, width = "85px",
        choices = c(
          "Tab.C2" = "mstats",
          "Fig.C1" = "CertValPlot"
        ),
        selected = c("mstats", "CertValPlot")
      )
    )
  )

  top_panel <- bslib::card(
    fill = FALSE,
    style = "background-color: var(--_sidebar-bg);",
    shiny::div(
      class = "d-flex justify-content-between",
      select_panel_div,
      # Analyte Modul
      m_analyteUI(ns("analyteModule")),
      # Report-Section
      m_reportUI(ns("report"))
    )
  )

  shiny::tabsetPanel(
    id = ns("certificationPanel"),
    type = "hidden",
    # when nothing is loaded
    shiny::tabPanel(title = "standby-Panel", value = "standby", "empty panel content"),
    # when something is loaded
    shiny::tabPanel(
      title = "active-Panel",
      value = "loaded",
      bslib::layout_columns(
        shiny::tagList(
          top_panel,
          shiny::div(id=ns("tab_C0_panel"), m_DataViewUI(ns("dv"))),
          tab_C1_panel,
          tab_C2_panel
        ),
        shiny::tagList(
          m_materialtabelleUI(id = ns("mat_cert"), sidebar_width = sidebar_width),
          fig_C1_panel
        ),
        col_widths =  bslib::breakpoints(
          sm = c(12, 12),
          xl = c(6, 6)
        )
      )
    )
  )
}

#' @noRd
#' @keywords internal
page_CertificationServer <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    observeEvent(input$certification_view, {
      shinyjs::toggle(id = "tab_C0_panel", condition = "dataview" %in% input$certification_view)
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    observeEvent(input$certification_view, {
      shinyjs::toggle(id = "tab_C1_panel", condition = "stats" %in% input$certification_view)
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    observeEvent(input$certification_view2, {
      shinyjs::toggle(id = "tab_C2_panel", condition = "mstats" %in% input$certification_view2)
    }, ignoreInit = FALSE, ignoreNULL = FALSE)
    observeEvent(input$certification_view2, {
      shinyjs::toggle(id = "fig_C1_panel", condition = "CertValPlot" %in% input$certification_view2)
    }, ignoreInit = FALSE, ignoreNULL = FALSE)

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
    output$test <- shiny::renderPlot({plot(1:10)})

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
        e_msg("CertValPlot parameter list changed")
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
        #style = "padding-bottom: 10px;",
        shiny::column(
          width = 12,
          shiny::HTML(paste0("The data ", ifelse(KS_p, "<font color=\"#FF0000\">", "<font color=\"#00FF00\">"), "is", ifelse(KS_p, " not ", " "), "normally distributed</font> (KS<sub>p</sub>", ifelse(KS_p, "<", "&ge;"), "0.05).")),
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
