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
#'     ui = bslib::page_fluid(
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

  # Stats (on Lab distributions)
  tab_C1_panel <- bslib::card(
    id = ns("tab_C1_panel"),
    fill = FALSE,
    #min_height = 200, max_height = 700, fill = FALSE,
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::div(
        shiny::strong(shiny::actionLink(inputId = ns("tabC1_link"), label = "Tab.C1 - Statistics regarding lab variances and outlier detection")),
        shiny::actionButton(inputId = ns("btn_tab_C1"), label = NULL, icon = shiny::icon("compress-arrows-alt"), style = "border: none; padding-left: 5px; padding-right: 5px; padding-top: 0px; padding-bottom: 0px;")
      ),
      shiny::div(
        shiny::div(style = "float: right; margin-left: 15px; text-align: right;", shiny::checkboxInput(width = 170, inputId = ns("tabC1_opt"), label = "Exclude filtered Labs", value = FALSE)),
        shiny::div(style = "float: right; margin-left: 15px;", shiny::selectInput(width = 200, inputId = ns("tabC1_opt2"), label = NULL, choices = c("Significance level", "P-value", "Test statistic", "Critical value a=0.05", "Critical value a=0.01"), selected = "Significance level"))
      )
    ),
    bslib::card_body(
      id = ns("body_tab_C1"),
      shiny::div(DT::dataTableOutput(ns("TabC1")))
    )
  )

  # mstats (on Lab means)
  tab_C2_panel <- bslib::card(
    #min_height = 160, max_height = 240, fill = FALSE,
    id = ns("tab_C2_panel"),
    fill = FALSE,
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::div(
        shiny::strong(shiny::actionLink(inputId = ns("stat2_link"), label = "Tab.C2 - Statistics regarding lab mean distribution")),
        shiny::actionButton(inputId = ns("btn_tab_C2"), label = NULL, icon = shiny::icon("compress-arrows-alt"), style = "border: none; padding-left: 5px; padding-right: 5px; padding-top: 0px; padding-bottom: 0px;")
      ),
      shiny::div(style = "float: right; margin-left: 15px; text-align: right;", shiny::checkboxInput(width = 170, inputId = ns("tabC2_opt"), label = "Exclude filtered Labs", value = FALSE))
    ),
    bslib::card_body(
      id = ns("body_tab_C2"),
      shiny::div(DT::dataTableOutput(ns("TabC2")))
    ),
    bslib::card_footer(
      shiny::uiOutput(outputId = ns("tab2_statement"))
    )
  )

  # CertValPlot
  fig_C1_panel <- bslib::card(
    id = ns("fig_C1_panel"),
    #style = "resize:vertical;",
    bslib::card_header(
      shiny::strong(shiny::actionLink(inputId = ns("certifiedValuePlot_link"), label = "Fig.C1 - Certified Value Plot")),
      shiny::actionButton(inputId = ns("shift_test"), label = NULL, icon = shiny::icon("arrow-right-arrow-left"), style = "border: none; padding-left: 5px; padding-right: 5px; padding-top: 0px; padding-bottom: 0px;"),
      shiny::actionButton(inputId = ns("btn_fig_C1"), label = NULL, icon = shiny::icon("compress-arrows-alt"), style = "border: none; padding-left: 5px; padding-right: 5px; padding-top: 0px; padding-bottom: 0px;")
    ),
    bslib::card_body(
      id = ns("body_fig_C1"),
      fill = FALSE,
      bslib::layout_sidebar(
        padding = 0,
        sidebar = bslib::sidebar(
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
        # $JL$ the surrounding div is required to prevent the figure from being resized horizontally but respect user selected dimensions
        div(
          style = "overflow: hidden; display: inline-block;",
          uiOutput(ns("fig_C1"))
        )
      )
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
        id = "main_columns",
        shiny::div(
          id = ns("left_column"),
          m_DataViewUI(ns("dv")),
          tab_C1_panel,
          tab_C2_panel
        ),
        shiny::div(
          id = ns("right_column"),
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

    # observers to collaps or expand individual cards (tab and fig)
    shiny::observeEvent(input$btn_tab_C1, {
      x <- input$btn_tab_C1 %% 2 == 0
      shinyjs::toggleElement(id = "body_tab_C1", condition = x)
      shiny::updateActionButton(inputId = "btn_tab_C1", icon = shiny::icon(ifelse(x, "compress-arrows-alt", "expand-arrows-alt")))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$btn_tab_C2, {
      x <- input$btn_tab_C2 %% 2 == 0
      shinyjs::toggleElement(id = "body_tab_C2", condition = x)
      shiny::updateActionButton(inputId = "btn_tab_C2", icon = shiny::icon(ifelse(x, "compress-arrows-alt", "expand-arrows-alt")))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$btn_fig_C1, {
      x <- input$btn_fig_C1 %% 2 == 0
      shinyjs::toggleElement(id = "body_fig_C1", condition = x)
      shiny::updateActionButton(inputId = "btn_fig_C1", icon = shiny::icon(ifelse(x, "compress-arrows-alt", "expand-arrows-alt")))
    }, ignoreInit = TRUE)

    # observer to move Fig_C1 to left column when many metabolites are in CRM
    shiny::observeEvent(input$shift_test, {
      #browser()
      #shiny::removeUI(selector = "#fig_C1_panel")
      ns <- session$ns
      shinyjs::runjs(sprintf("
        var card = document.getElementById('%s');
        var column1 = document.getElementById('%s');
        var column2 = document.getElementById('%s');
        if (column1.contains(card)) {
          column2.appendChild(card);
        } else {
          column1.appendChild(card);
        }
      ", ns("fig_C1_panel"), ns("left_column"), ns("right_column")))
    })
    #shiny::removeUI(selector = "#fig_C1_panel")
    # shiny::insertUI(selector = "#column_layout", where = "beforeEnd", ui = bslib::card(
    #   id = "card1",
    #   bslib::card_header("Card 1 Header"),
    #   bslib::card_body("Content of Card 1")
    # ))

    # Materialtabelle is embedded in Certification-UI, that's why it is here
    m_materialtabelleServer(id = "mat_cert", rv = rv)

    # -- -- -- -- -- -- --
    # Tab_C0 (iported data)
    m_DataViewServer(id = "dv", rv = rv)

    # C_analyte() checks if the globally selected analyte is available in the C module
    C_analyte <- shiny::reactive({
      req(rv$cur_an)
      shiny::validate(shiny::need(expr = rv$cur_an %in% rv$a_p("name"), message = paste("Analyte", rv$cur_an, "is not present in C data.")))
      rv$cur_an
    })

    # precision needs to be local to avoid excessive updates of Tab.C1 and Tab.C2 due to changes in 'apm'
    precision <- shiny::reactiveVal(4)

    # this data.frame contains the following columns for each analyte:
    # --> [ID, Lab, analyte, replicate, value, unit, S_flt, L_flt]
    dat <- shiny::reactiveVal(NULL)

    # if new C data is uploaded from Excel or RData in 'rv'
    shiny::observeEvent(getValue(rv, c("Certification", "data")),
      {
        if (is.null(getValue(rv, c("Certification", "data")))) {
          shiny::updateTabsetPanel(session = session, "certificationPanel", selected = "standby")
        } else {
          #dat(c_filter_data(x = getValue(rv, c("Certification", "data")), c_apm = getValue(rv, c("General", "apm"))[[C_analyte()]]))
          shiny::updateTabsetPanel(session = session, "certificationPanel", selected = "loaded")
        }
      },
      ignoreNULL = FALSE
    )

    shiny::observe({
      req(C_analyte(), getValue(rv, c("General", "apm")))
      #tmp <- c_filter_data(x = getValue(rv, c("Certification", "data")), c_apm = getValue(rv, c("General", "apm"))[[C_analyte()]])
      if (!identical(rv$c_fltData(), dat())) {
        message("setting new dat for analyte", rv$cur_an)
        dat(rv$c_fltData())
      }
      if (!identical(rv$a_p("precision")[C_analyte()], precision())) {
        precision(rv$a_p("precision")[C_analyte()])
      }
    })

    shiny::observeEvent(dat(), {
      if ("auto_width" %in% input$C1_opt) {
        n_Labs <- length(unique(dat()$Lab))
        shiny::updateNumericInput(session = session, inputId = "Fig01_width", value = calc_bxp_width(n = n_Labs))
      }
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

    shiny::observeEvent(getValue(rv, c("Certification_processing", "CertValPlot")), {
      shiny::req(input$C1_opt)
      w <- ifelse(
        "auto_width" %in% input$C1_opt,
        calc_bxp_width(n = length(unique(dat()$Lab))),
        getValue(rv, c("Certification_processing", "CertValPlot", "Fig01_width"))
      )
      shiny::updateNumericInput(session = session, inputId = "Fig01_width", value = w)
      shiny::updateNumericInput(session = session, inputId = "Fig01_height", value = getValue(rv, c("Certification_processing", "CertValPlot", "Fig01_height")))
    }, ignoreNULL = TRUE)

    # CertVal Plot
    output$fig_C1 <- renderUI({
      # this solution using renderUI allows to reproducibly ensure width and height to be respected by bslib
      shiny::plotOutput(session$ns("fig_C1_pre"), width = paste0(input$Fig01_width, "px"), height = paste0(input$Fig01_height, "px"))
    })
    output$fig_C1_pre <- shiny::renderPlot({
      shiny::req(dat())
      CertValPlot(
        data = dat(),
        annotate_id = "annotate_id" %in% input$C1_opt,
        filename_labels = "filename_labels" %in% input$C1_opt,
        show_legend = "show_legend" %in% input$C1_opt,
        dp = precision()
      )
    })

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
          ", dp=", precision(), ")"
        )),
        "Fig01_width" = input$Fig01_width,
        "Fig01_height" = input$Fig01_height
      )
    })

    shiny::observeEvent(CertValPlot_list(),
      {
        e_msg("Fig.C1 options changed")
        setValue(rv, c("Certification_processing", "CertValPlot"), CertValPlot_list())
      },
      ignoreInit = TRUE
    )

    # FIGURE DOWNLOAD
    output$Fig01 <- shiny::downloadHandler(
      filename = function() {
        paste0(getValue(rv, c("General", "study_id")), "_", getValue(rv, c("General", "apm"))[[C_analyte()]][["name"]], "_Fig01.pdf")
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
    TabC1_pre <- shiny::reactive({
      shiny::req(dat(), input$tabC1_opt2)
      prepTabC1(dat = dat(), lab_means = rv$c_lab_means(data = dat()), excl_labs = input$tabC1_opt, fmt = encode_fmt(input$tabC1_opt2))
    })
    shiny::observeEvent(TabC1_pre(), {
      setValue(rv, c("Certification_processing", "stats"), TabC1_pre())
    })
    output$TabC1 <- DT::renderDataTable({
      styleTabC1(x = TabC1_pre(), n = precision(), fmt = encode_fmt(input$tabC1_opt2))
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
      styleTabC2(x = TabC2_pre(), n = precision())
    })

    # Normality statement and QQ plot
    output$tab2_statement <- shiny::renderUI({
      KS_p <- as.numeric(TabC2_pre()[, "KS_p"]) < 0.05
      shiny::fluidRow(
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
        y <- TabC1_pre()[, "mean"]
        stats::qqnorm(y = y, main = paste("QQ plot for analyte", C_analyte()))
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
