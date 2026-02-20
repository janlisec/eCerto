#' @title page_Stability
#' @name page_Stability
#'
#' @param id Id when called in module.
#' @param rv eCerto object.
#'
#' @return Will return UI and Server logic for the stability page.
#' @noRd
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyjs::useShinyjs(),
#'       eCerto:::page_StabilityUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       # rv <- eCerto::eCerto$new(eCerto:::init_rv()) # initiate persistent variables
#'       # shiny::isolate({eCerto::setValue(rv, c("Stability","data"), eCerto:::test_Stability_Excel() )})
#'       rv <- eCerto:::test_rv(type = "SR3")
#'       shiny::isolate(eCerto::setValue(rv, c("Stability", "data"), eCerto:::test_Stability_Arrhenius()))
#'       eCerto:::page_StabilityServer(id = "test", rv = rv)
#'     }
#'   )
#' }
page_StabilityUI <- function(id) {
  ns <- shiny::NS(id)

  tab_S1_panel <- bslib::card(
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::strong(shiny::actionLink(inputId = ns("tab_link"), label = "Tab.S1 - calculation of uncertainty contribution")),
      shiny::div(
        shiny::div(
          style = "float: right; margin-left: 15px;",
          m_TransferUUI(ns("s_transfer"))
        ),
        shiny::div(
          style = "float: left; margin-left: 15px;",
          shiny::checkboxInput(inputId = ns("s_adjust"), width = 130, label = shiny::HTML("P-value adjust-<br>ment (bonferroni)"), value = TRUE)
        )
      )
    ),
    #bslib::card_body(max_height = 600,
    bslib::card_body(
      shiny::div(DT::DTOutput(ns("s_tab1"))),
      shinyjs::hidden(shiny::radioButtons(inputId = ns("time_fmt"), label = "Time format in lm", choices = c("mon", "day"), selected = "mon"))
    ),
    bslib::card_footer(
      shiny::p(id = ns("temp_level_info"), "")
    )
  )

  fig_S1_panel <- bslib::card(
    id = ns("fig_S1_panel"),
    style = "resize:vertical;",
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::strong(shiny::actionLink(inputId = ns("fig1_link"), label = "Fig.S1 - linear model plot")),
      shiny::div(
        shiny::div(style = "float: left; margin-left: 15px;", shiny::downloadButton(ns("s_Report"), label = "Download Report")),
        shiny::div(style = "float: left; margin-left: 15px;", shiny::radioButtons(inputId = ns("ReportFormat"), label = NULL, choices = list("HTML"="html", "DOCX"="docx"), width = 70))
      ),
    ),
    bslib::card_body(
      bslib::layout_sidebar(
        padding = 0,
        sidebar = bslib::sidebar(
          position = "right", open = "open", width = 260,
          shiny::div(
            sub_header("Fig.S1 options"),
            shiny::div(style = "margin-top: -2px;", shiny::checkboxInput(inputId = ns("slope_of_means"), label = "Average by Day", value = FALSE)),
            shiny::div(style = "margin-top: -12px;", shiny::checkboxInput(inputId = ns("show_legend"), label = "Annotate plot", value = TRUE)),
            shiny::div(style = "margin-top: -12px;", shiny::checkboxInput(inputId = ns("show_ids"), label = "Show sample IDs", value = FALSE)),
            shiny::div(style = "margin-top: -12px;", shiny::radioButtons(inputId = ns("plot_type"), label = NULL, choices = list("standard" = 1, "adjusted" = 3), inline = TRUE)),
            shiny::div(style = "margin-top: -12px;", shiny::radioButtons(inputId = ns("s_sel_dev"), label = NULL, choiceValues = list("2s", "U"), choiceNames = list("2s", shiny::HTML("U<sub>abs</sub>")), inline = TRUE)),
            shiny::hr(),
            shiny::sliderInput(inputId = ns("s_shelf_life"), label = shiny::HTML("Exp. shelf life t<sub>cert</sub> [Month]"), min = 0, max = 120, value = 60, step = 6),
            shiny::checkboxInput(inputId = ns("optimize_u_stab"), value = FALSE, label = shiny::HTML("Optimize u<sub>stab</sub>")),
            shinyWidgets::pickerInput(inputId = ns("s_sel_temp"), label = "Use Temp level", choices = "", multiple = TRUE),
            shinyWidgets::pickerInput(inputId = ns("s_samples_filtered"), label = "Exclude IDs", choices = "", multiple = TRUE, options = list(container = "body"))
          )
        ),
        shiny::plotOutput(ns("s_plot"), height = "500px")
      )
    )
  )

  shiny::tabsetPanel(
    id = ns("StabilityPanel"),
    type = "hidden",
    # when nothing is loaded
    shiny::tabPanel(
      title = "standby-Panel",
      value = "standby",
      "nothing has uploaded yet"
    ),
    # when something is loaded
    shiny::tabPanel(
      title = "active-Panel",
      value = "loaded",
      bslib::layout_columns(
        shiny::tagList(
          tab_S1_panel
        ),
        shiny::tagList(
          fig_S1_panel
        ),
        col_widths =  bslib::breakpoints(
          sm = c(12, 12),
          xl = c(4, 8)
        )
      ),
      shiny::div(id = ns("arrhenius_panel"), m_arrheniusUI(id = ns("arrhenius")))
    )

  )
}

#' @noRd
page_StabilityServer <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # server part of the arrhenius module
    m_arrheniusServer(id = "arrhenius", rv = rv)

    # collect user parameters in a reactive values list (to store and retrieve from file)
    s_pars <- shiny::reactiveValues(
      # allow filtering of individual s sample values
      "s_samples_filtered" = NULL,
      "slope_of_means" = FALSE,
      "show_legend" = TRUE,
      "show_ids" = FALSE,
      "s_sel_temp" = NULL,
      "s_shelf_life" = 60
    )

    shiny::observe({
      # take care that user selected parameters are copied to rv object (to be maintained upon save)
      s_pars$s_samples_filtered
      s_pars$slope_of_means
      s_pars$show_legend
      s_pars$show_ids
      s_pars$s_sel_temp
      s_pars$s_shelf_life
      if (!identical(getValue(rv, c("Stability", "s_pars")), shiny::reactiveValuesToList(s_pars))) {
        setValue(rv, c("Stability", "s_pars"), shiny::reactiveValuesToList(s_pars))
      }
    })

    shiny::observeEvent(getValue(rv, c("Stability", "data")), {
      tmp <- getValue(rv, c("Stability", "data"))
      # does the data contain Temp information (arrhenius model)
      test <- "Temp" %in% colnames(tmp)
      shinyjs::toggle(id = "s_sel_temp", condition = test)
      shinyjs::toggle(id = "arrhenius_panel", condition = test)
      if (test) {
        temp_levels <- levels(factor(tmp[, "Temp"]))
        shinyWidgets::updatePickerInput(inputId = "s_sel_temp", choices = temp_levels, selected = temp_levels)
        s_pars$s_sel_temp <- temp_levels
      } else {
        temp_levels <- ""
        shinyWidgets::updatePickerInput(inputId = "s_sel_temp", choices = temp_levels)
        s_pars$s_sel_temp <- NULL
      }
      # does the data contain filtering information and IDs already? if not add column and row.names.
      if (is.null(rownames(tmp))) rownames(tmp) <- 1:nrow(tmp)
      if (!identical(tmp, getValue(rv, c("Stability", "data")))) {
        setValue(rv, c("Stability","data"), tmp)
      }
      if (is.null(getValue(rv, c("Stability", "s_pars")))) {
        # if no s_pars entry exists in 'rv' object create a default one
        setValue(rv, c("Stability", "s_pars"), shiny::reactiveValuesToList(s_pars))
      } else {
        # if s_pars entry exists in 'rv' object restore previous parameters
        tmp <- getValue(rv, c("Stability", "s_pars"))
        if ("s_samples_filtered" %in% names(tmp)) s_pars$s_samples_filtered <- tmp$s_samples_filtered else s_pars$s_samples_filtered <- NULL
        if ("s_sel_temp" %in% names(tmp) && !is.null(tmp$s_sel_temp)) s_pars$s_sel_temp <- tmp$s_sel_temp
        if ("slope_of_means" %in% names(tmp)) s_pars$slope_of_means <- tmp$slope_of_means else s_pars$slope_of_means <- FALSE
        if ("show_legend" %in% names(tmp)) s_pars$show_legend <- tmp$show_legend else s_pars$show_legend <- TRUE
        if ("show_ids" %in% names(tmp)) s_pars$show_ids <- tmp$show_ids else s_pars$show_ids <- FALSE
        if ("s_shelf_life" %in% names(tmp)) s_pars$s_shelf_life <- tmp$s_shelf_life else s_pars$s_shelf_life <- 60
        # JL: if more parameters are to be stored for S module they can be set up in this if/else construct
      }
    })

    shiny::observeEvent(input$s_sel_temp_open, {
      # the *_open input of a picker allows to trigger only after the multiple select is closed thus avoiding too frequent updates
      if (!isTRUE(input$s_sel_temp_open)) {
        s_pars$s_sel_temp <- input$s_sel_temp
      }
    })

    shiny::observeEvent(input$s_samples_filtered_open, {
      # the *_open input of a picker allows to trigger only after the multiple select is closed thus avoiding too frequent updates
      if (!isTRUE(input$s_samples_filtered_open)) {
        choices <- rownames(s_Data())[as.character(s_Data()[,"analyte"])==S_analyte()]
        selected <- input$s_samples_filtered
        tmp <- s_pars$s_samples_filtered
        # remove ID subset
        tmp <- tmp[!(tmp %in% choices)]
        # re add ID selection
        tmp <- sort(c(tmp, selected))
        s_pars$s_samples_filtered <- tmp
      }
    })

    shiny::observeEvent(s_pars$s_sel_temp, {
      if (any(s_pars$s_sel_temp != "")) {
        shinyjs::html(id = "temp_level_info", html = paste0("Temperature levels used for calculations in Tab.S1: ", paste(s_pars$s_sel_temp, collapse="\u00B0C, "), "\u00B0C"))
        if (!identical(s_pars$s_sel_temp, input$s_sel_temp)) shinyWidgets::updatePickerInput(inputId = "s_sel_temp", selected = s_pars$s_sel_temp)
      } else {
        shinyjs::html(id = "temp_level_info", html = "")
      }
    })


    shiny::observeEvent(input$show_ids, {
      if (!identical(s_pars$show_ids, input$show_ids)) s_pars$show_ids <- input$show_ids
    })
    shiny::observeEvent(s_pars$show_ids, {
      if (!identical(s_pars$show_ids, input$show_ids)) shiny::updateCheckboxInput(inputId = "show_ids", value = s_pars$show_ids)
    })
    shiny::observeEvent(input$slope_of_means, {
      if (!identical(s_pars$slope_of_means, input$slope_of_means)) s_pars$slope_of_means <- input$slope_of_means
    })
    shiny::observeEvent(s_pars$slope_of_means, {
      if (!identical(s_pars$slope_of_means, input$slope_of_means)) shiny::updateCheckboxInput(inputId = "slope_of_means", value = s_pars$slope_of_means)
    })
    shiny::observeEvent(input$show_legend, {
      if (!identical(s_pars$show_legend, input$show_legend)) s_pars$show_legend <- input$show_legend
    })
    shiny::observeEvent(s_pars$show_legend, {
      if (!identical(s_pars$show_legend, input$show_legend)) shiny::updateCheckboxInput(inputId = "show_legend", value = s_pars$show_legend)
    })
    shiny::observeEvent(input$s_shelf_life, {
      if (!identical(s_pars$s_shelf_life, input$s_shelf_life)) s_pars$s_shelf_life <- input$s_shelf_life
    })
    shiny::observeEvent(s_pars$s_shelf_life, {
      if (!identical(s_pars$s_shelf_life, input$s_shelf_life)) shiny::updateSliderInput(inputId = "s_shelf_life", value = s_pars$s_shelf_life)
    })

    shiny::observeEvent(rv$e_present(), {
      if (rv$e_present()["Stability"]) {
        shiny::updateTabsetPanel(session = session, "StabilityPanel", selected = "loaded")
      } else {
        shiny::updateTabsetPanel(session = session, "StabilityPanel", selected = "standby")
      }
    })

    # the complete data table of stability data as a local copy
    s_Data <- shiny::reactive({
      shiny::req(getValue(rv, c("Stability", "data")))
      s_dat <- getValue(rv, c("Stability", "data"))
      if (!is.factor(s_dat[, "analyte"])) s_dat[, "analyte"] <- factor(s_dat[, "analyte"], levels = unique(s_dat[, "analyte"]))
      if ("Temp" %in% colnames(s_dat)) {
        shiny::validate(shiny::need(expr = length(s_pars$s_sel_temp) >= 1, message = "Please select a Temp level."))
        s_dat <- s_dat[as.character(s_dat[, "Temp"]) %in% s_pars$s_sel_temp, ]
        shiny::validate(shiny::need(expr = diff(range(s_dat[, "time"])) > 0, message = "Please select Temp levels such that independent time points exist."))
        test_number_of_values <- sapply(split(s_dat, s_dat[,"analyte"]), nrow) > 0
        shiny::validate(shiny::need(expr = all(test_number_of_values), message = paste("These analytes do not contain data points:", paste(names(which(test_number_of_values==0)), collapse=", "))))
      }
      tmp <- shiny::isolate(getValue(rv, c("Stability", "s_vals")))
      if (!is.null(tmp)) {
        # expected shelf life (t_cert) could be stored as an input parameter permanently in RData,
        # but having it in Tab.S1 (s_vals) from now on allows to restore a previous setting as well
        if ("t_cert" %in% colnames(tmp)) {
          shiny::updateSliderInput(inputId = "s_shelf_life", value = ifelse(is.finite(tmp[1, "t_cert"]), tmp[1, "t_cert"], 0))
        }
      }
      return(s_dat)
    })

    # the summary of linear models per analyte to estimate u_stab
    s_vals <- shiny::reactive({
      shiny::req(s_Data(), input$s_shelf_life)
      s_dat <- s_Data()[!(rownames(s_Data()) %in% s_pars$s_samples_filtered),]
      out <- prepTabS1(x = s_dat, time_fmt = input$time_fmt, t_cert = input$s_shelf_life, slope_of_means = s_pars$slope_of_means, mt = getValue(rv, c("General", "materialtabelle")), optimize_u_stab = input$optimize_u_stab, adjust = input$s_adjust)
      setValue(rv, c("Stability", "s_vals"), out)
      return(out)
    })

    # we need a local representation of the currently selected analyte in case that there is only a partial overlap between analytes in S and C modul
    S_analyte <- shiny::reactive({
      req(s_vals(), rv$cur_an)
      if (rv$e_present()["Certification"]) shinyjs::enable(id = "s_sel_dev") else shinyjs::disable(id = "s_sel_dev")
      shiny::validate(shiny::need(expr = rv$cur_an %in% as.character(shiny::isolate(s_vals())[, "analyte"]), message = paste("Analyte", rv$cur_an, "is not present in S data.")))
      rv$cur_an
    })

    # Tables
    s_tab1_current <- shiny::reactiveValues("row" = 1, "redraw" = 0)
    output$s_tab1 <- DT::renderDataTable({
      shiny::req(s_vals())
      s_tab1_current$redraw
      styleTabS1(x = s_vals(), mt = getValue(rv, c("General", "materialtabelle")), sr = s_tab1_current$row)
    })
    shiny::observeEvent(input$s_tab1_rows_selected, {
      if (is.null(input$s_tab1_rows_selected)) {
        # trigger a redraw of s_tab1 if the user deselects the current row
        s_tab1_current$redraw <- s_tab1_current$redraw + 1
      } else {
        if (s_tab1_current$row != input$s_tab1_rows_selected) {
          sel <- as.character(s_vals()[input$s_tab1_rows_selected, "analyte"])
          if (!identical(rv$cur_an, sel)) rv$cur_an <- sel
        }
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(S_analyte(), {
      #req(s_vals(), rv$cur_an != S_analyte())
      req(s_vals())
      s_a <- S_analyte()
      # update view for currently selected analyte (trigger coming from C module or Arrhenius module)
      if (!(s_tab1_current$row == which(as.character(s_vals()[, "analyte"]) == s_a))) {
        s_tab1_current$row <- which(as.character(s_vals()[, "analyte"]) == s_a)
      }
      if (!is.null(input$s_sel_dev)) {
        mt <- getValue(rv, c("General", "materialtabelle"))
        test <- s_a %in% mt[, "analyte"] && is.finite(mt[which(mt[, "analyte"] == s_a), "mean"])
        if (rv$e_present()["Certification"]) shinyjs::enable(id = "s_sel_dev") else shinyjs::disable(id = "s_sel_dev")
      }
      # update sample filter
      choices <- rownames(s_Data())[as.character(s_Data()[,"analyte"])==s_a]
      selected <- intersect(s_pars$s_samples_filtered, choices)
      shinyWidgets::updatePickerInput(session = session, inputId = "s_samples_filtered", choices = choices, selected = selected)
    }, ignoreNULL = TRUE, ignoreInit = FALSE)

    # Fig.S1
    output$s_plot <- renderPlotHD({
      shiny::req(s_Data(), S_analyte())
      s_dat <- s_Data()[!(rownames(s_Data()) %in% s_pars$s_samples_filtered),]
      plot_lts_data(
        x = prepFigS1(
          s = s_dat,
          an = S_analyte(),
          apm = getValue(rv, c("General", "apm")),
          U_Def = input$s_sel_dev,
          mt = getValue(rv, c("General", "materialtabelle"))
        ),
        type = as.numeric(input$plot_type),
        t_cert = input$s_shelf_life,
        slope_of_means = s_pars$slope_of_means,
        show_legend = s_pars$show_legend,
        show_ids = s_pars$show_ids
      )
    })

    # u transfer
    m_TransferUServer(id = "s_transfer", rv = rv, type = "S")

    # download report
    output$s_Report <- shiny::downloadHandler(
      filename = function() { paste0("Stability_Report.", input$ReportFormat) },
      content = function(file) {
        render_report_S(
          file = file,
          rv = rv,
          s_dat = s_Data(),
          s_pars = s_pars,
          U_def = input$s_sel_dev,
          t_cert = input$s_shelf_life,
          p_type = as.numeric(input$plot_type)
        )
      }
    )

    # help modals
    shiny::observeEvent(input$fig1_link, { show_help("stability_plot") })
    shiny::observeEvent(input$tab_link, { show_help("stability_uncertainty") })

  })
}
