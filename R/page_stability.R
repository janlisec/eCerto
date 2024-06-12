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
    #min_height = 500
    #fill = FALSE,
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::strong(shiny::actionLink(inputId = ns("tab_link"), label = "Tab.S1 - calculation of uncertainty contribution")),
      shiny::div(
        shiny::div(style = "float: left; margin-left: 15px;", m_TransferUUI(id = ns("s_transfer")))
      )
    ),
    bslib::card_body(
      shiny::div(DT::DTOutput(ns("s_tab1"))),
      shinyjs::hidden(shiny::radioButtons(inputId = ns("time_fmt"), label = "Time format in lm", choices = c("mon", "day"), selected = "mon"))
    )
  )

  fig_S1_panel <- bslib::card(
    id = ns("fig_H1_panel"),
    style = "resize:vertical;",
    bslib::card_header(
      shiny::strong(shiny::actionLink(inputId = ns("fig1_link"), label = "Fig.S1 - linear model plot"))
    ),
    bslib::card_body(
      #min_height = 300,
      #fill = TRUE,
      bslib::layout_sidebar(
        padding = 0,
        sidebar = bslib::sidebar(
          position = "right", open = "open", width = 260,
          shiny::div(
            sub_header("Fig.S1 options"),
            shiny::checkboxGroupInput(
              inputId = ns("FigS1_options"), label = NULL,
              choices = list("Average by Day" = "slope_of_means", "Annotate plot" = "show_legend"),
              selected = c("show_legend")
            ),
            shiny::div(style = "margin-top: -12px;", shiny::radioButtons(inputId = ns("plot_type"), label = NULL, choices = list("standard" = 1, "adjusted" = 3), inline = TRUE)),
            shiny::div(style = "margin-top: -12px;", shiny::radioButtons(inputId = ns("s_sel_dev"), label = NULL, choiceValues = list("2s", "U"), choiceNames = list("2s", shiny::HTML("U<sub>abs</sub>")), inline = TRUE)),
            shiny::hr(),
            shiny::sliderInput(inputId = ns("s_shelf_life"), label = shiny::HTML("Exp. shelf life t<sub>cert</sub> [Month]"), min = 0, max = 120, value = 60, step = 6),
            shiny::checkboxInput(inputId = ns("optimize_u_stab"), value = FALSE, label = shiny::HTML("Optimize u<sub>stab</sub>")),
            shinyWidgets::pickerInput(inputId = ns("s_sel_temp"), label = "Use Temp level", choices = "", multiple = TRUE),
            shiny::hr(),
            sub_header("Save Report"),
            shiny::downloadButton(ns("s_Report"), label = "Download", style = "margin-bottom:16px;")
          )
        ),
        # $JL$ the surrounding div is required to include the empty HTML as a way to allow shinking and prevent the figure to be resized horizontally otherwise
        shiny::plotOutput(ns("s_plot"), height = "500px")
        # shiny::div(
        #   shiny::div(style = "display: inline-block;", shiny::plotOutput(ns("s_plot"), height = "500px", inline = TRUE)),
        #   shiny::div(style = "display: inline-block;", shiny::HTML(""))
        # )
      )
    )
  )

  # bslib::navset_card_pill(
  #   ...,
  #   id = NULL,
  #   selected = NULL,
  #   title = NULL,
  #   sidebar = NULL,
  #   header = NULL,
  #   footer = NULL,
  #   height = NULL,
  #   placement = c("above", "below"),
  #   full_screen = FALSE,
  #   wrapper = card_body
  # )

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

    # shiny::tabPanel(
    #   title = "altern-Panel",
    #   value = "tP_arrhenius",
    #   m_arrheniusUI(id = ns("arrhenius"))
    # )
  )
}

#' @noRd
page_StabilityServer <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    # server part of the arrhenius module
    arrhenius_out <- m_arrheniusServer(id = "arrhenius", rv = rv)

    #
    shiny::observeEvent(arrhenius_out$switch,
      {
        shiny::updateTabsetPanel(session = session, "StabilityPanel", selected = "loaded")
      },
      ignoreInit = TRUE
    )

    # # switch back and forth between stability 'main' and 'arrhenius' panels
    # shiny::observeEvent(input$s_switch_arrhenius,
    #   {
    #     shiny::updateTabsetPanel(session = session, "StabilityPanel", selected = "tP_arrhenius")
    #   },
    #   ignoreInit = TRUE
    # )

    shiny::observeEvent(getValue(rv, c("Stability", "data")), {
      tmp <- getValue(rv, c("Stability", "data"))
      # does the data contain Temp information (arrhenius model)
      test <- "Temp" %in% colnames(tmp)
      shinyjs::toggle(id = "s_sel_temp", condition = test)
      #shinyjs::toggle(id = "s_switch_arrhenius", condition = test)
      shinyjs::toggle(id = "arrhenius_panel", condition = test)
      if (test) {
        lev <- levels(factor(tmp[, "Temp"]))
        shinyWidgets::updatePickerInput(inputId = "s_sel_temp", choices = lev, selected = lev)
        #shiny::updateCheckboxGroupInput(inputId = "s_sel_temp", choices = lev, selected = lev, inline = TRUE)
      } else {
        shinyWidgets::updatePickerInput(inputId = "s_sel_temp", choices = "")
        #shiny::updateCheckboxGroupInput(inputId = "s_sel_temp", choices = "", inline = TRUE)
      }
    })

    shiny::observeEvent(rv$e_present(), {
      if (rv$e_present()["Stability"]) {
        shiny::updateTabsetPanel(session = session, "StabilityPanel", selected = "loaded")
      } else {
        message("[Stability] Show empty panel")
        shiny::updateTabsetPanel(session = session, "StabilityPanel", selected = "standby")
      }
    })

    # the complete data table of stability data as a local copy
    s_Data <- shiny::reactive({
      shiny::req(getValue(rv, c("Stability", "data")))
      s_dat <- getValue(rv, c("Stability", "data"))
      if (!is.factor(s_dat[, "analyte"])) s_dat[, "analyte"] <- factor(s_dat[, "analyte"], levels = unique(s_dat[, "analyte"]))
      if ("Temp" %in% colnames(s_dat)) {
        shiny::validate(shiny::need(expr = length(input$s_sel_temp) >= 1, message = "Please select a Temp level."))
        s_dat <- s_dat[as.character(s_dat[, "Temp"]) %in% input$s_sel_temp, ]
        shiny::validate(shiny::need(expr = diff(range(s_dat[, "time"])) > 0, message = "Please select Temp levels such that independent time points exist."))
      }
      tmp <- shiny::isolate(getValue(rv, c("Stability", "s_vals")))
      if (!is.null(tmp)) {
        # expected shelf life (t_cert) could be stored as an input parameter permanently in RData,
        # but having it in Tab.S1 (s_vals) from now on allows to restore a previous setting as well
        if ("t_cert" %in% colnames(tmp)) {
          shiny::updateSliderInput(
            inputId = "s_shelf_life",
            value = ifelse(is.finite(tmp[1, "t_cert"]), tmp[1, "t_cert"], 0)
          )
        }
      }
      return(s_dat)
    })

    # the summary of linear models per analyte to estimate u_stab
    s_vals <- shiny::reactive({
      shiny::req(s_Data(), input$s_shelf_life)
      out <- prepTabS1(x = s_Data(), time_fmt = input$time_fmt, t_cert = input$s_shelf_life, slope_of_means = "slope_of_means" %in% input$FigS1_options, mt = getValue(rv, c("General", "materialtabelle")), optimize_u_stab = input$optimize_u_stab)
      setValue(rv, c("Stability", "s_vals"), out)
      return(out)
    })

    # we need a local representation of the currently selected analyte in case that there is only a partial overlap between analytes in S and C modul
    S_analyte <- shiny::reactive({
      req(s_vals(), rv$cur_an)
      if (rv$e_present()["Certification"]) shinyjs::enable(id = "s_sel_dev") else shinyjs::disable(id = "s_sel_dev")
      shiny::validate(shiny::need(expr = rv$cur_an %in% as.character(s_vals()[, "analyte"]), message = paste("Analyte", rv$cur_an, "is not present in S data.")))
      rv$cur_an
    })

    # Tables
    s_tab1_current <- shiny::reactiveValues("row" = 1, "redraw" = 0)
    output$s_tab1 <- DT::renderDataTable({
      shiny::req(s_vals())
      s_tab1_current$redraw
      styleTabS1(x = s_vals(), mt = getValue(rv, c("General", "materialtabelle")), sr = s_tab1_current$row)
    })
    shiny::observeEvent(input$s_tab1_rows_selected,
      {
        if (is.null(input$s_tab1_rows_selected)) {
          # trigger a redraw of s_tab1 if the user deselects the current row
          s_tab1_current$redraw <- s_tab1_current$redraw + 1
        } else {
          if (s_tab1_current$row != input$s_tab1_rows_selected) {
            sel <- as.character(s_vals()[input$s_tab1_rows_selected, "analyte"])
            if (!identical(rv$cur_an, sel)) rv$cur_an <- sel
          }
        }
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    observeEvent(S_analyte(),
      {
        req(s_vals())
        # update view for currently selected analyte (trigger coming from C module or Arrhenius module)
        if (!(s_tab1_current$row == which(as.character(s_vals()[, "analyte"]) == S_analyte()))) {
          s_tab1_current$row <- which(as.character(s_vals()[, "analyte"]) == S_analyte())
        }
        if (!is.null(input$s_sel_dev)) {
          mt <- getValue(rv, c("General", "materialtabelle"))
          a <- S_analyte()
          test <- a %in% mt[, "analyte"] && is.finite(mt[which(mt[, "analyte"] == a), "mean"])
          if (rv$e_present()["Certification"]) shinyjs::enable(id = "s_sel_dev") else shinyjs::disable(id = "s_sel_dev")
        }
      },
      ignoreNULL = TRUE,
      ignoreInit = TRUE
    )

    # Fig.S1
    output$s_plot <- shiny::renderPlot({
      shiny::req(s_Data(), S_analyte())
      plot_lts_data(
        x = prepFigS1(
          s = s_Data(),
          an = S_analyte(),
          apm = getValue(rv, c("General", "apm")),
          U_Def = input$s_sel_dev,
          mt = getValue(rv, c("General", "materialtabelle"))
        ),
        type = as.numeric(input$plot_type),
        t_cert = input$s_shelf_life,
        slope_of_means = "slope_of_means" %in% input$FigS1_options,
        show_legend = "show_legend" %in% input$FigS1_options
      )
    })

    # u transfer
    m_TransferUServer(id = "s_transfer", rv = rv, type = "S")

    # download outputs
    output$s_Report <- shiny::downloadHandler(
      filename = function() {
        "Stability_report.html"
      },
      content = function(file) {
        rmdfile <- get_local_file("report_vorlage_stability.Rmd")
        # render the markdown file
        shiny::withProgress(
          expr = {
            incProgress(0.5)
            out <- rmarkdown::render(
              input = rmdfile,
              output_file = file,
              output_format = rmarkdown::html_document(),
              params = list(
                "Stability" = shiny::reactiveValuesToList(getValue(rv, "Stability"))
              ),
              envir = new.env(parent = globalenv())
            )
          },
          message = "Rendering Stability Report..."
        )
        return(out)
      }
    )

    # help modals
    shiny::observeEvent(input$fig1_link, {
      show_help("stability_plot")
    })
    shiny::observeEvent(input$tab_link, {
      show_help("stability_uncertainty")
    })
  })
}
