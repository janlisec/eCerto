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
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    shinyjs::useShinyjs(),
#'    eCerto:::page_StabilityUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    #rv <- eCerto::eCerto$new(eCerto:::init_rv()) # initiate persistent variables
#'    #shiny::isolate({eCerto::setValue(rv, c("Stability","data"), eCerto:::test_Stability_Excel() )})
#'    rv <- eCerto:::test_rv(type = "SR3")
#'    shiny::isolate(eCerto::setValue(rv, c("Stability", "data"), eCerto:::test_Stability_Arrhenius()))
#'    eCerto:::page_StabilityServer(id = "test", rv = rv)
#'  }
#' )
#' }

page_StabilityUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabsetPanel(
    id = ns("StabilityPanel"),
    type = "hidden",
    # when nothing is loaded
    shiny::tabPanel(
      title = "standby-Panel",
      value  = "standby",
      "nothing has uploaded yet"),
    # when something is loaded
    shiny::tabPanel(
      title = "active-Panel",
      value = "loaded",
      shiny::fluidRow(
        shiny::column(
          width = 10,
          sub_header(
            shiny::actionLink(
              inputId = ns("tab_link"),
              label = "Tab.S1 - calculation of uncertainty contribution"
            )
          ),
          DT::dataTableOutput(ns("s_tab1"))
        ),
        shiny::column(
          width = 2,
          shiny::wellPanel(
            shiny::sliderInput(inputId = ns("s_shelf_life"), label = "Expected shelf life [Month]", min = 0, max = 120, value = 60, step = 6),
            m_TransferUUI(id = ns("s_transfer")),
            shinyjs::hidden(shiny::radioButtons(inputId = ns("time_fmt"), label = "Time format in lm", choices = c("mon", "day"), selected = "mon"))
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 10,
            sub_header(shiny::actionLink(inputId = ns("fig1_link"), label = "Fig.S1 - linear model plot")),
            shiny::plotOutput(ns("s_plot"), height = "500px"),
        ),
        shiny::column(
          width = 2,
          shiny::wellPanel(
            sub_header("Save Report"),
            shiny::downloadButton(ns("s_Report"), label="Download"),
            shiny::p(),
            sub_header("Fig.S1 Options"),
            shiny::checkboxGroupInput(inputId = ns("FigS1_options"), label = NULL, choices = list("Average by Day" = "slope_of_means", "Annotate plot" = "show_legend")),
            shiny::div(style = "margin-top: -10px;", shiny::radioButtons(inputId = ns("plot_type"), label = NULL, choices = list("standard"=1, "adjusted"=3), inline = TRUE)),
            shiny::div(style = "margin-top: -10px;", shiny::radioButtons(inputId = ns("s_sel_dev"), label = NULL, choices = list("2s"="2s", "U_abs"="U"), inline = TRUE)),
            shiny::checkboxGroupInput(inputId = ns("s_sel_temp"), label = "Use Temp level", choices = "", inline = TRUE),
            shiny::actionButton(inputId = ns("s_switch_arrhenius"), label = "Show Arrhenius", style = "width: 100%; max-width: 160px; font-weight: 700; background-color: rgb(0,175,240); margin-bottom: 10px;")
          )
        ),
      )
    ),
    shiny::tabPanel(
      title = "altern-Panel",
      value = "tP_arrhenius",
      m_arrheniusUI(id=ns("arrhenius"))
    )
  )
}

#' @noRd
page_StabilityServer <- function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    # server part of the arrhenius module
    arrhenius_out <- m_arrheniusServer(id="arrhenius", rv=rv)

    #
    shiny::observeEvent(arrhenius_out$switch, {
      shiny::updateTabsetPanel(session = session, "StabilityPanel", selected = "loaded")
    }, ignoreInit = TRUE)

    # switch back and forth between stability 'main' and 'arrhenius' panels
    shiny::observeEvent(input$s_switch_arrhenius, {
      shiny::updateTabsetPanel(session = session, "StabilityPanel", selected = "tP_arrhenius")
    }, ignoreInit = TRUE)

    shiny::observeEvent(getValue(rv, c("Stability", "data")), {
      tmp <- getValue(rv, c("Stability", "data"))
      # does the data contain Temp information (arrhenius model)
      test <- "Temp" %in% colnames(tmp)
      shinyjs::toggle(id = "s_sel_temp", condition = test)
      shinyjs::toggle(id = "s_switch_arrhenius", condition = test)
      if (test) {
        lev <- levels(factor(tmp[,"Temp"]))
        #shiny::updateSelectInput(inputId = "s_sel_temp", choices = lev, selected = lev)
        shiny::updateCheckboxGroupInput(inputId = "s_sel_temp", choices = lev, selected = lev, inline = TRUE)
      } else {
        #shiny::updateSelectInput(inputId = "s_sel_temp", choices = "")
        shiny::updateCheckboxGroupInput(inputId = "s_sel_temp", choices = "", inline = TRUE)
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
      if (!is.factor(s_dat[,"analyte"])) s_dat[,"analyte"] <- factor(s_dat[,"analyte"], levels=unique(s_dat[,"analyte"]))
      if ("Temp" %in% colnames(s_dat)) {
        shiny::validate(shiny::need(expr = length(input$s_sel_temp) >= 1, message = "Please select a Temp level."))
        s_dat <- s_dat[as.character(s_dat[,"Temp"]) %in% input$s_sel_temp,]
        shiny::validate(shiny::need(expr = diff(range(s_dat[,"time"]))>0, message = "Please select Temp levels such that independent time points exist."))
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
      out <- prepTabS1(x = s_Data(), time_fmt = input$time_fmt, t_cert = input$s_shelf_life, slope_of_means = "slope_of_means" %in% input$FigS1_options)
      setValue(rv, c("Stability", "s_vals"), out)
      return(out)
    })

    # we need a local representation of the currently selected analyte in case that there is only a partial overlap between analytes in S and C modul
    S_analyte <- shiny::reactive({
      req(s_vals(), rv$cur_an)
      if (rv$e_present()["Certification"]) shinyjs::enable(id = "s_sel_dev") else shinyjs::disable(id = "s_sel_dev")
      shiny::validate(shiny::need(expr = rv$cur_an %in% as.character(s_vals()[,"analyte"]), message = paste("Analyte", rv$cur_an, "is not present in S data.")))
      rv$cur_an
    })

    # Tables
    s_tab1_current <- shiny::reactiveValues("row"=1, "redraw"=0)
    output$s_tab1 <- DT::renderDataTable({
      shiny::req(s_vals())
      s_tab1_current$redraw
      styleTabS1(x = s_vals(), mt = getValue(rv, c("General", "materialtabelle")), sr = s_tab1_current$row)
    })
    shiny::observeEvent(input$s_tab1_rows_selected, {
      if (is.null(input$s_tab1_rows_selected)) {
        # trigger a redraw of s_tab1 if the user deselects the current row
        s_tab1_current$redraw <- s_tab1_current$redraw+1
      } else {
        if (s_tab1_current$row!=input$s_tab1_rows_selected) {
          sel <- as.character(s_vals()[input$s_tab1_rows_selected,"analyte"])
          if (!identical(rv$cur_an, sel)) rv$cur_an <- sel
        }
      }
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(S_analyte(), {
      req(s_vals())
      # update view for currently selected analyte (trigger coming from C module or Arrhenius module)
      if (!(s_tab1_current$row == which(as.character(s_vals()[,"analyte"])==S_analyte()))) {
        s_tab1_current$row <- which(as.character(s_vals()[,"analyte"])==S_analyte())
      }
      if (!is.null(input$s_sel_dev)) {
        mt <- getValue(rv, c("General", "materialtabelle"))
        a <- S_analyte()
        test <- a %in% mt[,"analyte"] && is.finite(mt[which(mt[,"analyte"]==a),"mean"])
        if (rv$e_present()["Certification"]) shinyjs::enable(id = "s_sel_dev") else shinyjs::disable(id = "s_sel_dev")
      }

    }, ignoreNULL = TRUE, ignoreInit = TRUE)

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
      filename = function() { "Stability_report.html" },
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
    shiny::observeEvent(input$fig1_link,{ show_help("stability_plot") })
    shiny::observeEvent(input$tab_link,{ show_help("stability_uncertainty") })

  })
}
