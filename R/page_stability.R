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
          shiny::strong(
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
            m_TransferUUI(id = ns("s_transfer")),
            shinyjs::hidden(shiny::radioButtons(inputId = ns("time_fmt"), label = "Time format in lm", choices = c("mon", "day"), selected = "mon"))
          )
        )
      ),
      shiny::p(),
      shiny::fluidRow(
        shiny::column(width = 2, DT::dataTableOutput(ns("s_tab2"))),
        shiny::column(
          width = 8,
          shiny::fluidRow(
            shiny::strong(
              shiny::actionLink(
                inputId = ns("fig1_link"),
                label = "Fig.S1 - linear model plot"
              )
            ), shiny::p(),
            shiny::plotOutput(ns("s_plot"), height = "500px"),
            shiny::uiOutput(ns("s_info"))
          )
        ),
        shiny::column(
          width = 2,
          shiny::wellPanel(
            #shiny::uiOutput(outputId = ns("s_sel_dev")),
            shiny::radioButtons(inputId = ns("s_sel_dev"), label = "Deviation type", choices = list("2s"="2s", "U_abs"="U"), inline = TRUE),
            shiny::numericInput(inputId = ns("s_shelf_life"), label = "Expected shelf life [Month]", value = 60, min = 0),
            shiny::radioButtons(inputId = ns("plot_type"), label = "Plot type", choices = list("standard"=1, "adjusted"=3), inline = TRUE),
            shiny::selectInput(inputId = ns("s_sel_temp"), label = "Use Temp level", choices = "", multiple = TRUE),
            shiny::actionButton(inputId = ns("s_switch_arrhenius"), label = "Switch to Arrhenius")
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
        shiny::updateSelectInput(inputId = "s_sel_temp", choices = lev, selected = lev)
      } else {
        shiny::updateSelectInput(inputId = "s_sel_temp", choices = "")
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
      shiny::req(getValue(rv, c("Stability","data")))
      s_dat <- getValue(rv, c("Stability","data"))
      if (!is.factor(s_dat[,"analyte"])) s_dat[,"analyte"] <- factor(s_dat[,"analyte"], levels=unique(s_dat[,"analyte"]))
      if ("Temp" %in% colnames(s_dat)) {
        shiny::validate(shiny::need(expr = input$s_sel_temp != "", message = "Please select a Temp level."))
        s_dat <- s_dat[as.character(s_dat[,"Temp"]) %in% input$s_sel_temp,]
        shiny::validate(shiny::need(expr = diff(range(s_dat[,"time"]))>0, message = "Please select Temp levels such that independent time points exist."))
      }

      return(s_dat)
    })

    # the summary of linear models per analyte to estimate u_stab
    s_vals <- shiny::reactive({
      shiny::req(s_Data(), input$s_shelf_life)
      out <- prepTabS1(x = s_Data(), time_fmt = input$time_fmt, t_cert = input$s_shelf_life)
      setValue(rv, c("Stability","s_vals"), out)
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
    output$s_tab2 <- DT::renderDataTable({
      shiny::req(s_Data(), S_analyte())
      dt <- DT::datatable(
        data = s_Data()[s_Data()[,"analyte"]==S_analyte(), c("Date", "Value")],
        options = list(paging = TRUE, searching = FALSE), rownames = NULL, selection = "none"
      )
      prec <- try(getValue(rv, c("General","apm"))[[S_analyte()]][["precision"]])
      prec <- ifelse(!is.null(prec) && is.finite(prec), prec, 4)
      dt <- DT::formatCurrency(table = dt, columns = 2, currency = "", digits = prec)
      return(dt)
    })

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
      # show/hide the input field to select a deviation type (will effect the Figure)
      # if (!is.null(input$s_sel_dev)) {
      #   browser()
      #   mt <- getValue(rv, c("General", "materialtabelle"))
      #   a <- S_analyte()
      #   test <- a %in% mt[,"analyte"] && is.finite(mt[which(mt[,"analyte"]==a),"mean"])
      #   shinyjs::toggle(id="s_sel_dev", condition = test)
      # }
      if (!is.null(input$s_sel_dev)) {
        mt <- getValue(rv, c("General", "materialtabelle"))
        a <- S_analyte()
        test <- a %in% mt[,"analyte"] && is.finite(mt[which(mt[,"analyte"]==a),"mean"])
        if (rv$e_present()["Certification"]) shinyjs::enable(id = "s_sel_dev") else shinyjs::disable(id = "s_sel_dev")
      }

    }, ignoreNULL = TRUE, ignoreInit = TRUE)

    # output$s_sel_dev <- shiny::renderUI({
    #   shiny::req(s_Data(), getValue(rv, c("General", "materialtabelle")), S_analyte())
    #   mt <- getValue(rv, c("General", "materialtabelle"))
    #   # show element only once mat_tab is available and analyte and mean value exist
    #   shiny::req(S_analyte() %in% mt[,"analyte"] && is.finite(mt[which(mt[,"analyte"]==S_analyte()),"mean"]))
    #   browser()
    #   shiny::selectInput(inputId=session$ns("s_sel_dev"), label="deviation to show", choices=c("2s","U"), selected="2s")
    # })

    output$s_info <- shiny::renderUI({
      # text info shown below the Figure
      shiny::req(s_Data(), S_analyte())
      an <- S_analyte()
      aps <- getValue(rv, c("General", "apm"))
      U_type <- "2s"
      U_source <- "stability"
      U_tab <- "(Fig.S1)"
      #if (!is.null(input$s_sel_dev) && an %in% names(aps) && aps[[an]][["confirmed"]]) {
      if (rv$e_present()["Certification"] && an %in% names(aps) && aps[[an]][["confirmed"]]) {
        U_type <- input$s_sel_dev
        U_source <- "certification"
        U_tab <- "(Tab.C3)"
      }
      shiny::div(
        style = "height: 80px; padding-top: 10px",
        shiny::HTML(paste0("Figure shows mean and ", U_type, " of uploaded ", U_source, " data ", U_tab, " for analyte ", an, "."))
      )
    })

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
        t_cert = input$s_shelf_life
      )
    })

    # The Dropdown-Menu to select the column of materialtabelle to transfer to
    output$s_transfer_ubb <- shiny::renderUI({
      mt <- getValue(rv, c("General", "materialtabelle"))
      shiny::validate(shiny::need(mt, message = "Please upload certification data to transfer Uncertainty values"))

      cc <- attr(mt, "col_code")
      test <- nrow(cc)>0 && any(substr(cc[, "ID"], 1, 1) == "U")
      shiny::validate(shiny::need(test, message = "Please specify a U column in material table to transfer Uncertainty values"))

      shiny::column(
        width = 12,
        shiny::fluidRow(shiny::HTML("<p style=margin-bottom:-3%;><strong>Transfer 'u_stab' to column</strong></p>"), align="right"),
        shiny::selectInput(
          inputId=session$ns("s_transfer_ubb"),
          label="",
          width='100%',
          selectize=TRUE,
          choices=cc[substr(cc[,"ID"],1,1)=="U","Name"]
        ),
        shiny::fluidRow(shiny::actionButton(inputId = session$ns("s_transfer_ubb_button"), label = "Transfer Now!"), align="right")
      )
    })

    # allow transfer of U values
    s_transfer_U <- m_TransferUServer(
      id = "s_transfer",
      dat = s_vals,
      mat_tab = shiny::reactive({getValue(rv, c("General", "materialtabelle"))})
    )
    shiny::observeEvent(s_transfer_U$changed, {
      message("Stability: observeEvent(s_transfer_U)")
      setValue(rv, c("General","materialtabelle"), s_transfer_U$value)
    }, ignoreInit = TRUE)

    # render help files
    shiny::observeEvent(input$fig1_link,{
      show_help("stability_plot")
    })

    shiny::observeEvent(input$tab_link,{
      show_help("stability_uncertainty")
    })

  })
}
