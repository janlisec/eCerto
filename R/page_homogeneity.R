#' @title Homogeneity page
#' @description \code{page_Homogeneity} is the module for handling Homogeneity Data
#' @details not yet
#' @param id Name when called as a module in a shiny app.
#' @param rv The session R6 object.
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyjs::useShinyjs(),
#'       eCerto:::page_HomogeneityUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       rv <- eCerto:::test_rv()
#'       mt <- isolate(eCerto::getValue(rv, c("General", "materialtabelle")))
#'       attr(mt, "col_code") <- data.frame("ID" = "U", "Name" = "U")
#'       isolate(eCerto::setValue(rv, c("General", "materialtabelle"), mt))
#'       isolate(eCerto::setValue(rv, "Homogeneity", eCerto:::test_homog()))
#'       eCerto:::page_HomogeneityServer(
#'         id = "test",
#'         rv = rv
#'       )
#'     }
#'   )
#' }
#' @keywords internal
#' @noRd
page_HomogeneityUI <- function(id) {
  ns <- shiny::NS(id)

  tab_H1_panel <- bslib::card(
    #min_height = 500
    fill = FALSE,
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::strong(shiny::actionLink(inputId = ns("tab1_link"), label = "Tab.H1 - calculation of uncertainty contribution")),
      shiny::div(
        shiny::div(
          style = "float: right; margin-left: 15px;",
          m_TransferUUI(ns("h_transfer"))
        ),
        shiny::div(
          style = "float: left; margin-left: 15px;",
          shiny::checkboxInput(inputId = ns("h_adjust"), width = 130, label = shiny::HTML("P-value adjust-<br>ment (bonferroni)"), value = TRUE)
        )
      )
    ),
    bslib::card_body(
      shiny::div(DT::DTOutput(ns("h_tab1")))
    )
  )

  tab_H2_panel <- bslib::card(
    #min_height = 500
    #fill = FALSE,
    bslib::card_header(
      shiny::strong(shiny::actionLink(inputId = ns("tab2_link"), label = "Tab.H2 - specimen stats")),
    ),
    bslib::card_body(
      shiny::div(DT::DTOutput(ns("h_tab2")))
    )
  )

  fig_H1_panel <- bslib::card(
    id = ns("fig_H1_panel"),
    style = "resize:vertical;",
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::strong(shiny::actionLink(inputId = ns("fig1_link"), label = "Fig.H1 - boxplot of specimen values")),
      shiny::div(style = "float: left; margin-left: 15px;", shiny::downloadButton(ns("h_Report"), label = "Download Report"))
    ),
    bslib::card_body(
      fill = TRUE,
      gap = "0px",
      bslib::layout_sidebar(
        padding = 0,
        sidebar = bslib::sidebar(
          position = "right", open = "open", width = 360,
          shiny::div(
            shinyjs::hidden(shiny::selectInput(inputId = ns("h_sel_analyt"), label = "Row selected in Tab.1", choices = "")),
            sub_header("Fig.H1 options"),
            shiny::checkboxGroupInput(
              inputId = ns("FigH1_opt"), label = NULL,
              choices = list(
                "Identify replicates in Fig.H1" = "show_repID",
                "Show combined analyte z-scores" = "show_H2"
              )
            ),
            shiny::textInput(inputId = ns("FigH1_xlab"), label = "Edit x-axis label", value = "Flasche")
          )
        ),
        bslib::card_body(min_height = 400, padding = 0, gap = 0, shiny::plotOutput(ns("h_FigH1"))),
        bslib::card_body(min_height = 0, padding = 0, gap = 0, shiny::plotOutput(ns("h_FigH2")))
      )
    ),
    bslib::card_footer(
      shiny::uiOutput(ns("h_txt"))
    )
  )

  shiny::tabsetPanel(
    id = ns("HomogeneityPanel"),
    type = "hidden", # when nothing is loaded
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
          tab_H1_panel,
          tab_H2_panel
        ),
        shiny::tagList(
          fig_H1_panel
        ),
        col_widths =  bslib::breakpoints(
          sm = c(12, 12),
          xl = c(5, 7)
        )
      )
    )
  )
}

#' @noRd
page_HomogeneityServer <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- shiny::NS(id)

    # this is the local version of the homology data and parameters
    shiny::observeEvent(rv$e_present(), {
      if (rv$e_present()["Homogeneity"]) {
        shiny::updateTabsetPanel(session = session, "HomogeneityPanel", selected = "loaded")
      } else {
        shiny::updateTabsetPanel(session = session, "HomogeneityPanel", selected = "standby")
      }
    })

    # local version of input data table
    h_Data <- shiny::reactive({
      shiny::req(getValue(rv, c("Homogeneity", "data")))
      # whatever range is loaded from excel can be checked and transformed in here
      h_dat <- checkHdata(x = getValue(rv, c("Homogeneity", "data")))
      # update analyte select input (can be removed from App as selection is done in Tab.H1 by row)
      lev <- levels(interaction(h_dat[, "analyte"], h_dat[, "H_type"]))
      shiny::updateSelectInput(inputId = "h_sel_analyt", label = "Row selected in Tab.1", choices = lev, selected = lev[1])
      shinyjs::disable("h_sel_analyt")
      return(h_dat)
    })

    # keep rv$cur_an in sync with input$h_sel_analyt
    shiny::observeEvent(rv$cur_an, {
      req(h_vals())
      shiny::validate(shiny::need(expr = rv$cur_an %in% as.character(h_vals()[, "analyte"]), message = paste("Analyte", rv$cur_an, "is not present in H data.")))
      # was a different analyte selected in one of the other modules
      i <- input$h_tab1_rows_selected
      if (is.null(i) || rv$cur_an != as.character(h_vals()[i, "analyte"])) {
        cr <- which(as.character(h_vals()[, "analyte"])==rv$cur_an)
        if (length(cr)>1) {
          # try to match the previously selected H_type
          flt <- h_vals()[cr, "H_type"] == h_vals()[i, "H_type"]
          if (any(flt)) cr <- cr[which(flt)[1]] else cr <- cr[1]
        }
        h_tab1_current$row <- cr
      }
    })

    # local version of statistical values (Tab.H1)
    h_vals <- shiny::reactiveVal(NULL)

    shiny::observeEvent(h_Data(), {
      x <- prepTabH1(x = h_Data(), adjust = input$h_adjust)
      # set rv version
      setValue(rv, c("Homogeneity", "h_vals"), x)
      # set local version
      h_vals(x)
    })

    # apply multiple testing correction
    shiny::observeEvent(input$h_adjust,
      {
        x <- prepTabH1(x = h_Data(), adjust = input$h_adjust)
        # set rv version
        setValue(rv, c("Homogeneity", "h_vals"), x)
        # set local version
        h_vals(x)
      },
      ignoreInit = TRUE
    )

    # compute specimen means for Tab.H2
    tab_H2 <- shiny::reactive({
      shiny::req(h_Data(), input$h_sel_analyt)
      h_dat <- h_Data()
      h_dat <- h_dat[interaction(h_dat[, "analyte"], h_dat[, "H_type"]) == input$h_sel_analyt, , drop = FALSE]
      validate(need(expr = nrow(h_dat) >= 1, message = "Not enough data."))
      h_dat[, "Flasche"] <- factor(h_dat[, "Flasche"])
      out <- ldply_base(split(h_dat[, "value"], h_dat[, "Flasche"]), function(x) {
        data.frame("mean" = mean(x, na.rm = T), "sd" = stats::sd(x, na.rm = T), "n" = sum(is.finite(x)))
      }, .id = "Flasche")
      rownames(out) <- out[, "Flasche"]
      colnames(out) <- gsub("Flasche", input$FigH1_xlab, colnames(out))
      return(out)
    })

    precision <- shiny::reactive({
      shiny::req(input$h_sel_analyt)
      prec <- 4
      an <- as.character(h_vals()[interaction(h_vals()[, "analyte"], h_vals()[, "H_type"]) == input$h_sel_analyt, "analyte"])
      apm <- getValue(rv, c("General", "apm"))
      if (an %in% names(apm)) { prec <- apm[[an]][["precision"]] }
      return(prec)
    })

    # Tables
    h_tab1_current <- shiny::reactiveValues("row" = 1, "redraw" = 0)
    output$h_tab1 <- DT::renderDataTable({
      shiny::req(h_vals())
      # watch the reactiveVal 'redraw' to avoid the user deselecting all rows
      h_tab1_current$redraw
      dt <- styleTabH1(
        x = h_vals(),
        mt = getValue(rv, c("General", "materialtabelle")),
        prec = rv$a_p("precision"),
        output = "dt", cr = h_tab1_current$row
      )
      return(dt)
    })
    shiny::observeEvent(input$h_tab1_rows_selected,
      {
        i <- input$h_tab1_rows_selected
        if (is.null(i)) {
          # trigger a redraw of h_tab1 if the user deselects the current row
          h_tab1_current$redraw <- h_tab1_current$redraw + 1
        } else {
          h_tab1_current$row <- i
          sel <- as.character(interaction(h_vals()[i, 1:2]))
          shiny::updateSelectInput(session = session, inputId = "h_sel_analyt", selected = sel)
          rv$cur_an <- as.character(h_vals()[i, "analyte"])
        }
        # shinyjs::disable(id = "h_sel_analyt")
      },
      ignoreNULL = FALSE
    )

    output$h_tab2 <- DT::renderDataTable({
      shiny::req(tab_H2(), precision())
      styleTabH2(x = tab_H2(), precision = precision())
    })

    # Plots & Print
    fig_width <- shiny::reactive({
      shiny::req(h_Data(), input$h_sel_analyt)
      x <- h_Data()[, c("analyte", "H_type", "Flasche")]
      calc_bxp_width(n = length(levels(factor(x[interaction(x[, 1], x[, 2]) == input$h_sel_analyt, 3]))))
    })

    output$h_FigH1 <- shiny::renderPlot(
      {
        shiny::req(h_Data(), input$h_sel_analyt, precision())
        prepFigH1(x = h_Data(), sa = input$h_sel_analyt, prec = precision(), xlab = input$FigH1_xlab, showIDs = "show_repID" %in% input$FigH1_opt)
      },
      # [JL] height and width needs to be fixed as long as we render the figure as inline
      #height = 500,
      width = shiny::reactive({ fig_width() })
    )

    output$h_FigH2 <- shiny::renderPlot(
      {
        shiny::req(h_Data(), precision())
        prepFigH1(x = h_Data(), sa = NULL, prec = 2, xlab = input$FigH1_xlab)
      },
      #height = 500,
      width = shiny::reactive({ fig_width() })
    )

    shiny::observeEvent(input$FigH1_opt, {
      shinyjs::toggle(id = "h_FigH2", condition = "show_H2" %in% input$FigH1_opt)
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    output$h_txt <- shiny::renderUI({
      shiny::req(h_vals(), input$h_sel_analyt)
      if ("show_H2" %in% input$FigH1_opt) {
        shiny::HTML("Combined analyte z-scores allow to identify a systematic outlier item more robustly.")
      } else {
        h_statement(x = h_vals(), a = input$h_sel_analyt)
      }
    })

    # U transfer button module
    m_TransferUServer(id = "h_transfer", rv = rv, type = "H")

    # download reports
    output$h_Report <- shiny::downloadHandler(
      filename = function() { "Homogeneity_report.html" },
      content = function(file) {
        render_report_H(file = file, rv = rv, xlab = input$FigH1_xlab, adjust = input$h_adjust)
      }
    )

    # help modals
    shiny::observeEvent(input$hom_help_modal, { show_help("homogeneity_uncertainty") })
    shiny::observeEvent(input$tab1_link, { show_help("homogeneity_uncertainty") })
    shiny::observeEvent(input$tab2_link, { show_help("homogeneity_specimen_stats") })
    shiny::observeEvent(input$fig1_link, { show_help("homogeneity_boxplot") })

  })
}
