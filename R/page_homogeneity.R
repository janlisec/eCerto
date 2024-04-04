#' @name page_Homogeneity
#' @aliases page_HomogeneityUI
#' @aliases page_HomogeneityServer
#'
#' @title Homogeneity page
#'
#' @description \code{page_Homogeneity} is the module for handling Homogeneity Data
#'
#' @details not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv The session R6 object.
#'
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
      shiny::fluidRow(
        shiny::column(
          width = 10,
          shiny::strong(
            shiny::actionLink(
              inputId = ns("tab1_link"),
              label = "Tab.H1 - calculation of uncertainty contribution"
            )
          ),
          DT::dataTableOutput(ns("h_tab1"))
        ),
        shiny::column(
          width = 2,
          shiny::wellPanel(
            m_TransferUUI(ns("h_transfer")),
            shiny::checkboxInput(inputId = ns("h_adjust"), label = "P-value adjustment (bonferroni)", value = TRUE)
          )
        )
      ),
      shiny::p(),
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::strong(
            shiny::actionLink(
              inputId = ns("tab2_link"),
              label = "Tab.H2 - specimen stats"
            )
          ),
          DT::dataTableOutput(ns("h_tab2"))
        ),
        shiny::column(
          width = 7,
          shiny::fluidRow(
            shiny::strong(
              shiny::actionLink(
                inputId = ns("fig1_link"),
                label = "Fig.H1 - boxplot of specimen values"
              )
            ), shiny::p(),
            shiny::plotOutput(ns("h_boxplot"), inline = TRUE),
            shiny::plotOutput(ns("h_Fig.H2"), inline = TRUE),
            shiny::uiOutput(ns("h_txt"))
          )
        ),
        shiny::column(
          width = 2,
          shiny::wellPanel(
            shinyjs::hidden(shiny::selectInput(inputId = ns("h_sel_analyt"), label = "Row selected in Tab.1", choices = "")),
            sub_header("Save Report"),
            shiny::downloadButton(ns("h_Report"), label = "Download", style = "margin-bottom:10px;"),
            sub_header("Figure options"),
            shiny::textInput(inputId = ns("FigH1_xlab"), label = "x-axis label", value = "Flasche"),
            shiny::checkboxInput(inputId = ns("h_show_repID"), label = "Identify replicates in Fig.H1", value = FALSE),
            shiny::checkboxInput(inputId = ns("h_show_H2"), label = "Show combined analyte z-scores", value = FALSE)
          )
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
        message("[Homogeneity] Show empty panel")
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
      rv$cur_an
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
      out <- plyr::ldply(split(h_dat[, "value"], h_dat[, "Flasche"]), function(x) {
        data.frame("mean" = mean(x, na.rm = T), "sd" = stats::sd(x, na.rm = T), "n" = sum(is.finite(x)))
      }, .id = "Flasche")
      rownames(out) <- out[, "Flasche"]
      colnames(out) <- gsub("Flasche", input$FigH1_xlab, colnames(out))
      return(out)
    })

    precision <- shiny::reactive({
      shiny::req(input$h_sel_analyt)
      # message("[H] setting precision")
      prec <- 4
      an <- as.character(h_vals()[interaction(h_vals()[, "analyte"], h_vals()[, "H_type"]) == input$h_sel_analyt, "analyte"])
      apm <- getValue(rv, c("General", "apm"))
      if (an %in% names(apm)) {
        # prec <- apm[[an]][["precision_export"]]
        prec <- apm[[an]][["precision"]]
      }
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
        if (is.null(input$h_tab1_rows_selected)) {
          # trigger a redraw of h_tab1 if the user deselects the current row
          h_tab1_current$redraw <- h_tab1_current$redraw + 1
        } else {
          h_tab1_current$row <- input$h_tab1_rows_selected
          sel <- as.character(interaction(h_vals()[input$h_tab1_rows_selected, 1:2]))
          shiny::updateSelectInput(session = session, inputId = "h_sel_analyt", selected = sel)
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
    output$h_boxplot <- shiny::renderPlot(
      {
        shiny::req(h_Data(), input$h_sel_analyt, precision(), input$FigH1_xlab)
        prepFigH1(x = h_Data(), sa = input$h_sel_analyt, prec = precision(), xlab = input$FigH1_xlab, showIDs = input$h_show_repID)
      },
      height = 504,
      width = shiny::reactive({
        fig_width()
      })
    )

    output$h_Fig.H2 <- shiny::renderPlot(
      {
        shiny::req(h_Data(), precision(), input$FigH1_xlab, input$h_show_H2)
        prepFigH1(x = h_Data(), sa = NULL, prec = precision(), xlab = input$FigH1_xlab)
      },
      height = 504,
      width = shiny::reactive({
        fig_width()
      })
    )

    output$h_txt <- shiny::renderUI({
      shiny::req(h_vals(), input$h_sel_analyt)
      if (input$h_show_H2) {

      } else {
        h_statement(x = h_vals(), a = input$h_sel_analyt)
      }
    })

    # U transfer button module
    m_TransferUServer(id = "h_transfer", rv = rv, type = "H")

    # download outputs
    output$h_Report <- shiny::downloadHandler(
      # filename = function() { "Homogeneity_report.pdf" },
      filename = function() {
        "Homogeneity_report.html"
      },
      content = function(file) {
        rmdfile <- get_local_file("report_vorlage_homogeneity.Rmd")
        # render the markdown file
        shiny::withProgress(
          expr = {
            incProgress(0.5)
            out <- rmarkdown::render(
              input = rmdfile,
              output_file = file,
              # output_format = rmarkdown::pdf_document(),
              output_format = rmarkdown::html_document(),
              params = list(
                "Homogeneity" = shiny::reactiveValuesToList(getValue(rv, "Homogeneity")),
                "xlab" = input$FigH1_xlab,
                "precision" = rv$a_p("precision"),
                "adjust" = input$h_adjust
              ),
              envir = new.env(parent = globalenv())
            )
          },
          message = "Rendering Homogeneity Report..."
        )
        return(out)
      }
    )

    # help modals
    shiny::observeEvent(input$hom_help_modal, {
      show_help("homogeneity_uncertainty")
    })

    shiny::observeEvent(input$tab1_link, {
      show_help("homogeneity_uncertainty")
    })

    shiny::observeEvent(input$tab2_link, {
      show_help("homogeneity_specimen_stats")
    })

    shiny::observeEvent(input$fig1_link, {
      show_help("homogeneity_boxplot")
    })
  })
}
