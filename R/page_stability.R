#' @title page_Stability
#' @name page_Stability
#'
#' @param id Id when called in module.
#' @param rv eCerto object.
#'
#' @return Will return UI and Server logic for the stability page.
#' @export
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    page_StabilityUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- eCerto::eCerto$new(eCerto::init_rv()) # initiate persistent variables
#'    shiny::isolate({eCerto::setValue(rv, c("Stability","data"), eCerto:::test_Stability_Excel() )})
#'    shiny::isolate({eCerto::setValue(rv, c("Stability","uploadsource"), "Excel" )})
#'    page_StabilityServer(id = "test", rv = rv)
#'  }
#' )
#' }
#'

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
              label = "Tab.1 Stability - calculation of uncertainty contribution"
            )
          ),
          DT::dataTableOutput(ns("s_vals"))
        ),
        shiny::column(width = 2, shiny::wellPanel(m_TransferUUI(id = ns("s_transfer"))))
      ),
      shiny::p(),
      shiny::fluidRow(
        shiny::column(width = 2, DT::dataTableOutput(ns("s_overview"))),
        shiny::column(
          width = 8,
          shiny::fluidRow(
            shiny::plotOutput(ns("s_plot")),
            shiny::uiOutput(ns("s_info"))
          )
        ),
        shiny::column(
          width = 2,
          shiny::wellPanel(
            shiny::uiOutput(outputId = ns("s_sel_analyte")),
            shiny::uiOutput(outputId = ns("s_sel_dev")),
            shiny::selectInput(inputId = ns("s_sel_temp"), label = "Use Temp level", choices = "", multiple = TRUE),
            #sub_header("Change View"),
            shiny::actionButton(inputId = ns("s_switch_arrhenius"), label = "Switch to Arrhenius")
            #p(),
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

#' @rdname page_Stability
#' @export
page_StabilityServer <- function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$tab_link,{
      help_the_user_modal("stability_uncertainty")
    })

    arrhenius_out <- m_arrheniusServer(id="arrhenius", rv=rv)
    shiny::observeEvent(arrhenius_out$switch, {
      shiny::updateTabsetPanel(session = session, "StabilityPanel", selected = "loaded")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$s_switch_arrhenius, {
      shiny::updateTabsetPanel(session = session, "StabilityPanel", selected = "tP_arrhenius")
    }, ignoreInit = TRUE)

    shiny::observeEvent(getValue(rv, c("Stability", "data")), {
      tmp <- getValue(rv, c("Stability", "data"))
      shinyjs::toggle(id = "s_sel_temp", condition = "Temp" %in% colnames(tmp))
      shinyjs::toggle(id = "s_switch_arrhenius", condition = "Temp" %in% colnames(tmp))
      if ("Temp" %in% colnames(tmp)) {
        lev <- levels(factor(tmp[,"Temp"]))
        shiny::updateSelectInput(inputId = "s_sel_temp", choices = lev, selected = lev)
      } else {
        shiny::updateSelectInput(inputId = "s_sel_temp", choices = "")
      }
    })

    shiny::observeEvent(getValue(rv, c("Stability","uploadsource")), {
      shiny::updateTabsetPanel(session = session,"StabilityPanel", selected = "loaded")
    })

    # # TODO This is for saving; Has to be transformed to setValue()
    # s_res <- reactive({
    #   # combine data for backup in a list
    #   if (is.null(getValue(rv,c("Stability","data")))) {
    #     return(list("Stability"=NULL))
    #   } else {
    #     return(list("Stability"=list(
    #       "s_file"=input$s_input_file[[1]],
    #       "s_sel_analyte"=input$s_sel_analyte,
    #       "s_dat"=getData("s_dat"),
    #       "s_vals"=getData("s_vals")
    #     )))
    #   }
    # }, label="debug_s_res")


    # the complete data table of stability data
    s_Data <- shiny::reactive({
      #shiny::req(input$s_sel_temp)
      s_dat <- getValue(rv,c("Stability","data"))
      if (!is.factor(s_dat[,"analyte"])) s_dat[,"analyte"] <- factor(s_dat[,"analyte"])
      if ("Temp" %in% colnames(s_dat) && input$s_sel_temp != "") {
        s_dat <- s_dat[as.character(s_dat[,"Temp"]) %in% input$s_sel_temp,]
      }
      return(s_dat)
    })

    # the summary of linear models per analyte to estimat U_stab
    s_vals <- shiny::reactive({
      shiny::req(s_Data())
      out <- plyr::ldply(split(s_Data(), s_Data()[,"analyte"]), function(x) {
        x_lm <- stats::lm(Value ~ Date, data=x)
        mon_diff <- max(mondf(x[,"Date"]))
        x_slope <- summary(x_lm)$coefficients[2,1:2]
        data.frame("mon_diff"=mon_diff, "slope"=x_slope[1], "SE_slope"=x_slope[2], "U_Stab"=abs(x_slope[1]*x_slope[2]))
      }, .id="analyte")
      setValue(rv, c("Stability","s_vals"), out)
      return(out)
    })

    # Specific UI and events
    output$s_sel_analyte <- shiny::renderUI({
      shiny::req(s_Data())
      lev <- levels(s_Data()[,"analyte"])
      shiny::selectInput(inputId = session$ns("s_sel_analyte"), label = "analyte", choices = lev)
    })

    # Tables
    output$s_overview <- DT::renderDataTable({
      shiny::req(s_Data(), input$s_sel_analyte)
      s <- s_Data()
      s[s[,"analyte"]==input$s_sel_analyte,c("Date","Value")]
    }, options = list(paging = TRUE, searching = FALSE), rownames=NULL)

    output$s_vals <- DT::renderDataTable({
        shiny::req(s_vals())
        s_vals_print <- s_vals()
        for (i in c("slope","SE_slope","U_Stab")) {
          s_vals_print[,i] <- pn(s_vals_print[,i], 4)
        }
        if (!is.null(getValue(rv, c("General", "materialtabelle")))) {
          c_vals <- getValue(rv, c("General", "materialtabelle"))
          s_vals_print[,"Present"] <- sapply(s_vals_print[,"analyte"], function(x) {
            ifelse(x %in% c_vals[,"analyte"], "Yes", "No")
          })
        }
        return(s_vals_print)
      }, options = list(dom = "t", pageLength=100), selection = list(mode="single", target="row"), rownames=NULL
    )

    shiny::observeEvent(input$s_vals_rows_selected, {
      sel <- as.character(s_vals()[input$s_vals_rows_selected,"analyte"])
      shiny::updateSelectInput(session = session, inputId = "s_sel_analyte", selected = sel)
    })

    shiny::observeEvent(input$s_sel_analyte, {
      # show/hide the input field to select a deviation type (will effect the Figure)
      shiny::req(input$s_sel_dev)
      mt <- getValue(rv, c("General", "materialtabelle"))
      a <- input$s_sel_analyte
      test <- a %in% mt[,"analyte"] && is.finite(mt[which(mt[,"analyte"]==a),"mean"])
      shinyjs::toggle(id="s_sel_dev", condition = test)
    })

    output$s_sel_dev <- shiny::renderUI({
      shiny::req(s_Data(), getValue(rv, c("General", "materialtabelle")), input$s_sel_analyte)
      mt <- getValue(rv, c("General", "materialtabelle"))
      a <- input$s_sel_analyte
      # show element only once mat_tab is available and analyte and mean value exist
      shiny::req(a %in% mt[,"analyte"] && is.finite(mt[which(mt[,"analyte"]==a),"mean"]))
      shiny::selectInput(inputId=session$ns("s_sel_dev"), label="deviation to show", choices=c("2s","U"), selected="2s")
    })

    output$s_info <- shiny::renderUI({
      # text info shown below the Figure
      shiny::req(s_Data(), input$s_sel_analyte)
      an <- input$s_sel_analyte
      aps <- getValue(rv, c("General", "apm"))
      U_type <- "2s"
      U_source <- "stability"
      if (!is.null(input$s_sel_dev) && an %in% names(aps) && aps[[an]][["confirmed"]]) {
        U_type <- input$s_sel_dev
        U_source <- "certification"
      }
      shiny::HTML(paste0("Figure shows mean and ", U_type, " of uploaded ", U_source, " data for analyte ", an, "."))
    })

    # Figure
    output$s_plot <- shiny::renderPlot({
      shiny::req(s_Data(), input$s_sel_analyte)
      s <- s_Data()
      an <- input$s_sel_analyte
      l <- s[,"analyte"]==an
      aps <- getValue(rv, c("General", "apm"))

      # Convert to format used in LTS modul
      # load SD and U from certification if available
      CertVal <- mean(s[l,"Value"], na.rm=T)
      U <- 2*stats::sd(s[l,"Value"], na.rm=T)
      U_Def <- "2s"
      if (!is.null(input$s_sel_dev) && an %in% names(aps) && aps[[an]][["confirmed"]]) {
        mt <- getValue(rv, c("General", "materialtabelle"))
        CertVal <- mt[mt[,"analyte"] %in% an, "cert_val"]
        U <- ifelse(input$s_sel_dev=="U", 1, 2) * mt[mt[,"analyte"] %in% an, ifelse(input$s_sel_dev=="U", "U", "sd")]
        U_Def <- input$s_sel_dev
      }
      x <- list("val"=s[l,],
                "def"=data.frame(
                  "CertVal" = CertVal,
                  "U"= U,
                  "U_Def" = U_Def,
                  "KW" = an,
                  "KW_Def" = ifelse("KW_Def" %in% colnames(s), unique(s[l,"KW_Def"])[1],"KW_Def"),
                  "KW_Unit" = ifelse("KW_Unit" %in% colnames(s), unique(s[l,"KW_Unit"])[1],"KW_Unit"),
                  stringsAsFactors = FALSE
                )
      )
      plot_lts_data(x=x)
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
        shiny::fluidRow(shiny::HTML("<p style=margin-bottom:-3%;><strong>Transfer 'U_stab' to column</strong></p>"), align="right"),
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

  })
}
