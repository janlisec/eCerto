#' @title STABILITY MODULE
#' @name mod_Stability
#'
#' @param id Id when called in module.
#'
#' @return Will return UI and Server logic for the stability tab.
#' @export
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_StabilityUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- ecerto::reactiveClass$new(ecerto::init_rv()) # initiate persistent variables
#'    shiny::isolate({ecerto::setValue(rv, c("Stability","data"), ecerto:::test_Stability_Excel() )})
#'    shiny::isolate({ecerto::setValue(rv, c("Stability","uploadsource"), "Excel")})
#'    datreturn <- ecerto:::test_datreturn()
#'    m_StabilityServer(
#'      id = "test",
#'      rv = rv,
#'      datreturn = datreturn
#'    )
#'  }
#' )
#' }
#'

m_StabilityUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
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
          shiny::column(width = 2, m_TransferUUI(id = ns("s_transfer")))
        ),
        shiny::p(),
        shiny::fluidRow(
          shiny::column(width = 4, DT::dataTableOutput(ns("s_overview"))),
          shiny::column(
            width = 8,
            shiny::fluidRow(
              shiny::column(6,shiny::uiOutput(ns("s_sel_analyte"))),
              shiny::column(6,shiny::uiOutput(ns("s_sel_dev")))),
            shiny::fluidRow(
              shiny::plotOutput(ns("s_plot")),
              shiny::uiOutput(ns("s_info"))
            )
          )
        )
      )
    )
  )
}

#' @rdname mod_Stability
#' @param rv reactiveClass object.
#' @param datreturn reactiveClass object.
#' @export
m_StabilityServer <- function(id, rv, datreturn) {

  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$tab_link,{
      help_the_user("stability_uncertainty")
    })

    # Upload Notification. Since "uploadsource" is invalidated also when other
    # parameters within Stability are changed (because of the reactiveValues
    # thing), it has to be checked if it has changed value since the last change
    # to verify an upload
    uploadsource <- shiny::reactiveVal(NULL)
    shiny::observeEvent(ecerto::getValue(rv,c("Stability","uploadsource")),{
      o.upload <- ecerto::getValue(rv,c("Stability","uploadsource"))
      # assign upload source if (a) hasn't been assigned yet or (b), if not
      # null, has changed since the last time, for example because other data
      # source has been uploaded
      if(is.null(uploadsource()) || uploadsource() != o.upload ){
        uploadsource(o.upload)
        shiny::updateTabsetPanel(session = session,"StabilityPanel", selected = "loaded")
        message("Stability: Uploadsource changed to ", shiny::isolate(ecerto::getValue(rv,c("Stability","uploadsource"))))
        #browser()
        s_dat <- ecerto::getValue(rv,c("Stability","data"))
        s_vals <- plyr::ldply(split(s_dat, s_dat[,"analyte"]), function(x) {
          x_lm <- stats::lm(Value ~ Date, data=x)
          mon_diff <- max(ecerto::mondf(x[,"Date"]))
          x_slope <- summary(x_lm)$coefficients[2,1:2]
          data.frame("mon_diff"=mon_diff, "slope"=x_slope[1], "SE_slope"=x_slope[2], "U_Stab"=abs(x_slope[1]*x_slope[2]))
        }, .id="analyte")
        ecerto::setValue(rv, c("Stability","s_vals"), s_vals)
      }
    })

    # # TODO This is for saving; Has to be transformed to ecerto::setValue()
    # s_res <- reactive({
    #   # combine data for backup in a list
    #   if (is.null(ecerto::getValue(rv,c("Stability","data")))) {
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


    s_Data <- shiny::reactive({
      s_dat <- ecerto::getValue(rv,c("Stability","data"))
      if (!is.factor(s_dat[,"analyte"])) s_dat[,"analyte"] <- factor(s_dat[,"analyte"])
      return(s_dat)
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
      s <- ecerto::getValue(rv,c("Stability","data"))
      s[s[,"analyte"]==input$s_sel_analyte,c("Date","Value")]
    }, options = list(paging = TRUE, searching = FALSE), rownames=NULL)

    output$s_vals <- DT::renderDataTable({
        shiny::req(s_Data())
        s_vals <- ecerto::getValue(rv, c("Stability","s_vals"))
        for (i in c("slope","SE_slope","U_Stab")) {
          s_vals[,i] <- ecerto::pn(s_vals[,i], 4)
        }
        if (!is.null(ecerto::getValue(datreturn,"mater_table"))) {
          c_vals <- ecerto::getValue(datreturn,"mater_table")
          s_vals[,"Present"] <- sapply(s_vals[,"analyte"], function(x) {
            ifelse(x %in% c_vals[,"analyte"], "Yes", "No")
          })
        }
        return(s_vals)
      },
      options = list(dom = "t", pageLength=shiny::isolate(nrow(ecerto::getValue(rv,c("Stability","s_vals"))))),
      selection = list(mode="single", target="row"), rownames=NULL
    )

    shiny::observeEvent(input$s_vals_rows_selected, {
      sel <- as.character(ecerto::getValue(rv,c("Stability","s_vals"))[input$s_vals_rows_selected,"analyte"])
      shiny::updateSelectInput(session = session, inputId = "s_sel_analyte", selected = sel)
    })

    shiny::observeEvent(input$s_sel_analyte, {
      # show/hide the input field to select a deviation type (will effect the Figure)
      shiny::req(input$s_sel_dev)
      mt <- ecerto::getValue(datreturn,"mater_table")
      a <- input$s_sel_analyte
      test <- a %in% mt[,"analyte"] && is.finite(mt[which(mt[,"analyte"]==a),"mean"])
      shinyjs::toggle(id="s_sel_dev", condition = test)
    })

    output$s_sel_dev <- shiny::renderUI({
      shiny::req(s_Data(), ecerto::getValue(datreturn,"mater_table"), input$s_sel_analyte)
      mt <- ecerto::getValue(datreturn,"mater_table")
      a <- input$s_sel_analyte
      # show element only once mat_tab is available and analyte and mean value exist
      shiny::req(a %in% mt[,"analyte"] && is.finite(mt[which(mt[,"analyte"]==a),"mean"]))
      shiny::selectInput(inputId=session$ns("s_sel_dev"), label="deviation to show", choices=c("2s","U"), selected="2s")
    })

    output$s_info <- shiny::renderUI({
      # text info shown below the Figure
      shiny::req(s_Data(), input$s_sel_analyte)
      U_type <- ifelse(is.null(input$s_sel_dev), "2s", input$s_sel_dev)
      U_source <- ifelse(is.null(input$s_sel_dev), "stability", "certification")
      shiny::HTML(paste0("Figure shows mean and ", U_type, " of uploaded ", U_source, " data for ", input$s_sel_analyte, "."))
    })

    # Figure
    output$s_plot <- shiny::renderPlot({
      shiny::req(s_Data(), input$s_sel_analyte)
      s <- s_Data()
      l <- s[,"analyte"]==input$s_sel_analyte

      # Convert to format used in LTS modul
      # load SD and U from certification if available
      CertVal <- mean(s[l,"Value"], na.rm=T)
      U <- 2*stats::sd(s[l,"Value"], na.rm=T)
      U_Def <- "2s"
      if (!is.null(input$s_sel_dev)) { # !is.null(input$sel_analyt) &
        cert_vals <- ecerto::getValue(datreturn,"mater_table")
        if (any(cert_vals[,"analyte"] %in% input$s_sel_analyte)) {
          CertVal <- cert_vals[cert_vals[,"analyte"] %in% input$s_sel_analyte,"cert_val"]
          U <- ifelse(input$s_sel_dev=="U", 1, 2)*cert_vals[cert_vals[,"analyte"] %in% input$s_sel_analyte, ifelse(input$s_sel_dev=="U", "U", "sd")]
          U_Def <- input$s_sel_dev
        }
      }
      x <- list("val"=s[l,],
                "def"=data.frame(
                  "CertVal" = CertVal,
                  "U"= U,
                  "U_Def" = U_Def,
                  "KW" = input$s_sel_analyte,
                  "KW_Def" = ifelse("KW_Def" %in% colnames(s), unique(s[l,"KW_Def"])[1],"KW_Def"),
                  "KW_Unit" = ifelse("KW_Unit" %in% colnames(s), unique(s[l,"KW_Unit"])[1],"KW_Unit"),
                  stringsAsFactors = FALSE
                )
      )
      ecerto::plot_lts_data(x=x)
    })

    # The Dropdown-Menu to select the column of materialtabelle to transfer to
    output$s_transfer_ubb <- shiny::renderUI({
      cert_vals <- ecerto::getValue(datreturn,"mater_table")
      shiny::validate(shiny::need(cert_vals, message = "Please upload certification data to transfer Uncertainty values"))

      cc <- attr(cert_vals, "col_code")
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
      dat = shiny::reactive({ecerto::getValue(rv, c("Stability","s_vals"))}),
      mat_tab = shiny::reactive({ecerto::getValue(datreturn, "mater_table")})
    )
    shiny::observeEvent(s_transfer_U$changed, {
      message("Stability: observeEvent(s_transfer_U)")
      ecerto::setValue(datreturn, "mater_table", s_transfer_U$value)
      setValue(datreturn, "transfer", 1) # trigger panel change in app_server
    }, ignoreInit = TRUE)

  })
}