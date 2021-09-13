#' @name mod_Homogeneity
#' @aliases m_HomogeneityUI
#' @aliases m_HomogeneityServer
#'
#' @title HOMOGENEITY MODULE
#'
#' @description \code{m_Homogeneity} is the module for handling Homogeneity Data
#'
#' @details not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param homog The Homogeneity table - shiny::reactive({getValue(rv,"Homogeneity")}).
#' @param cert The Certification table - shiny::reactive({getValue(rv,"Certification")}).
#' @param datreturn session-only reactiveValues --> for transfer Module
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_HomogeneityUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- reactiveClass$new(init_rv()) # initiate persistent variables
#'    datreturn = ecerto:::test_datreturn()
#'    m_HomogeneityServer(
#'      id = "test",
#'      homog = shiny::reactive({test_homog()}),
#'      cert = shiny::reactive({test_certification()}),
#'      datreturn = datreturn
#'    )
#'  }
#' )
#' }
#'
#' @return h_vals = The Homogeneity data (not the transferred ones yet)
#' @rdname mod_Homogeneity
#' @export

m_HomogeneityUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tabsetPanel(
    id = ns( "HomogeneityPanel"),
    type = "hidden", # when nothing is loaded
    shiny::tabPanel(
      title = "standby-Panel",
      value  = "standby",
      "nothing has uploaded yet"
    ),
    # when something is loaded
    shiny::tabPanel(
      title = "active-Panel",
      value = "loaded",
      #shiny::wellPanel(m_TransferHomogeneityUI(ns("trH"))),
      shiny::fluidRow(
        shiny::column(
          width = 10,
          shiny::strong(
            shiny::actionLink(
              inputId = ns("tab_link"),
              label = "Tab.1 Homogeneity - calculation of uncertainty contribution"
            )
          ),
          DT::dataTableOutput(ns("h_vals"))
        ),
        shiny::column(2, m_TransferUUI(ns("h_transfer")))
      ),
      shiny::p(),
      shiny::fluidRow(
        shiny::column(
          width = 3,
          DT::dataTableOutput(ns("h_overview_stats"))
        ),
        shiny::column(
          width = 7,
          shiny::fluidRow(
            shiny::plotOutput(ns("h_boxplot"), inline=TRUE),
            shiny::uiOutput(ns("h_statement2"))
          )
        ),
        shiny::column(
          width = 2,
          shiny::uiOutput(ns("h_sel_analyt")),
          shiny::numericInput(inputId=ns("h_Fig_width"), label="Figure Width", value=650),
          shiny::numericInput(inputId=ns("h_precision"), label="Precision", value=4),
          shiny::HTML("<p style=margin-bottom:2%;><strong>Save Table/Figure</strong></p>"),
          shiny::downloadButton('h_Report', label="Download")
        )
      )
    )
  )
}

#' @rdname mod_Homogeneity
#' @export
m_HomogeneityServer = function(id, homog, cert, datreturn) {
  stopifnot(shiny::is.reactive(homog))
  stopifnot(shiny::is.reactive(cert))

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)
    h_vals = shiny::reactiveVal(NULL)

    shiny::observeEvent(input$tab_link,{
      help_the_user("homogeneity_uncertainty")
    })

    shiny::observeEvent(homog(), {
      if(!is.null(homog())){
        shiny::updateTabsetPanel(session = session, "HomogeneityPanel", selected = "loaded")
      } else {
        # else if nothing is loaded, keep Panel empty
        shiny::updateTabsetPanel(session = session, "certificationPanel", selected = "standBy")
      }
    })

    h_Data <- shiny::reactive({
      shiny::req(homog()[["data"]])
      # this is the local version of the homology data
      # whatever range is loaded from excel can be checked and transformed in here
      #browser()
      h_dat <- homog()[["data"]]
      # rename if if first column is not named 'analyte' and convert to factor
      colnames(h_dat)[1] <- "analyte"
      h_dat[,"analyte"] <- factor(h_dat[,"analyte"])
      # ensure that there is a second column 'H_type' and convert to factor
      if (ncol(h_dat)==4) {
        h_dat <- cbind(h_dat[,1,drop=FALSE], data.frame("H_type"=gl(n = 1, k = nrow(h_dat), labels = "hom")), h_dat[,2:4])
      } else {
        colnames(h_dat)[2] <- "H_type"
        h_dat[,"H_type"] <- factor(h_dat[,"H_type"])
      }
      # ensure that there is a third column 'Flasche' and convert to factor
      colnames(h_dat)[3] <- "Flasche"
      h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
      return(h_dat)
    })

    newh_vals <- shiny::reactive({
      plyr::ldply(split(h_Data(), h_Data()[,"analyte"]), function(y) {
        plyr::ldply(split(y, y[,"H_type"]), function(x) {
          if (nrow(x)>=2) {
            anm <- stats::anova(stats::lm(value ~ Flasche, data=x))
            MSamong <- anm[1,"Mean Sq"]
            MSwithin <- anm[2,"Mean Sq"]
            mn <- mean(sapply(split(x[,"value"],x[,"Flasche"]),mean,na.rm=T),na.rm=T)
            n <- round(mean(table(as.character(x[,"Flasche"]))))
            N <- length(unique(x[,"Flasche"]))
            s_bb <- ifelse(MSamong>MSwithin, sqrt((MSamong-MSwithin)/n), 0)/mn
            s_bb_min <- (sqrt(MSwithin/n)*(2/(N*(n-1)))^(1/4))/mn
            return(data.frame("mean"=mn, "n"=n, "N"=N, "MSamong"=MSamong, "MSwithin"=MSwithin, "P"=anm$Pr[1], "s_bb"=s_bb, "s_bb_min"=s_bb_min))
          } else {
            return(data.frame("mean"=NA, "n"=0, "N"=0, "MSamong"=0, "MSwithin"=0, "P"=0, "s_bb"=0, "s_bb_min"=0))
          }
        }, .id="H_type")
      }, .id="analyte")
    })

    shiny::observe({h_vals(newh_vals())})

    output$h_fileUploaded <- shiny::reactive({
      return(!is.null(h_Data()))
    })

    output$h_sel_analyt <- shiny::renderUI({
      shiny::req(h_Data())
      lev <- levels(interaction(h_Data()[,"analyte"],h_Data()[,"H_type"]))
      shiny::selectInput(inputId=session$ns("h_sel_analyt"), label="analyte", choices=lev)
    })

    h_means <- shiny::reactive({
      shiny::req(h_Data(), input$h_sel_analyt)
      h_dat <- h_Data()
      h_dat <- h_dat[interaction(h_dat[,"analyte"],h_dat[,"H_type"])==input$h_sel_analyt,]
      h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
      out <- plyr::ldply(split(h_dat[,"value"], h_dat[,"Flasche"]), function(x) {
        data.frame("mean"=mean(x,na.rm=T), "sd"=stats::sd(x,na.rm=T), "n"=sum(is.finite(x)))
      }, .id="Flasche")
      rownames(out) <- out[,"Flasche"]
      return(out)
    })

    # Error checks
    h_errors <- shiny::reactive({
      shiny::req(input$h_precision)
      shiny::validate(shiny::need(is.numeric(input$h_precision) && input$h_precision>=0 && input$h_precision<=12, message="please check precision value"))
      return("")
    })
    output$h_error_message <- shiny::renderText(h_errors())

    # Tables
    output$h_overview_stats <- DT::renderDataTable({
      shiny::req(h_means(), input$h_precision)
      tab <- h_means()
      for (i in c("mean","sd")) { tab[,i] <- pn(tab[,i], input$h_precision) }
      return(tab)
    }, options = list(paging = TRUE, searching = FALSE), rownames=NULL, selection = "none")

    h_vals_print <- shiny::reactive({
      shiny::req(h_Data())
      c_Data <- cert
      h_vals_print <- h_vals()
      for (cn in c("mean","MSamong","MSwithin","P","s_bb","s_bb_min")) {
        h_vals_print[,cn] <- ecerto::pn(h_vals_print[,cn], input$h_precision)
      }
      if (!is.null(c_Data()$data)) {
        mater_table <- c_Data()$data
        h_vals_print[,"In_Cert_Module"] <- sapply(h_vals_print[,"analyte"], function(x) {
          ifelse(is.null(mater_table),"cert table not found", ifelse(x %in% mater_table[,"analyte"], "Yes", "No"))
        })
      }
      return(h_vals_print)
    })

    output$h_vals <- DT::renderDataTable({
      h_vals_print()
    }, options = list(paging = FALSE, searching = FALSE), rownames=NULL, selection = list(mode="single", target="row"))


    shiny::observeEvent(input$h_vals_rows_selected, {
      sel <- as.character(interaction(h_vals_print()[input$h_vals_rows_selected,1:2]))
      shiny::updateSelectInput(session = session, inputId = "h_sel_analyt", selected = sel)
    })

    # Plots & Print
    output$h_boxplot <- shiny::renderPlot({
      shiny::req(h_Data(), input$h_sel_analyt, input$h_precision, input$h_Fig_width)
      h_dat <- h_Data()
      h_dat <- h_dat[interaction(h_dat[,"analyte"], h_dat[,"H_type"])==input$h_sel_analyt,]
      h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
      #browser()
      #plot(h_dat)
      omn <- round(mean(h_dat[,"value"],na.rm=T), input$h_precision)
      osd <- round(stats::sd(h_dat[,"value"],na.rm=T), input$h_precision)
      anp <- pn(stats::anova(stats::lm(h_dat[,"value"] ~ h_dat[,"Flasche"]))$Pr[1], 2)
      graphics::par(mar=c(5,4,6,0)+0.1)
      graphics::plot(x=c(0.6,0.4+length(levels(h_dat[,"Flasche"]))), y=range(h_dat[,"value"],na.rm=T), type="n", xlab="Flasche", ylab=paste0(input$h_sel_analyt, " [", unique(h_dat["unit"]),"]"), axes=F)
      graphics::abline(h=omn, lty=2)
      graphics::abline(h=omn+c(-1,1)*osd, lty=2, col=grDevices::grey(0.8))
      graphics::boxplot(h_dat[,"value"] ~ h_dat[,"Flasche"], add=TRUE)
      graphics::mtext(text = paste("Overall mean =", omn), side = 3, line = 2.45, adj = 1)
      graphics::mtext(text = paste("Overall sd =", osd), side = 3, line = 1.3, adj = 1)
      graphics::mtext(text = paste("ANOVA P =", anp), side = 3, line = 2.45, adj = 0)
    }, height=500, width=shiny::reactive({input$h_Fig_width}))

    output$h_statement2 <- shiny::renderUI({
      shiny::req(h_Data(), input$h_sel_analyt)
      ansd <- max(h_vals()[interaction(h_vals()[,"analyte"],h_vals()[,"H_type"])==input$h_sel_analyt,c("s_bb","s_bb_min")])
      anp <- h_vals()[interaction(h_vals()[,"analyte"],h_vals()[,"H_type"])==input$h_sel_analyt,"P"]
      if (anp<0.05) {
        h2 <- "<font color=\"#FF0000\"><b>significantly different</b></font>"
        h4 <- "<b>Please check your method and data!</b>"
      } else {
        h2 <- "<font color=\"#00FF00\">not significantly different</font>"
        h4 <- ""
      }
      return(
        shiny::fluidRow(
          shiny::HTML("The tested items (Flasche) are ", h2, "(ANOVA P-value = ", pn(anp,2), ").<p>",
                      "The uncertainty value for analyte ", input$h_sel_analyt),
          shiny::actionLink(inputId = ns("hom_help_modal"), label = "was determined as"),
          shiny::HTML("<b>", pn(ansd), "</b>.<p>"), h4
        )
      )
    })

    shiny::observeEvent(input$hom_help_modal, {
      help_the_user("homogeneity_uncertainty")
    })

    output$h_anova <- shiny::renderPrint({
      shiny::req(h_Data(), input$h_sel_analyt)
      h_dat <- h_Data()
      h_dat <- h_dat[interaction(h_dat[,"analyte"],h_dat[,"H_type"])==input$h_sel_analyt,]
      stats::anova(stats::lm(h_dat[,"value"] ~ h_dat[,"Flasche"]))
    })

    # --- --- --- --- --- --- --- --- --- --- ---
    # t_H = m_TransferHomogeneityServer(
    #   id = "trH",
    #   homogData = shiny::reactive({getValue(datreturn,"h_vals")}),
    #   matTab_col_code = shiny::reactive({attr(getValue(datreturn,"mater_table"), "col_code")}),
    #   matTab_analytes = shiny::reactive({as.character(getValue(datreturn,"mater_table")[, "analyte"])})
    # )
    # # --- --- --- --- --- --- --- --- --- --- ---
    # # to Certification page after Transfer of Homogeneity Data
    # shiny::observeEvent(t_H(),{
    #   message("app_server: t_H() changed, set datreturn.t_H")
    #   setValue(datreturn,"t_H",t_H())
    # })

    h_transfer_U <- m_TransferUServer(
      id = "h_transfer",
      dat = shiny::reactive({h_vals()}),
      mat_tab = shiny::reactive({ecerto::getValue(datreturn, "mater_table")})
    )
    shiny::observeEvent(h_transfer_U$changed, {
      message("Homogeneity: observeEvent(h_transfer_U)")
      ecerto::setValue(datreturn, "mater_table", h_transfer_U$value)
    }, ignoreInit = TRUE)

    return(h_vals)

  })
}

