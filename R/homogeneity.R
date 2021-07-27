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
#' @param cert The Certification table - shiny::reactive({getValue(rv,"Certifications")}).
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_HomogeneityUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- reactiveClass$new(init_rv()) # initiate persistent variables
#'    m_HomogeneityServer(
#'      id = "test",
#'      homog = shiny::reactive({test_homog()}),
#'      cert = shiny::reactive({test_certification()})
#'    )
#'  }
#' )
#' }
#'
#' @return h_vals = The Homogeneity data (not the transferred ones yet)
#' @rdname mod_Homogeneity
#' @export

m_HomogeneityUI <- function(id) {
  shiny::tabsetPanel(
    id = shiny::NS(id, "HomogeneityPanel"),
    type = "hidden",
    # when nothing is loaded
    shiny::tabPanel(
      title = "standby-Panel",
      value  = "standby",
      "emtpy channel here, nix los"
      #helpText("Example Table"), imageOutput("myImage08a", inline = TRUE)
    ),
    # when something is loaded
    shiny::tabPanel(
      title = "active-Panel",
      value = "loaded",
      shiny::fluidRow(
        shiny::column(10, DT::dataTableOutput(shiny::NS(id,"h_vals"))),
        #  column(2,
        # #  conditionalPanel(
        # #   condition="output.c_fileUploaded_message != ''",
        # shiny::fluidRow(shiny::HTML("<p style=margin-bottom:-2%;><strong>Transfer s_bb of H_type</strong></p>"), align="right"),
        # shiny::fluidRow(shiny::uiOutput("h_transfer_H_type")),
        # shiny::fluidRow(shiny::HTML("<p style=margin-bottom:-2%;><strong>to Certification table column</strong></p>"), align="right"),
        # shiny::fluidRow(shiny::uiOutput("h_transfer_ubb")),
        # shiny::fluidRow(shiny::actionButton(inputId = "h_transfer_ubb_button", label = "Transfer Now!"), align="right")
        # )
      ),
      shiny::hr(),
      shiny::fluidRow(
        shiny::column(
          width = 3,
          DT::dataTableOutput(shiny::NS(id,"h_overview_stats"))
        ),
        shiny::column(
          width = 7,
          shiny::fluidRow(
            shiny::plotOutput(shiny::NS(id,"h_boxplot"), inline=TRUE),
            shiny::uiOutput(shiny::NS(id,"h_statement2"))
          )
        ),
        shiny::column(
          width = 2,
          shiny::uiOutput(shiny::NS(id,"h_sel_analyt")),
          shiny::numericInput(inputId=shiny::NS(id,"h_Fig_width"), label="Figure Width", value=650),
          shiny::numericInput(inputId=shiny::NS(id,"h_precision"), label="Precision", value=4),
          shiny::HTML("<p style=margin-bottom:2%;><strong>Save Table/Figure</strong></p>"),
          shiny::downloadButton('h_Report', label="Download")
        )
      )
    )
  )
}

#' @rdname mod_Homogeneity
#' @export
m_HomogeneityServer = function(id, homog, cert) {
  stopifnot(shiny::is.reactive(homog))
  stopifnot(shiny::is.reactive(cert))

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)
    h_vals = shiny::reactiveVal(NULL)

    shiny::observeEvent(homog(), {
      if(!is.null(homog())){
        shiny::updateTabsetPanel(session = session, "HomogeneityPanel", selected = "loaded")
      } else {
        # else if nothing is loaded, keep Panel empty
        shiny::updateTabsetPanel(session = session, "certificationPanel", selected = "standBy")
      }
    })
    #if loaded (successfully), male area visible
    # AGAIN: SUCCESSFULLY LOADED HERE!



    h_Data = shiny::reactive({
      h_dat = homog()[["data"]]
      h_dat[,"analyte"] <- factor(h_dat[,"analyte"])
      shiny::validate(shiny::need("Flasche" %in% colnames(h_dat), "No column 'Flasche' found in input file."))
      h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
      shiny::validate(shiny::need("H_type" %in% colnames(h_dat), "No column 'H_type' found in input file."))
      h_dat[,"H_type"] <- factor(h_dat[,"H_type"])
      return(h_dat)
    })

    newh_vals =  shiny::reactive({
      plyr::ldply(split(h_Data(), h_Data()[,"analyte"]), function(y) {
        plyr::ldply(split(y, y[,"H_type"]), function(x) {
          anm <- stats::anova(stats::lm(value ~ Flasche, data=x))
          MSamong <- anm[1,"Mean Sq"]
          MSwithin <- anm[2,"Mean Sq"]
          mn <- mean(sapply(split(x[,"value"],x[,"Flasche"]),mean,na.rm=T),na.rm=T)
          n <- round(mean(table(as.character(x[,"Flasche"]))))
          N <- length(unique(x[,"Flasche"]))
          s_bb <- ifelse(MSamong>MSwithin, sqrt((MSamong-MSwithin)/n), 0)/mn
          s_bb_min <- (sqrt(MSwithin/n)*(2/(N*(n-1)))^(1/4))/mn
          data.frame("mean"=mn, "n"=n, "N"=N, "MSamong"=MSamong, "MSwithin"=MSwithin, "P"=anm$Pr[1], "s_bb"=s_bb, "s_bb_min"=s_bb_min)
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
    }, options = list(paging = FALSE, searching = FALSE), rownames=NULL, selection = "none")

    h_vals_print = shiny::reactive({
      shiny::req(h_Data())
      c_Data = cert
      h_vals_print <- h_vals()

      for (i in c("mean","MSamong","MSwithin","P","s_bb","s_bb_min")) {
        h_vals_print[,i] <- ecerto::pn(h_vals_print[,i], input$h_precision)
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
      #browser()
      selected <- as.character(interaction(h_vals_print()[input$h_vals_rows_selected,1:2]))
      shiny::updateSelectInput(session = session, inputId = "h_sel_analyt", selected = selected)
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
      anp <- formatC(stats::anova(stats::lm(h_dat[,"value"] ~ h_dat[,"Flasche"]))$Pr[1], digits = 2, format = "e")
      graphics::par(mar=c(5,4,6,0)+0.1)
      graphics::plot(x=c(0.75,0.25+length(levels(h_dat[,"Flasche"]))), y=range(h_dat[,"value"],na.rm=T), type="n", xlab="Flasche", ylab=paste0(input$h_sel_analyt, " [", unique(h_dat["unit"]),"]"), axes=F)
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
        return(
          shiny::fluidRow(
            shiny::HTML("The tested items (Flasche) are ", "<font color=\"#FF0000\"><b>significantly different</b></font>", "(ANOVA P-value = ", pn(anp,2), ").<p>Please check your method and data.")
          )
        )
      } else {
        return(
          shiny::fluidRow(
            shiny::HTML("The tested items (Flasche) are ", "<font color=\"#00FF00\">not significantly different</font>", "(ANOVA P-value = ", pn(anp,2), ").<p>",
                        "The uncertainty value for analyte ", input$h_sel_analyt
            ),
            shiny::actionLink(inputId = ns("hom_help_modal"), label = "was determined as"),
            shiny::HTML("<b>", pn(ansd), "</b>.")
          )
        )
      }
    })

    shiny::observeEvent(input$hom_help_modal, {
      shiny::req(h_vals_print(), input$h_sel_analyt)
      #browser()
      h <- h_vals_print()
      i <- which(interaction(h_vals()[,"analyte"],h_vals()[,"H_type"])==input$h_sel_analyt)

      shiny::showModal(
        shiny::modalDialog(
        shiny::withMathJax(
        shiny::includeHTML(
          rmarkdown::render(
            system.file("help","uncertainty.Rmd",package = "ecerto")
          )
        )),
          # shiny::HTML(
          #   "<p>Using ANOVA on a one factor linear model we determined from N =", h[i,"N"], " containers with n =", h[i,"n"], "replicates each:",
          #   "<p>Variance within containers (MS_within, s_w) =", h[i,"MSwithin"],
          #   "<br>Variance between containers (MS_among, s_a) =", h[i,"MSamong"],
          #   "<p>Using formula: sqrt(s_a-s_w)/mean the relative uncertainty (s_bb) =", h[i,"s_bb"],
          #   "<br>and s_bb_min using: sqrt(s_w/n)*(2/(N*(n-1)))^(1/4))/mean =", h[i,"s_bb_min"],
          #   "<p>The larger of both values, s_bb and s_bb_min, is transfered as uncertainty contribution",
          #   "<p>Note: s_bb = 0 for s_a < s_w"
          # ),
          footer = shiny::tagList(
            shiny::modalButton("Ok")
          ),
          size = "m",
          title = "Uncertainty calculation"
        )
      )
    })

    output$h_anova <- shiny::renderPrint({
      shiny::req(h_Data(), input$h_sel_analyt)
      h_dat <- h_Data()
      h_dat <- h_dat[interaction(h_dat[,"analyte"],h_dat[,"H_type"])==input$h_sel_analyt,]
      stats::anova(stats::lm(h_dat[,"value"] ~ h_dat[,"Flasche"]))
    })


    # Special UI
    # TODO
    # output$h_transfer_ubb <- shiny::renderUI({
    #   shiny::validate(shiny::need(input$sel_analyt, message = "please upload certification data first"))
    #   shiny::req(cert())
    #   shiny::selectInput(
    #     inputId=session$ns("h_transfer_ubb"),
    #     label="",
    #     selectize=TRUE,
    #     choices=attr(cert(), "col_code")[substr(attr(cert(), "col_code")[,"ID"],1,1)=="U","Name"]
    #   )
    # })

    output$h_transfer_H_type <- shiny::renderUI({
      shiny::req(h_Data())
      shiny::selectInput(inputId=session$ns("h_transfer_H_type"), label="", selectize=TRUE, choices=levels(h_vals()[,"H_type"]))
    })



    # })

    return(h_vals)
  })
}

