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
#' @param homog = the Homogeneity table shiny::reactive({getValue(rv,"Homogeneity")})
#' @param cert = shiny::reactive({getValue(rv,"Certifications")})
#'
#' @return h_vals = The Homogeneity data (not the transferred ones yet)
#' @rdname mod_Homogeneity
#' @export
m_HomogeneityUI = function(id) {
  shiny::tabsetPanel(
    id = NS(id, "HomogeneityPanel"),
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
        shiny::column(10, DT::dataTableOutput(NS(id,"h_vals"))),
        #  column(2,
        # #  conditionalPanel(
        # #   condition="output.c_fileUploaded_message != ''",
        #    fluidRow(HTML("<p style=margin-bottom:-2%;><strong>Transfer s_bb of H_type</strong></p>"), align="right"),
        #    fluidRow(uiOutput("h_transfer_H_type")),
        #    fluidRow(HTML("<p style=margin-bottom:-2%;><strong>to Certification table column</strong></p>"), align="right"),
        #    fluidRow(uiOutput("h_transfer_ubb")),
        #    fluidRow(actionButton(inputId = "h_transfer_ubb_button", label = "Transfer Now!"), align="right")
        # )
      ),
      hr(),
      shiny::fluidRow(
        shiny::column(3, DT::dataTableOutput(shiny::NS(id,"h_overview_stats"))),
        shiny::column(9,
                      shiny::fluidRow(
                        shiny::column(2, shiny::uiOutput(shiny::NS(id,"h_sel_analyt"))),
                        shiny::column(2, shiny::numericInput(inputId=shiny::NS(id,"h_Fig_width"), label="Figure Width", value=850)),
                        shiny::column(2,shiny::numericInput(inputId=shiny::NS(id,"h_precision"), label="Precision", value=4)),
                        shiny::column(6,
                                      shiny::fluidRow(shiny::HTML("<p style=margin-bottom:2%;><strong>Save Table/Figure</strong></p>")),
                                      shiny::fluidRow(shiny::downloadButton('h_Report', label="Download")), align = "right"
                        )
                      ),
                      shiny::fluidRow(
                        shiny::column(12, shiny::plotOutput(NS(id,"h_boxplot"), inline=TRUE), offset = 0.1)
                      ),
                      shiny::fluidRow(
                        shiny::column(12, shiny::textOutput(NS(id,"h_statement")), offset = 0.1),
                        tags$style(type="text/css", "#h_statement {margin-top:2%;}")
                      ),
                      shiny::fluidRow(
                        shiny::column(12, shiny::verbatimTextOutput(NS(id,"h_anova")), offset = 0.1),
                        tags$style(type="text/css", "#h_anova {margin-top:2%;}")
                      )
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

    h_vals = reactiveVal(NULL)

    shiny::observeEvent(homog(), {
      if(!is.null(homog())){
        shiny::updateTabsetPanel(session = session,"HomogeneityPanel", selected = "loaded")
      } else {
        # else if nothing is loaded, keep Panel empty
        shiny::updateTabsetPanel(session = session,"certificationPanel", selected = "standBy")
      }
    })
    #if loaded (successfully), male area visible
    # AGAIN: SUCCESSFULLY LOADED HERE!



    h_Data = shiny::reactive({
      h_dat = ecerto::data_of_godelement(homog())
      h_dat[,"analyte"] <- factor(h_dat[,"analyte"])
      validate(need("Flasche" %in% colnames(h_dat), "No column 'Flasche' found in input file."))
      h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
      validate(need("H_type" %in% colnames(h_dat), "No column 'H_type' found in input file."))
      h_dat[,"H_type"] <- factor(h_dat[,"H_type"])
      return(h_dat)
    })

    newh_vals =  shiny::reactive({
      plyr::ldply(split(h_Data(), h_Data()[,"analyte"]), function(y) {
        plyr::ldply(split(y, y[,"H_type"]), function(x) {
          anm <- anova(lm(value ~ Flasche, data=x))
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

    observe({h_vals(newh_vals())})

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
        data.frame("mean"=mean(x,na.rm=T), "sd"=sd(x,na.rm=T), "n"=sum(is.finite(x)))
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
    }, options = list(paging = FALSE, searching = FALSE), rownames=NULL)

    h_vals_print = reactive({
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
    }, options = list(paging = FALSE, searching = FALSE), rownames=NULL)


    # Plots & Print
    output$h_boxplot <- shiny::renderPlot({
      shiny::req(h_Data(), input$h_sel_analyt, input$h_precision, input$h_Fig_width)
      h_dat <- h_Data()
      h_dat <- h_dat[interaction(h_dat[,"analyte"],h_dat[,"H_type"])==input$h_sel_analyt,]
      h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
      plot(h_dat)
      omn <- round(mean(h_dat[,"value"],na.rm=T),input$h_precision)
      osd <- round(sd(h_dat[,"value"],na.rm=T),input$h_precision)
      anp <- formatC(anova(lm(h_dat[,"value"] ~ h_dat[,"Flasche"]))$Pr[1],digits = 2, format = "e")
      par(mar=c(5,4,6,0)+0.1)
      plot(x=c(1,length(levels(h_dat[,"Flasche"]))), y=range(h_dat[,"value"],na.rm=T), type="n", xlab="Flasche", ylab=paste0(input$h_sel_analyt, " [", unique(h_dat["unit"]),"]"), axes=F)
      abline(h=omn, lty=2)
      abline(h=omn+c(-1,1)*osd, lty=2, col=grey(0.8))
      boxplot(h_dat[,"value"] ~ h_dat[,"Flasche"], add=TRUE)
      mtext(text = paste("Overall mean =", omn), side = 3, line = 2.45, adj = 1)
      mtext(text = paste("Overall sd =", osd), side = 3, line = 1.3, adj = 1)
      mtext(text = paste("ANOVA P =", anp), side = 3, line = 2.45, adj = 0)
    }, height=500, width=reactive({input$h_Fig_width}))

    output$h_statement <- shiny::renderText({
      shiny::req(h_Data(), input$h_sel_analyt)
      ansd <- max(h_vals()[interaction(h_vals()[,"analyte"],h_vals()[,"H_type"])==input$h_sel_analyt,c("s_bb","s_bb_min")])
      anp <- h_vals()[interaction(h_vals()[,"analyte"],h_vals()[,"H_type"])==input$h_sel_analyt,"P"]
      if (anp<0.05) {
        return(paste0("The tested items (Flasche) are significantly different (ANOVA P-value = ", pn(anp,2),"). Please check your method and model."))
      } else {
        return(paste0("The tested items (Flasche) are not significantly different (ANOVA P-value = ", pn(anp,2), "). The uncertainty value for analyte ", input$h_sel_analyt, " was determined as ", pn(ansd), "."))
      }
    })

    output$h_anova <- shiny::renderPrint({
      shiny::req(h_Data(), input$h_sel_analyt)
      h_dat <- h_Data()
      h_dat <- h_dat[interaction(h_dat[,"analyte"],h_dat[,"H_type"])==input$h_sel_analyt,]
      anova(lm(h_dat[,"value"] ~ h_dat[,"Flasche"]))
    })


    # Special UI
    # TODO
    output$h_transfer_ubb <- shiny::renderUI({
      shiny::validate(shiny::need(input$sel_analyt, message = "please upload certification data first"))
      shiny::req(getData("cert_vals"))
      shiny::selectInput(
        inputId=session$ns("h_transfer_ubb"),
        label="",
        selectize=TRUE,
        choices=attr(getData("cert_vals"), "col_code")[substr(attr(getData("cert_vals"), "col_code")[,"ID"],1,1)=="U","Name"]
      )
    })

    output$h_transfer_H_type <- shiny::renderUI({
      shiny::req(h_Data())
      shiny::selectInput(inputId=session$ns("h_transfer_H_type"), label="", selectize=TRUE, choices=levels(h_vals()[,"H_type"]))
    })



    # })

    return(h_vals)
  })
}

