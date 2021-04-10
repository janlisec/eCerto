# HOMOGENEITY MODULE -------------------------

.HomogeneityUI = function(id) {
  tabsetPanel(
    id = NS(id, "HomogeneityPanel"),
    type = "hidden",
    # when nothing is loaded
    tabPanel(
      title = "standby-Panel", 
      value  = "standby", 
      "emtpy channel here, nix los"
      #helpText("Example Table"), imageOutput("myImage08a", inline = TRUE)
    ),
    # when something is loaded
    tabPanel(
      title = "active-Panel",
      value = "loaded",
      fluidRow(
        column(10, DT::dataTableOutput(NS(id,"h_vals"))),
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
      fluidRow(
        column(3, DT::dataTableOutput(NS(id,"h_overview_stats"))),
        column(9,
               fluidRow(
                 column(2, uiOutput(NS(id,"h_sel_analyt"))),
                 column(2, numericInput(inputId=NS(id,"h_Fig_width"), label="Figure Width", value=850)),
                 column(2, numericInput(inputId=NS(id,"h_precision"), label="Precision", value=4)),
                 column(6,
                        fluidRow(HTML("<p style=margin-bottom:2%;><strong>Save Table/Figure</strong></p>")),
                        fluidRow(downloadButton('h_Report', label="Download")), align = "right"
                 )
               ),
               fluidRow(
                 column(12, plotOutput(NS(id,"h_boxplot"), inline=TRUE), offset = 0.1)
               ),
               fluidRow(
                 column(12, textOutput(NS(id,"h_statement")), offset = 0.1),
                 tags$style(type="text/css", "#h_statement {margin-top:2%;}")
               ),
               fluidRow(
                 column(12, verbatimTextOutput(NS(id,"h_anova")), offset = 0.1),
                 tags$style(type="text/css", "#h_anova {margin-top:2%;}")
               )
        )
      )
    )
  )
}

.HomogeneityServer = function(id, rv, datreturn) {
  moduleServer(id, function(input, output, session) {
    
    d = reactive({rv$Homogeneity})
    
    observeEvent(d(), {
      #if loaded (successfully), male area visible
      # AGAIN: SUCCESSFULLY LOADED HERE!
      if(!is.null(d())){
        # print(ecerto::data_of_godelement(d()))
        updateTabsetPanel(session = session,"HomogeneityPanel", selected = "loaded")
        h_Data = reactive({
          h_dat = ecerto::data_of_godelement(d())
          h_dat[,"analyte"] <- factor(h_dat[,"analyte"])
          validate(need("Flasche" %in% colnames(h_dat), "No column 'Flasche' found in input file."))
          h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
          validate(need("H_type" %in% colnames(h_dat), "No column 'H_type' found in input file."))
          h_dat[,"H_type"] <- factor(h_dat[,"H_type"])
          return(h_dat)
        })
       
        
        h_vals =  reactive({
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
        
        observeEvent(h_vals(),{
          print(".HomogeneityServer - h_vals added")
          datreturn$h_vals = h_vals()
        })
        
        output$h_fileUploaded <- reactive({
          return(!is.null(h_Data()))
        })
        
        output$h_sel_analyt <- renderUI({
          req(h_Data())
          lev <- levels(interaction(h_Data()[,"analyte"],h_Data()[,"H_type"]))
          selectInput(inputId=session$ns("h_sel_analyt"), label="analyte", choices=lev)
        })
        
        h_means <- reactive({
          req(h_Data(), input$h_sel_analyt)
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
        h_errors <- reactive({
          req(input$h_precision)
          validate(need(is.numeric(input$h_precision) && input$h_precision>=0 && input$h_precision<=12, message="please check precision value"))
          return("")
        })
        output$h_error_message <- renderText(h_errors())
        
        # Tables
        output$h_overview_stats <- DT::renderDataTable({
          req(h_means(), input$h_precision)
          tab <- h_means()
          for (i in c("mean","sd")) { tab[,i] <- pn(tab[,i], input$h_precision) }
          return(tab)
        }, options = list(paging = FALSE, searching = FALSE), rownames=NULL)
        
        output$h_vals <- DT::renderDataTable({
          req(h_Data())
          c_Data = reactive({ecerto::data_of_godelement(rv$Certifications)})
          h_vals_print <- h_vals()
          for (i in c("mean","MSamong","MSwithin","P","s_bb","s_bb_min")) {
            h_vals_print[,i] <- pn(h_vals_print[,i], input$h_precision)
          }
          if (!is.null(c_Data())) {
            mater_table <- c_Data()
            h_vals_print[,"In_Cert_Module"] <- sapply(h_vals_print[,"analyte"], function(x) {
              ifelse(is.null(mater_table),"cert table not found", ifelse(x %in% mater_table[,"analyte"], "Yes", "No"))
            })
          }
          return(h_vals_print)
        }, options = list(paging = FALSE, searching = FALSE), rownames=NULL)
        
        
        # Plots & Print
        output$h_boxplot <- shiny::renderPlot({
          req(h_Data(), input$h_sel_analyt, input$h_precision, input$h_Fig_width)
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
          req(h_Data(), input$h_sel_analyt)
          ansd <- max(h_vals()[interaction(h_vals()[,"analyte"],h_vals()[,"H_type"])==input$h_sel_analyt,c("s_bb","s_bb_min")])
          anp <- h_vals()[interaction(h_vals()[,"analyte"],h_vals()[,"H_type"])==input$h_sel_analyt,"P"]
          if (anp<0.05) {
            return(paste0("The tested items (Flasche) are significantly different (ANOVA P-value = ", pn(anp,2),"). Please check your method and model."))
          } else {
            return(paste0("The tested items (Flasche) are not significantly different (ANOVA P-value = ", pn(anp,2), "). The uncertainty value for analyte ", input$h_sel_analyt, " was determined as ", pn(ansd), "."))
          }
        })
        
        output$h_anova <- shiny::renderPrint({
          req(h_Data(), input$h_sel_analyt)
          h_dat <- h_Data()
          h_dat <- h_dat[interaction(h_dat[,"analyte"],h_dat[,"H_type"])==input$h_sel_analyt,]
          anova(lm(h_dat[,"value"] ~ h_dat[,"Flasche"]))
        })
        
        
        # Special UI
        # TODO
        output$h_transfer_ubb <- renderUI({
          validate(need(input$sel_analyt, message = "please upload certification data first"))
          req(getData("cert_vals"))
          selectInput(
            inputId=session$ns("h_transfer_ubb"), 
            label="", 
            selectize=TRUE, 
            choices=attr(getData("cert_vals"), "col_code")[substr(attr(getData("cert_vals"), "col_code")[,"ID"],1,1)=="U","Name"]
          )
        })
        
        output$h_transfer_H_type <- renderUI({
          req(h_Data())
          selectInput(inputId=session$ns("h_transfer_H_type"), label="", selectize=TRUE, choices=levels(h_vals()[,"H_type"]))
        })
        
        
      } else { 
        # else if nothing is loaded, keep Panel empty
        updateTabsetPanel(session = session,"certificationPanel", selected = "standBy")
      }
    }, ignoreInit = TRUE)
  })
}

# TRANSFER HOMOGENEITY MODULE -------------------------
.TransferHomogeneityUI = function(id) {
  shinyjs::disabled(
    fluidRow(id = NS(id,"transferPanel"),
      #fluidRow(HTML("<p style=margin-bottom:-2%;><strong>Transfer s_bb of H_type</strong></p>"), align="right"),
      column(4,
             selectInput(
               inputId=NS(id,"h_transfer_H_type"), 
               label="", 
               selectize=TRUE, 
               choices=NULL
             )
      ),
      #fluidRow(HTML("<p style=margin-bottom:-2%;><strong>to Certification table column</strong></p>"), align="right"),
      column(4,
             selectInput(inputId=NS(id,"h_transfer_ubb"), 
                         label="", 
                         selectize=TRUE,
                         choices=NULL
             )           
      ),
      column(4, actionButton(inputId = NS(id,"h_transfer_ubb_button"), label = "Transfer Now!"))
    )
  )
}

# This module is called in lowest server module. It transfers values from the
# Homogeneity module into the materialtabelle
.TransferHomogeneityServer = function(id, datreturn) {
  moduleServer(id, function(input, output, session) {
    
    # activate transfer panel only, when (1) materialtabelle was created after 
    # certification upload and (2) homogeneity data was uploaded
    observeEvent({
      datreturn$mater_table
      datreturn$h_vals
      }
      ,{
      if(!is.null(datreturn$h_vals) && !is.null(datreturn$mater_table)){
        shinyjs::enable(id = "transferPanel")
        message("Transfer Homogeneity Panel activated")
        
        updateSelectInput(
          session = session,
          inputId = "h_transfer_H_type",
          choices = levels(datreturn$h_vals[,"H_type"]))
        
        updateSelectInput(
          session = session,
          inputId = "h_transfer_ubb",
          choices=attr(datreturn$mater_table, "col_code")[substr(attr(datreturn$mater_table, "col_code")[,"ID"],1,1)=="U","Name"]
        )
      }

    }) 
    
    # TODO
    observeEvent(input$h_transfer_ubb_button, {
      req(
        # h_Data(), 
        datreturn$h_vals, 
        datreturn$mater_table, 
        input$h_transfer_ubb, 
        input$h_transfer_H_type
      )
      message("TRANSFER BUTTON clicked")
      h_vals <- datreturn$h_vals
      cert_vals <- datreturn$mater_table
      for (i in 1:nrow(cert_vals)) {
        # select cell of same analyte and Homogeneity type
        j <-
          which(
            as.character(h_vals[, "analyte"]) ==  as.character(cert_vals[i, "analyte"]) 
            &
            as.character(h_vals[, "H_type"]) == isolate(input$h_transfer_H_type)
          )
        # if cell exists
        if (length(j) == 1) {
          c_name = which(attr(cert_vals, "col_code")[, "Name"] ==
                           isolate(input$h_transfer_ubb))
          cert_vals[i, attr(cert_vals, "col_code")[c_name, "ID"]] <-
            max(h_vals[j, c("s_bb", "s_bb_min")])
        }
      }
      print(cert_vals)
      datreturn$mater_table <- cert_vals 
      # assign("cert_vals", value=cert_vals, envir = env_perm)
      # shinyjs::click(id="show_table")
    })
  })
}