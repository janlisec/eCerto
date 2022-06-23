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
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    page_HomogeneityUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- eCerto:::test_rv()
#'    mt <- isolate(eCerto::getValue(rv, c("General","materialtabelle")))
#'    attr(mt, "col_code") <- data.frame("ID"="U","Name"="U")
#'    isolate(eCerto::setValue(rv, c("General","materialtabelle"), mt))
#'    isolate(eCerto::setValue(rv, "Homogeneity", eCerto:::test_homog()))
#'    page_HomogeneityServer(
#'      id = "test",
#'      rv = rv
#'    )
#'  }
#' )
#' }
#'
#' @rdname page_Homogeneity
#' @export

page_HomogeneityUI <- function(id) {
  shinyjs::useShinyjs()
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
              inputId = ns("tab1_link"),
              label = "Tab.1 Homogeneity - calculation of uncertainty contribution"
            )
          ),
          DT::dataTableOutput(ns("h_tab1"))
        ),
        shiny::column(2, shiny::wellPanel(m_TransferUUI(ns("h_transfer"))))
      ),
      shiny::p(),
      shiny::fluidRow(
        shiny::column(
          width = 3,
          shiny::strong(
            shiny::actionLink(
              inputId = ns("tab2_link"),
              label = "Tab.2 Homogeneity - specimen stats"
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
                label = "Fig.1 Homogeneity - boxplot"
              )
            ), shiny::p(),
            shiny::plotOutput(ns("h_boxplot"), inline=TRUE),
            shiny::uiOutput(ns("h_statement2"))
          )
        ),
        shiny::column(
          width = 2,
          shiny::wellPanel(
            shinyjs::hidden(shiny::selectInput(inputId=ns("h_sel_analyt"), label="Row selected in Tab.1", choices="")),
            shiny::HTML("<p style=margin-bottom:2%;><strong>Save Table/Figure</strong></p>"),
            shiny::downloadButton(ns("h_Report"), label="Download")
          )
        )
      )
    )
  )
}

#' @rdname page_Homogeneity
#' @export
page_HomogeneityServer = function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    homog <- shiny::reactive({getValue(rv,"Homogeneity")})

    h_vals <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$tab1_link,{
      help_the_user_modal("homogeneity_uncertainty")
    })

    shiny::observeEvent(input$tab2_link,{
      help_the_user_modal("homogeneity_specimen_stats")
    })

    shiny::observeEvent(input$fig1_link,{
      help_the_user_modal("homogeneity_boxplot")
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
      #
      h_dat <- homog()[["data"]]
      # rename if if first column is not named 'analyte' and convert to factor
      colnames(h_dat)[1] <- "analyte"
      h_dat[,"analyte"] <- factor(h_dat[,"analyte"])
      # ensure that there is a second column 'H_type' and convert to factor
      if (colnames(h_dat)[2]!="H_type" && colnames(h_dat)[3]=="value") {
        h_dat <- cbind(h_dat[,1,drop=FALSE], data.frame("H_type"=gl(n = 1, k = nrow(h_dat), labels = "hom")), h_dat[,2:4])
      } else {
        h_dat[,"H_type"] <- factor(h_dat[,"H_type"])
      }
      # ensure that there is a third column 'Flasche' and convert to factor
      colnames(h_dat)[3] <- "Flasche"
      h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
      # update analyte select input
      lev <- levels(interaction(h_dat[,"analyte"],h_dat[,"H_type"]))
      shiny::updateSelectInput(inputId="h_sel_analyt", label="Row selected in Tab.1", choices = lev, selected=lev[1])
      shinyjs::disable("h_sel_analyt")
      # return h_dat
      return(h_dat)
    })

    newh_vals <- shiny::reactive({
      plyr::ldply(split(h_Data(), h_Data()[,"analyte"]), function(y) {
        plyr::ldply(split(y, y[,"H_type"]), function(x) {
          if (nrow(x)>=2) {
            anm <- stats::anova(stats::lm(value ~ Flasche, data=x))
            M_between <- anm[1,"Mean Sq"]
            M_within <- anm[2,"Mean Sq"]
            mn <- mean(sapply(split(x[,"value"],x[,"Flasche"]),mean,na.rm=T),na.rm=T)
            n_i <- table(as.character(x[,"Flasche"]))
            N <- length(n_i)
            #n <- round(mean(table(as.character(x[,"Flasche"]))))
            #[modified to ISO35[B.4] on suggestion of KV]
            n <- 1/(N-1)*(sum(n_i)-sum(n_i^2)/sum(n_i))
            s_bb <- ifelse(M_between>M_within, sqrt((M_between-M_within)/n)/mn, 0)
            s_bb_min <- (sqrt(M_within/n)*(2/(N*(n-1)))^(1/4))/mn
            return(data.frame("mean"=mn, "n"=n, "N"=N, "M_between"=M_between, "M_within"=M_within, "P"=anm$Pr[1], "s_bb"=s_bb, "s_bb_min"=s_bb_min))
          } else {
            return(data.frame("mean"=NA, "n"=0, "N"=0, "M_between"=0, "M_within"=0, "P"=0, "s_bb"=0, "s_bb_min"=0))
          }
        }, .id="H_type")
      }, .id="analyte")
    })

    shiny::observe({h_vals(newh_vals())})
    shiny::observeEvent(newh_vals(), {
      setValue(rv, c("Homogeneity","h_vals"), newh_vals())
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

    precision <- shiny::reactive({
      shiny::req(input$h_sel_analyt)
      #message("[H] setting precision")
      prec <- 4
      an <- as.character(h_vals()[interaction(h_vals()[,"analyte"], h_vals()[,"H_type"])==input$h_sel_analyt,"analyte"])
      apm <- getValue(rv, c("General", "apm"))
      if (an %in% names(apm)) {
        #prec <- apm[[an]][["precision_export"]]
        prec <- apm[[an]][["precision"]]
      }
      return(prec)
    })

    # Tables
    output$h_tab2 <- DT::renderDataTable({
      shiny::req(h_means(), precision())
      tab <- h_means()
      #message("[H] rendering tab2")
      cols <- which(colnames(tab) %in% c("mean","sd"))
      for (i in cols) tab[,i] <- pn(tab[,i], precision())
      tab <- DT::datatable(
        data = tab,
        options = list(
          dom = "t",
          pageLength = NULL,
          columnDefs = list(
            list(className = 'dt-right', targets='_all')
          )
        ),
        rownames=NULL, selection = "none"
      )
      #DT::formatCurrency(table = tab, columns = cols, currency = "", digits = precision())
      #DT::formatSignif(table = tab, columns = cols, digits = precision())
      return(tab)
    })

    h_vals_print <- shiny::reactive({
      shiny::req(h_Data())
      mt <- getValue(rv, c("General", "materialtabelle"))
      h_vals_print <- h_vals()
      apm <- getValue(rv, c("General", "apm"))
      for (i in 1:nrow(h_vals_print)) {
        an <- as.character(h_vals_print[i,"analyte"])
        h_vals_print[i,"mean"] <- pn(as.numeric(h_vals_print[i,"mean"]), ifelse(an %in% names(apm), apm[[an]][["precision"]], 4))
      }
      # round the following columns with fixed precision of 4 digits
      for (cn in c("M_between","M_within","P","s_bb","s_bb_min")) {
        h_vals_print[,cn] <- pn(h_vals_print[,cn], 4)
      }
      # check if analyte is present in C modul
      if (!is.null(mt)) {
        h_vals_print[,"style_analyte"] <- sapply(h_vals_print[,"analyte"], function(x) {
          ifelse(x %in% mt[,"analyte"], "black", "red")
        })
      }
      h_vals_print[,"style_s_bb"] <- c("bold","normal")[1+as.numeric(h_vals_print[,"s_bb"]<h_vals_print[,"s_bb_min"])]
      h_vals_print[,"style_s_bb_min"] <- c("bold","normal")[1+as.numeric(h_vals_print[,"s_bb"]>=h_vals_print[,"s_bb_min"])]
      return(h_vals_print)
    })

    output$h_tab1 <- DT::renderDataTable({
      #message("[H] rendering tab1")
      dt <- h_vals_print()
      inv_cols <- grep("style_", colnames(dt))-1
      if (length(unique(dt[,"H_type"]))==1) inv_cols <- c(1, inv_cols)
      dt <- DT::datatable(
        data = h_vals_print(),
        options = list(
          dom = "t",
          pageLength = NULL,
          columnDefs = list(
            list(visible = FALSE, targets = inv_cols),
            list(width = '30px', targets = c(3,4)),
            list(width = '60px', targets = c(5:9)),
            list(className = 'dt-right', targets='_all')
          )
        ),
        rownames=NULL, selection = list(mode="single", target="row")
      )
      dt <- DT::formatStyle(
        table = dt,
        columns = "analyte",
        valueColumns = "style_analyte",
        target = "cell",
        color = DT::styleValue()
      )
      dt <- DT::formatStyle(
        table = dt,
        columns = "s_bb",
        valueColumns = "style_s_bb",
        target = "cell",
        fontWeight = DT::styleValue()
      )
      dt <- DT::formatStyle(
        table = dt,
        columns = "s_bb_min",
        valueColumns = "style_s_bb_min",
        target = "cell",
        fontWeight = DT::styleValue()
      )
      dt <- DT::formatStyle(
        table = dt,
        columns = "P",
        target = "cell",
        color = DT::styleInterval(cuts = 0.05, values = c("red","black")),
        fontWeight = DT::styleInterval(cuts = 0.05, values = c("bold","normal"))
      )
      return(dt)
    })


    shiny::observeEvent(input$h_tab1_rows_selected, {
      sel <- as.character(interaction(h_vals_print()[input$h_tab1_rows_selected,1:2]))
      shiny::updateSelectInput(session = session, inputId = "h_sel_analyt", selected = sel)
      #shinyjs::disable(id = "h_sel_analyt")
    })

    # Plots & Print
    fig_width <- shiny::reactive({
      shiny::req(h_Data(), input$h_sel_analyt)
      x <- h_Data()
      a <- input$h_sel_analyt
      n <- length(levels(factor(x[interaction(x[,"analyte"], x[,"H_type"])==a,"Flasche"])))
      return(150 + 40 * n)
    })
    output$h_boxplot <- shiny::renderPlot({
      shiny::req(h_Data(), input$h_sel_analyt, precision())
      h_dat <- h_Data()
      an <- ifelse(length(unique(h_dat[,"H_type"]))==1, as.character(h_dat[interaction(h_dat[,"analyte"], h_dat[,"H_type"])==input$h_sel_analyt,"analyte"]), input$h_sel_analyt)
      h_dat <- h_dat[interaction(h_dat[,"analyte"], h_dat[,"H_type"])==input$h_sel_analyt,]
      h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
      omn <- round(mean(h_dat[,"value"],na.rm=T), precision())
      osd <- round(stats::sd(h_dat[,"value"],na.rm=T), precision())
      graphics::par(mar=c(5,4,2.5,0)+0.1)
      graphics::plot(x=c(0.6,0.4+length(levels(h_dat[,"Flasche"]))), y=range(h_dat[,"value"],na.rm=T), type="n", xlab="Flasche", ylab=paste0(an, " [", unique(h_dat["unit"]),"]"), axes=F)
      graphics::abline(h=omn, lty=2)
      graphics::abline(h=omn+c(-1,1)*osd, lty=2, col=grDevices::grey(0.8))
      graphics::boxplot(h_dat[,"value"] ~ h_dat[,"Flasche"], add=TRUE)
      graphics::mtext(text = paste("Overall mean =", omn), side = 3, line = 1.5, adj = 1)
      graphics::mtext(text = paste("Overall sd =", osd), side = 3, line = 0.25, adj = 1)
    }, height=500, width=shiny::reactive({fig_width()}))

    output$h_statement2 <- shiny::renderUI({
      shiny::req(h_vals(), input$h_sel_analyt)
      h_dat <- h_vals()
      an <- ifelse(length(unique(h_dat[,"H_type"]))==1, as.character(h_dat[interaction(h_dat[,"analyte"], h_dat[,"H_type"])==input$h_sel_analyt,"analyte"]), input$h_sel_analyt)
      ansd <- max(h_dat[interaction(h_dat[,"analyte"],h_dat[,"H_type"])==input$h_sel_analyt,c("s_bb","s_bb_min")])
      anp <- h_dat[interaction(h_dat[,"analyte"],h_dat[,"H_type"])==input$h_sel_analyt,"P"]
      if (anp<0.05) {
        h2 <- "<font color=\"#FF0000\"><b>significantly different</b></font>"
        h4 <- "<b>Please check your method and data!</b>"
      } else {
        h2 <- "<font color=\"#00FF00\">not significantly different</font>"
        h4 <- ""
      }
      return(
        shiny::fluidRow(shiny::column(12,
          shiny::HTML("The tested items (Flasche) are ", h2, "(ANOVA P-value = ", pn(anp,2), ").<p>",
                      "The uncertainty value for analyte ", an),
          shiny::actionLink(inputId = ns("hom_help_modal"), label = "was determined as"),
          shiny::HTML("<b>", pn(ansd), "</b>.<p>", h4)
        ))
      )
    })

    shiny::observeEvent(input$hom_help_modal, {
      help_the_user_modal("homogeneity_uncertainty")
    })

    h_transfer_U <- m_TransferUServer(
      id = "h_transfer",
      dat = shiny::reactive({h_vals()}),
      mat_tab = shiny::reactive({getValue(rv, c("General","materialtabelle"))})
    )
    shiny::observeEvent(h_transfer_U$changed, {
      message("Homogeneity: observeEvent(h_transfer_U)")
      setValue(rv, c("General","materialtabelle"), h_transfer_U$value)
    }, ignoreInit = TRUE)

    output$h_Report <- shiny::downloadHandler(
      filename = function() { "Homogeneity_report.pdf" },
      content = function(file) {
        rmdfile <- get_local_file("report_vorlage_homogeneity.Rmd")
        # render the markdown file
        shiny::withProgress(
          expr = {
            incProgress(0.5)
            out <- rmarkdown::render(
              input = rmdfile,
              output_file = file,
              output_format = rmarkdown::pdf_document(),
              params = list("Homogeneity" = shiny::reactiveValuesToList(getValue(rv,"Homogeneity"))),
              envir = new.env(parent = globalenv())
            )
          },
          message = "Rendering Homogeneity Report.."
        )
        return(out)
      }
    )

  })
}

