#' @title STABILITY MODULE
#' @name mod_Stability
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_StabilityUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- reactiveClass$new(init_rv()) # initiate persistent variables
#'    isolate({ecerto::setValue(rv, c("Stability","data"), ecerto:::test_Stability_Excel() )})
#'    isolate({ecerto::setValue(rv, c("Stability","uploadsource"), "Excel")})
#'    datreturn = ecerto:::test_datreturn()
#'    m_StabilityServer(
#'      id = "test",
#'      rv = rv,
#'      datreturn = datreturn
#'    )
#'  }
#' )
#' }
#'

m_StabilityUI = function(id) {
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
        shiny::wellPanel(
          shiny::fluidRow(
            shiny::column(width = 10, DT::dataTableOutput(ns("s_vals"))),
            shiny::column(width = 2, shiny::fluidRow(shiny::uiOutput(ns("s_transfer_ubb"))))
          ),
          shiny::hr(),
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
  )
}

m_StabilityServer = function(id, rv, datreturn) {
  shiny::moduleServer(id, function(input, output, session) {

    # Upload Notification. Since "uploadsource" is invalidated also when other
    # parameters within Stability are changed (because of the reactiveValues
    # thing), it has to be checked if it has changed value since the last change
    # to verify an upload
    uploadsource <- shiny::reactiveVal(NULL)
    shiny::observeEvent(getValue(rv,c("Stability","uploadsource")),{
      o.upload = getValue(rv,c("Stability","uploadsource"))
      # assign upload source if (a) hasn't been assigned yet or (b), if not
      # null, has changed since the last time, for example because other data
      # source has been uploaded
      if(is.null(uploadsource()) || uploadsource() != o.upload ){
        uploadsource(o.upload)
        shiny::updateTabsetPanel(session = session,"StabilityPanel", selected = "loaded")
        message("Stability: Uploadsource changed to ", isolate(getValue(rv,c("Stability","uploadsource"))))
        #browser()
        s_dat <- getValue(rv,c("Stability","data"))
        s_vals <- plyr::ldply(split(s_dat, s_dat[,"analyte"]), function(x) {
          x_lm <- lm(Value ~ Date, data=x)
          mon_diff <- max(mondf(x[,"Date"]))
          x_slope <- summary(x_lm)$coefficients[2,1:2]
          data.frame("mon_diff"=mon_diff, "slope"=x_slope[1], "SE_slope"=x_slope[2], "U_Stab"=abs(x_slope[1]*x_slope[2]))
        }, .id="analyte")
        setValue(rv, c("Stability","s_vals"), s_vals)
      }
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


    s_Data = shiny::reactive({
      s_dat = getValue(rv,c("Stability","data"))
      s_dat[,"analyte"] <- factor(s_dat[,"analyte"])
      return(s_dat)
    })

    # Specific UI and events
    output$s_sel_analyte <- shiny::renderUI({
      shiny::req(s_Data())
      shiny::selectInput(
        inputId = session$ns("s_sel_analyte"),
        label = "analyte",
        choices = levels(s_Data()[,"analyte"]),
        selected = levels(s_Data()[,"analyte"])[1]
      )
    })

    # Tables
    output$s_overview <- DT::renderDataTable({
      req(s_Data(), input$s_sel_analyte)
      s <- getValue(rv,c("Stability","data"))
      s[s[,"analyte"]==input$s_sel_analyte,c("Date","Value")]
    }, options = list(paging = TRUE, searching = FALSE), rownames=NULL)

    output$s_vals <- DT::renderDataTable(
      expr = {
        req(s_Data())
        s_vals <- getValue(rv,c("Stability","s_vals"))
        for (i in c("slope","SE_slope","U_Stab")) {
          s_vals[,i] <- pn(s_vals[,i], 4)
        }
        if (!is.null(getValue(datreturn,"mater_table"))) { # formerly "cert_vals"
          cert_vals <- getValue(datreturn,"mater_table")
          s_vals[,"In_Cert_Module"] <- sapply(s_vals[,"analyte"], function(x) {
            ifelse(is.null(cert_vals), "cert table not found",
              ifelse(x %in% cert_vals[,"analyte"], "Yes", "No"))
          })
        }
        return(s_vals)
      },
      options = list(dom = "t", pageLength=isolate(nrow(getValue(rv,c("Stability","s_vals"))))),
      selection = list(mode="single", target="row"), rownames=NULL
    )

    shiny::observeEvent(input$s_vals_rows_selected, {
      #browser()
      #ToDo --> allow/fix updateSelectInput
      selected <- as.character(getValue(rv,c("Stability","s_vals"))[input$s_vals_rows_selected,"analyte"])
      shiny::updateSelectInput(session = session, inputId = "s_sel_analyt", selected = selected)
    })

    shiny::observeEvent(input$s_sel_analyte, {
      shiny::req(input$s_sel_dev)
      mt <- getValue(datreturn,"mater_table")
      a <- input$s_sel_analyte
      test <- a %in% mt[,"analyte"] && is.finite(mt[which(mt[,"analyte"]==a),"mean"])
      shinyjs::toggle(id="s_sel_dev", condition = test)
    })

    # Figures
    output$s_plot <- shiny::renderPlot({
      shiny::req(s_Data(), input$s_sel_analyte)
      s <- s_Data()
      l <- s[,"analyte"]==input$s_sel_analyte

      # Convert to format used in LTS modul
      # load SD and U from certification if available
      CertVal <- mean(s[l,"Value"], na.rm=T)
      U <- 2*sd(s[l,"Value"], na.rm=T)
      U_Def <- "2s"
      if (!is.null(input$s_sel_dev)) { # !is.null(input$sel_analyt) &
        cert_vals <- getValue(datreturn,"mater_table")
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
      plot_lts_data(x=x)
    })

    output$s_sel_dev <- shiny::renderUI({
      shiny::req(s_Data(), getValue(datreturn,"mater_table"), input$s_sel_analyte)
      mt <- getValue(datreturn,"mater_table")
      a <- input$s_sel_analyte
      shiny::req(a %in% mt[,"analyte"] && is.finite(mt[which(mt[,"analyte"]==a),"mean"]))
      # show element only once mat_tab is available
      shiny::selectInput(inputId=session$ns("s_sel_dev"), label="deviation to show", choices=c("2s","U"), selected="2s")
    })

    output$s_info <- shiny::renderUI({
      shiny::req(s_Data(), input$s_sel_analyte)
      txt <- "mean and 2s of uploaded stability data for "
      if (!is.null(input$s_sel_dev)) { # !is.null(input$sel_analyt) &
        cert_vals <- getValue(datreturn,"mater_table")
        if (any(cert_vals[,"analyte"] %in% input$s_sel_analyte)) {
          txt <- paste0("mean and ", input$s_sel_dev, " of uploaded certification data for ")
        }
      }
      shiny::helpText(paste0("Figure shows ", txt, input$s_sel_analyte, "."))
    })

    # The Dropdown-Menu to select the column of materialtabelle to transfer to
    output$s_transfer_ubb <- shiny::renderUI({
      cert_vals <- getValue(datreturn,"mater_table")
      shiny::validate(shiny::need(cert_vals, message = "Please upload certification data to transfer Uncertainty values"))

      cc <- attr(cert_vals, "col_code")
      shiny::validate(shiny::need(nrow(cc) > 0, message = "Please specify a U column in material table to transfer Uncertainty values"))
      #browser()
      #shinyjs::toggle(id = "s_transfer_ubb_button", condition = nrow(cc) > 0)

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

    # Only activate Transfer-Button when materialtabelle has enough
    shiny::observeEvent(getValue(datreturn,"mater_table"), {
      cc <- attr(getValue(datreturn,"mater_table"), "col_code")
      shinyjs::toggle(id = "s_transfer_ubb_button", condition = nrow(cc) > 0)
    })


    # currently not it's own module, but could be changed in the future if necessary
    shiny::observeEvent(input$s_transfer_ubb_button, {
      # TODO Test if materialtabelle is available
      # req(s_Data(), getValue(rv,c("Stability","s_vals")), getValue(datreturn,"mater_table"), input$s_transfer_ubb)

      s_vals <- getValue(rv, c("Stability","s_vals"))
      c_vals <- getValue(datreturn, "mater_table")
      cc <- attr(c_vals, "col_code")
      U_col <- cc[cc[, "Name"] == input$s_transfer_ubb,"ID"]
      for (i in 1:nrow(s_vals)) {
        j <- which(as.character(c_vals[,"analyte"])==as.character(s_vals[i,"analyte"]))
        if (length(j)==1) {
          c_vals[j, U_col] <- s_vals[i, "U_Stab"]
        }
      }
      # Ich überschreibe im R6 Objekt die Materialtabelle, muss diese im Modul materialtabelle aber durch observeEvent aktualisieren
      setValue(datreturn, "mater_table", c_vals)
      #browser()
      #print(getValue(datreturn, "mater_table"))



      # matTab_analytes <- as.character(c_vals[, "analyte"])
      # newDF <- c_vals
      # newDF = data.frame(rep(0,length(matTab_analytes)))
      # newDF = stats::setNames(newDF, as.character(shiny::isolate(input$s_sel_analyte)))
      #
      # # find and select materialtabelle-row of same analyte and Stability
      # # type (matTab()) as chosen for Transfer
      # for (i in 1:nrow(c_vals)) {
      #   j <- which(as.character(s_vals[,"analyte"])==as.character(c_vals[i,"analyte"]))
      #   # if row exists
      #   if (length(j)==1) {
      #     s_name =  which(attr(c_vals, "col_code")[, "Name"] ==
      #                       shiny::isolate(input$s_transfer_ubb))
      #     newDF[i, attr(c_vals, "col_code")[s_name, "ID"]] <- max(s_vals[j, "U_Stab"])
      #   }
      # }
      # setValue(datreturn, "t_S", newDF)



    })


  })
}