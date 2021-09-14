#' @name mod_Certification
#' @aliases m_CertificationUI
#' @aliases m_CertificationServer
#'
#' @title Certification.
#'
#' @description \code{m_CertificationServer} is the module for handling the
#'  Certification part but also contains the materialtabelle (until further
#'  changes).
#'
#' @details not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv the whole R6 object
#' @param datreturn the session data object
#'
#' @return nothing
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_CertificationUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'   rv <- reactiveClass$new(init_rv()) # initiate persistent variables
#'   shiny::observe({setValue(rv, c("Certification","data"), test_Certification_Excel()) })
#'   shiny::observe({set_uploadsource(rv, "Certification", uploadsource = "Excel") })
#'   datreturn <- reactiveClass$new(init_datreturn()) # initiate runtime variables
#'
#'  m_CertificationServer(
#'      id = "test",
#'      rv = rv,
#'      apm.input = shiny::reactiveVal(),
#'      datreturn = datreturn
#'    )
#'  }
#' )
#' }
#'
#' @rdname mod_Certification
#' @export
m_CertificationUI = function(id) {
  ns <- shiny::NS(id)
  shiny::tabsetPanel(
    id = ns("certificationPanel"),
    type = "hidden",
    # when nothing is loaded
    shiny::tabPanel(
      title = "standby-Panel",
      value  = "standby",
      "empty panel content" # Platzhalter, falls aus Versehen leere Seite aufgerufen wird
    ),
    # when something is loaded
    shiny::tabPanel(
      title = "active-Panel",
      value = "loaded",
      shiny::fluidRow(
        shiny::column(
          width=2,
          shiny::wellPanel(
            shiny::checkboxGroupInput(
              inputId = ns("certification_view"),
              label = "Select View:",
              choices = c("boxplot"="boxplot",
                          "Statistics 1" = "stats",
                          "Statistics 2" = "stats2",
                          "QQ-Plot" = "qqplot",
                          "material table" = "mt"),
              selected = c("boxplot","mt")
            )
          )
        ),
        # --- --- --- --- --- --- --- --- ---
        shiny::column(width=7, shiny::wellPanel(m_analyteUI(ns("analyteModule")))),
        # --- --- --- --- --- --- --- --- ---
        ##### Download-Teil
        shiny::column(
          width = 3,
          shiny::wellPanel(
            shiny::fluidRow(shiny::strong("Download Report")),
            shiny::fluidRow(
              shiny::radioButtons(
                inputId = ns("output_file_format"),
                label = NULL,
                choices = c('PDF', 'HTML', 'Word'),
                inline = TRUE
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                align = "left",
                shiny::downloadButton('FinalReport', label = "Analyte")),
              shiny::column(
                width = 6,
                align = "right",
                shiny::downloadButton('MaterialReport', label = "Material")
              )
            )#,
            #shiny::fluidRow(
            #  shiny::checkboxInput(inputId = ns("show_code"), label = "Show Code in Report")
            #)
          )
        )
      ),
      shiny::conditionalPanel(
        # check if checkBoxes are marked for material table
        condition = "input.certification_view.indexOf('boxplot') > -1",
        ns = shiny::NS(id), # namespace of current module,
        shiny::fluidRow(
          # --- --- --- --- --- --- ---
          shiny::column(
            width = 2,
            shiny::strong(
              shiny::actionLink(
                inputId = ns("certifiedValuePlot_link"),
                label = "Certified Value Plot"
              )
            ),
            shiny::uiOutput(ns("flt_labs")),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::numericInput(
                  inputId = ns("Fig01_width"),
                  label = "width",
                  value = 400
                )
              ),
              shiny::column(
                width = 6,
                shiny::numericInput(
                  inputId = ns("Fig01_height"),
                  label = "height",
                  value = 400
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::strong("Download"),
                shiny::br(),
                shiny::downloadButton(outputId = 'Fig01', label = "Figure")
              ),
              shiny::column(
                width = 6,
                # shiny::strong(""),
                # shiny::br(),
                shiny::checkboxInput(inputId = ns("annotate_id"), label = "Show IDs", value = FALSE)
              )
            ),
            shiny::p(),
            shiny::fluidRow(
              shiny::column(width = 6, shiny::strong("mean")),
              shiny::column(width = 6, shiny::strong("sd"))
            ),
            shiny::fluidRow(
              shiny::column(width = 6, shiny::textOutput(ns("cert_mean"))),
              shiny::column(width = 6, shiny::textOutput(ns("cert_sd")))
            ),
          ),
          shiny::column(width = 10, shiny::plotOutput(
            ns("overview_CertValPlot"), inline = TRUE
          ))
          #shiny::wellPanel(shiny::fluidRow(m_CertLoadedUI(ns("loaded"))))
        )
      ),
      # Stats (on Lab distributions)
      shiny::conditionalPanel(
        condition = "input.certification_view.indexOf('stats') > -1",
        ns = shiny::NS(id), # namespace of current module
        shiny::strong(
          shiny::actionLink(
            inputId = ns("stat_link"),
            label = "Tab.1 Statistics regarding lab variances and outlier detection"
          )
        ),
        DT::dataTableOutput(ns("overview_stats"))
      ),
      # Stats2 (on Lab means)
      shiny::conditionalPanel(
        condition = "input.certification_view.indexOf('stats2') > -1",
        ns = shiny::NS(id),
        shiny::strong(
          shiny::actionLink(
            inputId = ns("stat2_link"),
            label = "Tab.2 Statistics regarding lab mean distribution"
          )
        ),
        DT::dataTableOutput(ns("overview_mstats")),
        htmltools::p(),
        shiny::textOutput(outputId = ns("normality_statement")),
        htmltools::p(),
        shiny::conditionalPanel(
          condition = "input.certification_view.indexOf('qqplot') > -1",
          ns = shiny::NS(id),
          shiny::plotOutput(ns("qqplot")),
          htmltools::p()
        )
      ),
      # materialtabelle
      shiny::conditionalPanel(
        # check if checkBox is marked for material table
        condition = "input.certification_view.indexOf('mt') > -1",
        ns = shiny::NS(id), # namespace of current module
        # --- --- --- --- --- --- --- --- --- --- ---
        m_materialtabelleUI(ns("mat_cert"))
        # --- --- --- --- --- --- --- --- --- --- ---
      ),
    )
  )
}

#' @rdname mod_Certification
#' @export
m_CertificationServer = function(id, rv, datreturn) {
  shiny::moduleServer(id, function(input, output, session) {

    apm <- shiny::reactiveVal() # what will be returned by the module
    rdataupload<- shiny::reactiveVal() # forwarded to materialtabelle
    renewTabs <- shiny::reactiveVal(NULL) # command to renew Tabs in analyte-tabs module

    # Upload Notification. Since "uploadsource" is invalidated also when other
    # parameters within Certification are changed (because of the reactiveValues
    # thing), it has to be checked if it has changed value since the last change
    # to verify an upload
    uploadsource <- shiny::reactiveVal(NULL)
    UpdateInputs = shiny::reactiveVal(0)
    shiny::observeEvent(getValue(rv,c("Certification","uploadsource")),{
      o.upload = getValue(rv,c("Certification","uploadsource"))
      # assign upload source if (a) hasn't been assigned yet or (b), if not
      # null, has changed since the last time, for example because other data
      # source has been uploaded
      if(is.null(uploadsource()) || uploadsource() != o.upload ){
        uploadsource(o.upload)
        message("Certification: Uploadsource changed to ", o.upload, "; initiate apm")
        if(o.upload=="Excel") {
          # Creation of AnalyteParameterList.
          apm(
            analyte_parameter_list(
              getValue(rv, c("Certification", "data"))
            ))
        } else if(startsWith(o.upload, "RData")) {
          # only forward rData Upload after RData was uploaded
          message("Certification: forward RData to Materialtabelle")
          rdataupload(getValue(rv,c("materialtabelle")))
          if(!is.null(getValue(rv,c("General","apm")))) {
            # RData contains element "apm"
            apm(getValue(rv,c("General","apm")))
          } else {
            # RData did not contain "apm" --> create
            apm(analyte_parameter_list(getValue(rv,c("Certification","data"))))
          }
        } else {
          stop("unknown Upload Type")
        }
        UpdateInputs(UpdateInputs() +1 )
        renewTabs(1) # give a signal to renew tabs
        # Change the UploadPanel to the --loaded-- version
        shiny::updateTabsetPanel(session = session, "certificationPanel", selected = "loaded")
      }
    })


    # --- --- --- --- --- --- --- --- --- --- ---
    # Materialtabelle is embedded in Certification-UI, that's why it is here
    m_materialtabelleServer(
      id = "mat_cert",
      rdataUpload = rdataupload,
      datreturn = datreturn
    )
    # --- --- --- --- --- --- --- --- --- --- ---
    # selected analyte, sample filter, precision
    tablist <- shiny::reactiveVal(NULL) # store created tabs; to be replaced in future versions
    selected_tab <- ecerto::m_analyteServer("analyteModule", apm, renewTabs, tablist)
    # --- --- --- --- --- --- --- --- --- --- ---

    current_apm <- shiny::reactive({apm()[[selected_tab()]]})

    filtered_labs <- shiny::reactiveVal(NULL)

    # this data.frame contains the following columns for each analyte:
    # --> [ID, Lab, analyte, replicate, value, unit, S_flt, L_flt]
    dat <- shiny::reactive({
      shiny::req(selected_tab())
      # subset data frame for currently selected analyte

      message("Cert_Load: dat-reactive invalidated")
      cert.data <- getValue(rv,c("Certification","data")) # take the uploaded certification
      # round input values
      cert.data[, "value"] <- round(cert.data[, "value"], current_apm()$precision)
      cert.data <- cert.data[cert.data[, "analyte"] %in% selected_tab(), ]
      cert.data <- cert.data[!(cert.data[, "ID"] %in% current_apm()$sample_filter), ]
      cert.data[, "L_flt"] <- cert.data[, "Lab"] %in% filtered_labs()
      # adjust factor levels
      cert.data[, "Lab"] <- factor(cert.data[, "Lab"])
      # Notify User in case that only 1 finite measurement remained within group
      shiny::validate(
        shiny::need(
          all(sapply(split(cert.data[, "value"], cert.data[, "Lab"]), length) >= 2),
          message = paste(names(which(
            sapply(split(cert.data[, "value"], cert.data[, "Lab"]), length) < 2
          ))[1], "has less than 2 replicates left. Drop an ID filter if necessary.")),
        shiny::need(
          is.numeric(current_apm()$precision) &&
            current_apm()$precision >= 0 &&
            current_apm()$precision <= 6,
          message = "please check precision value: should be numeric and between 0 and 6"
        )
      )
      return(cert.data)
    })

    # BOXPLOT
    output$overview_boxplot <- shiny::renderPlot({
      TestPlot(data = dat())
    })


    # Filter laboratories (e.g. "L1")
    output$flt_labs <- shiny::renderUI({
      shiny::req(dat(), selected_tab())
      tmp <- dat()
      tmp <- tmp[tmp[, "analyte"] == selected_tab() & is.finite(tmp[, "value"]), ]
      choices <- levels(factor(tmp[, "Lab"]))
      selected = current_apm()$lab_filter
      shiny::selectizeInput(
        inputId = session$ns("flt_labs"),
        label = "Filter Labs",
        choices = choices,
        selected = selected,
        multiple = TRUE
      )
    })

    shiny::observeEvent(UpdateInputs(), {
      message("certification: UpdateInputs() observeEvent")

      us = getValue(rv,c("Certification","uploadsource"))
      if (startsWith(us,"RData") ) {
        shiny::updateNumericInput(
          session=session,
          inputId = "Fig01_width",
          value = getValue(
            rv,
            c("Certification_processing","CertValPlot","Fig01_width")
          )
        )
        shiny::updateNumericInput(
          session=session,
          inputId = "Fig01_height",
          value = shiny::isolate(getValue(rv, c("Certification_processing","CertValPlot","Fig01_height")))
        )
        shiny::updateSelectizeInput(
          session=session,
          inputId = "flt_labs",
          selected = shiny::isolate(getValue(rv, c("Certification_processing","opt")))[["flt_labs"]]
        )
        shiny::updateNumericInput(
          session=session,
          inputId = "Fig01_width",
          value = 150 + 40 * length(levels(factor(shiny::isolate(getValue(rv, c("Certification","data")))[, "Lab"])))
        )
      }
    }, ignoreInit  = TRUE)

    shiny::observeEvent(input$flt_labs,{
      # don't perform any update on apm() if variables are same
      # without this if statement apm() was changed on initial load and L_flt-list-item was deleted
      if (!identical(sort(filtered_labs()),sort(input$flt_labs)) & !is.null(selected_tab)) {
        message("Cert_load: lab filter: ", input$flt_labs)
        filtered_labs_tmp = filtered_labs()
        tmp <- dat()[dat()[, "analyte"] == selected_tab() & is.finite(dat()[, "value"]), ]
        n_labs <- length(levels(factor(tmp[, "Lab"])))
        if(length(input$flt_labs) < n_labs) {
          filtered_labs(input$flt_labs)
          apm_tmp <- apm()
          # L_flt list item was deleted because NULL can not be assigned to a list element in the standard way
          if (is.null(input$flt_labs)) {
            apm_tmp[[selected_tab()]]["lab_filter"] <- list(NULL)
          } else {
            apm_tmp[[selected_tab()]]$lab_filter <- input$flt_labs
          }
          apm(apm_tmp)
        } else {
          # if not valid --> reset
          shinyalert::shinyalert(
            title = "Too many labs filtered",
            text = "You can not filter all labs."
          )
          # currently a bit hacky. since just giving filtered_labs_tmp would not
          # trigger the reactive, first reset to the first and then give the
          # actual filtered labs
          apm_tmp = apm()
          apm_tmp[[selected_tab()]]$lab_filter = filtered_labs_tmp[1]
          apm(apm_tmp)
          apm_tmp = apm()
          apm_tmp[[selected_tab()]]$lab_filter = filtered_labs_tmp
          apm(apm_tmp)
        }
      }


    },
    # NULL should NOT be ignored, otherwise the LAST lab can't get deselected.
    ignoreNULL = FALSE, ignoreInit=TRUE
    )

    output$cert_mean <- shiny::renderText({
      getValue(datreturn,"cert_mean")
    })

    output$cert_sd <- shiny::renderText({
      getValue(datreturn,"cert_sd")
    })


    # CertVal Plot
    output$overview_CertValPlot <- shiny::renderPlot({
      CertValPlot(data = dat(), annotate_id=input$annotate_id)
    }, height = shiny::reactive({
      input$Fig01_height
    }), width = shiny::reactive({
      input$Fig01_width
    }))

    CertValPlot_list <- shiny::reactive({
      shiny::req(input$Fig01_width)
      shiny::req(input$Fig01_height)
      list(
        "show" = TRUE,
        "fnc" = deparse(CertValPlot),
        "call" = str2lang('CertValPlot(data=data)'),
        "Fig01_width" = input$Fig01_width,
        "Fig01_height" = input$Fig01_height
      )
    })

    shiny::observeEvent(CertValPlot_list(),{
      message("CertValPlot_list changed; set rv.CertValPlot")
      setValue(rv,c("Certification_processing","CertValPlot"), CertValPlot_list())
    }, ignoreInit = TRUE)


    # Calculates statistics for all available labs
    # formerly: lab_means()
    # Format example:
    # Lab       mean    sd       n
    # L1 0.04551667 0.0012560520 6
    # L2 0.05150000 0.0007563068 6
    # L3 0.05126667 0.0004926121 6
    lab_statistics = shiny::reactive({
      # data <- dat()
      shiny::req(dat())
      message("Certification: dat() changed; change lab_statistics")
      out <- plyr::ldply(split(dat()$value, dat()$Lab), function(x) {
        data.frame(
          "mean" = mean(x, na.rm = T),
          "sd" = stats::sd(x, na.rm = T),
          "n" = sum(is.finite(x))
        )
      }, .id = "Lab")
      rownames(out) <- out$Lab
      return(out)
    })


    output$normality_statement <- shiny::renderText({
      l = lab_statistics()
      suppressWarnings(
        KS_p <- stats::ks.test(x = l$mean, y = "pnorm", mean = mean(l$mean), sd = stats::sd(l$mean))$p.value
      )
      normality_statement <- paste0(
        "The data is",
        ifelse(KS_p < 0.05, " not ", " "),
        "normally distributed (KS_p=",
        #formatC(KS_p, format = "E", digits = 2),
        ecerto::pn(KS_p),
        ")."
      )
    })

    shiny::observeEvent(dat(),{
      message("Certification: dat() changed, set datreturn.selectedAnalyteDataframe")
      ecerto::setValue(datreturn, "selectedAnalyteDataframe", dat())
    })

    shiny::observeEvent(lab_statistics(),{
      message("Certification: lab_statistics() changed, set datreturn.lab_statistics")
      ecerto::setValue(datreturn, "lab_statistics", lab_statistics())
    })

    shiny::observeEvent(input$certification_view, {
      # Box "QQ-Plot" clickable? Depends in state of Box above it
      shinyjs::disable(selector = "#certification-certification_view input[value='qqplot']")
      if("stats2" %in% input$certification_view) {
        shinyjs::enable(selector = "#certification-certification_view input[value='qqplot']")
      }
      # only change rv-object if CertValplot has changed
      show_Boxplot =  "boxplot" %in% input$certification_view
      if(
        is.null(getValue(rv,c("Certification_processing","CertValPlot","show"))) ||
        show_Boxplot != getValue(rv,c("Certification_processing","CertValPlot","show"))
      ) {
        message("CERTIFICATION: SET Cert_ValPlot")
        setValue(rv,c("Certification_processing","CertValPlot","show"),show_Boxplot)
      }
    }, ignoreInit = TRUE)

    output$overview_stats <- DT::renderDataTable({
      Stats(data = dat(), precision = current_apm()$precision)
    }, options = list(dom = "t", pageLength=100, scrollX = TRUE), selection=list(mode = 'single', target = 'row'), rownames = NULL)

    # mStats
    output$overview_mstats <- DT::renderDataTable({
      mstats(data = dat(), precision = current_apm()$precision)
    }, options = list(dom = "t", pageLength=1, scrollX = TRUE), selection=list(mode = 'single', target = 'row'), rownames = NULL)

    output$qqplot <- shiny::renderPlot({
      shiny::req(lab_statistics())
      y <- lab_statistics()[, "mean"]
      stats::qqnorm(y = y)
      stats::qqline(y = y, col = 2)
    }, height = 400, width = 400)

    shiny::observeEvent(input$stat_link,{
      help_the_user("certification_laboratoryStatistics")
    })
    shiny::observeEvent(input$stat2_link,{
      help_the_user("certification_meanDistribution")
    })
    shiny::observeEvent(input$certifiedValuePlot_link, {
      help_the_user("certification_boxplot")
    })

    # whenever the analyte parameter like lab filter, sample filter etc are changed
    shiny::observeEvent(apm(), {
      message("certification: apm changed, set rv.apm")
      setValue(rv,c("General","apm"), apm())
    }, ignoreNULL = TRUE)


  })
}
