#' @name mod_CertLoaded
#' @aliases m_CertLoadedUI
#' @aliases m_CertLoadedServer
#'
#' @title CERTIFICATION LOADED MODULE
#'
#' @description \code{m_CertLoaded} is active as soon as Certification data was
#'  uploaded.
#'
#' @details not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param certification = reactive({rv$Certifications})
#' @param apm reactiveValues with analyte parameter
#' @param selected_tab currently selected tab
#'
#' @return nothing directly, works over apm parameter
#' @rdname mod_CertLoaded
#' @export
m_CertLoadedUI = function(id) {
  shiny::tagList(
    # Cert Value Plot start
    shiny::column(
      3,
      shiny::fluidRow(shiny::strong("Certified Value Plot")),
      shiny::fluidRow(shiny::uiOutput(shiny::NS(id, "flt_labs"))),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::numericInput(
            inputId = shiny::NS(id, "Fig01_width"),
            label = "width",
            value = 400
          )
        ),
        shiny::column(
          6,
          shiny::numericInput(
            inputId = shiny::NS(id, "Fig01_height"),
            label = "height",
            value = 400
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::strong("Download"),
          shiny::br(),
          shiny::downloadButton(outputId = 'Fig01', label = "Figure")
        )
      ),
      shiny::fluidRow(shiny::column(6, shiny::strong("mean")), shiny::column(6, shiny::strong("sd"))),
      # TODO
      shiny::fluidRow(
        shiny::column(6,
                      shiny::textOutput(shiny::NS(id,"cert_mean"))),
        shiny::column(6,
               shiny::textOutput(shiny::NS(id,"cert_sd")))
      ),
    ),
    shiny::column(9, shiny::plotOutput(
      shiny::NS(id, "overview_CertValPlot"), inline = TRUE
    ))
  )
}

#' @rdname mod_CertLoaded
#' @export
m_CertLoadedServer = function(id, certification, apm, selected_tab) {

  shiny::moduleServer(id, function(input, output, session) {

  # whereami::cat_where("CertLoaded")
    
    
    # this data.frame contains the following columns for each analyte:
    # --> [ID, Lab, analyte, replicate, value, unit, S_flt, L_flt]
    dat = shiny::reactive({
      shiny::req(selected_tab())
      # subset data frame for currently selected analyte
      current_analy = apm()[[selected_tab()]]

      cert.data = ecerto::data_of_godelement(certification()) # take the uploaded certification
      # round input values
      cert.data[, "value"] = round(cert.data[, "value"], current_analy$precision)
      cert.data <- cert.data[cert.data[, "analyte"] %in% selected_tab(), ]
      cert.data <- cert.data[!(cert.data[, "ID"] %in% current_analy$sample_filter), ]
      cert.data[, "L_flt"] <- cert.data[, "Lab"] %in% input$flt_labs

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
          is.numeric(current_analy$precision) &&
            current_analy$precision >= 0 &&
            current_analy$precision <= 6,
          message = "please check precision value: should be numeric and between 0 and 6"
        )
      )
      return(cert.data)
    }
    )

    # BOXPLOT
    output$overview_boxplot <- shiny::renderPlot({
      #if (input$opt_show_files == "boxplot") {
      TestPlot(data = dat())
    })

    # Filter laboraties (e.g. "L1")
    output$flt_labs <- shiny::renderUI({
      shiny::req(dat(), selected_tab())
      # analytelist$analytes[[i]]$lab_filter
      tmp <- dat()
      tmp <-
        tmp[tmp[, "analyte"] == selected_tab() &
              is.finite(tmp[, "value"]), ]
      choices <- levels(factor(tmp[, "Lab"]))
      # selected <- choices[which(sapply(split(tmp[, "L_flt"], factor(tmp[, "Lab"])), all))]
      selected = apm()[[selected_tab()]]$lab_filter
      shiny::selectizeInput(
        inputId = session$ns("flt_labs"),
        label = "Filter Labs",
        choices = choices,
        selected = selected,
        multiple = TRUE
      )
    })

    shiny::observeEvent(certification(), {
      us = uploadsource_of_element(certification())
      
      if (!is.null(us) && us=="RData" ) {
        shiny::updateNumericInput(session=session, inputId = "Fig01_width",value = certification()[["CertValPlot"]][["Fig01_width"]])
        shiny::updateNumericInput(session=session, inputId = "Fig01_height",value = certification()[["CertValPlot"]][["Fig01_height"]])
        shiny::updateSelectizeInput(session=session, inputId = "flt_labs",selected = certification()[["opt"]][["flt_labs"]])
      }

    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$flt_labs,{
      # message(paste0("selected lab filter: ", input$flt_labs))
      apm_tmp = apm()
      apm_tmp[[selected_tab()]]$lab_filter = input$flt_labs
      apm(apm_tmp)
    })



    # CertVal Plot
    output$overview_CertValPlot <- shiny::renderPlot({
      CertValPlot(data = dat())
    }, height = shiny::reactive({
      input$Fig01_height
    }), width = shiny::reactive({
      input$Fig01_width
    }))

    return(dat)
  })
}