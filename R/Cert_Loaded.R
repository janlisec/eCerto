#'@title CERTIFICATION LOADED MODULE
#'
#'@description \code{m_CertLoaded} is active as soon as Certification data was
#'  uploaded.
#'
#'@details not yet
#'
#'@param certification = reactive({rv$Certifications})
#'@param apm reactiveValues with analyte parameter
#'@param selected_tab currently selected tab
#'
#'@return nothing directly, works over apm parameter
#'@export
m_CertLoadedUI = function(id) {
  tagList(
    # Cert Value Plot start
    column(
      3,
      fluidRow(strong("Certified Value Plot")),
      fluidRow(uiOutput(NS(id, "flt_labs"))),
      fluidRow(
        column(
          6,
          numericInput(
            inputId = NS(id, "Fig01_width"),
            label = "width",
            value = 400
          )
        ), 
        column(
          6,
          numericInput(
            inputId = NS(id, "Fig01_height"),
            label = "height",
            value = 400
          )
        )
      ),
      fluidRow(
        column(
          6,
          strong("Download"),
          br(),
          downloadButton(outputId = 'Fig01', label = "Figure")
        )
      ),
      fluidRow(column(6, strong("mean")), column(6, strong("sd"))),
      # TODO
      fluidRow(
        column(6, 
               textOutput(NS(id,"cert_mean"))), 
        column(6, 
               textOutput(NS(id,"cert_sd")))
      ),
    ),
    column(9, plotOutput(
      NS(id, "overview_CertValPlot"), inline = TRUE
    ))
  )
}

m_CertLoadedServer = function(id, certification, apm, selected_tab) {
  
  moduleServer(id, function(input, output, session) {
    
    
    # this data.frame contains the following columns for each analyte: 
    # --> [ID, Lab, analyte, replicate, value, unit, S_flt, L_flt]
    dat = reactive({
      req(selected_tab())
      # subset data frame for currently selected analyte
      current_analy = apm$analytes[[selected_tab()]]
      
      cert.data = ecerto::data_of_godelement(certification()) # take the uploaded certification
      # round input values
      cert.data[, "value"] = round(cert.data[, "value"], current_analy$precision)
      cert.data <- cert.data[cert.data[, "analyte"] %in% selected_tab(), ]
      cert.data <-cert.data[!(cert.data[, "ID"] %in% current_analy$sample_filter), ]
      cert.data[, "L_flt"] <- cert.data[, "Lab"] %in% input$flt_labs
      
      # adjust factor levels
      cert.data[, "Lab"] <- factor(cert.data[, "Lab"])
      
      # Notify User in case that only 1 finite measurement remained within group
      validate(
        need(
          all(sapply(split(cert.data[, "value"], cert.data[, "Lab"]), length) >= 2),
          message = paste(names(which(
            sapply(split(cert.data[, "value"], cert.data[, "Lab"]), length) < 2
          ))[1], "has less than 2 replicates left. Drop an ID filter if necessary.")),
        need(
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
    output$overview_boxplot <- renderPlot({
      #if (input$opt_show_files == "boxplot") {
      TestPlot(data = dat())
    })
    
    # Filter laboraties (e.g. "L1")
    output$flt_labs <- renderUI({
      req(dat(), selected_tab())
      # analytelist$analytes[[i]]$lab_filter
      tmp <- dat()
      tmp <-
        tmp[tmp[, "analyte"] == selected_tab() &
              is.finite(tmp[, "value"]), ]
      choices <- levels(factor(tmp[, "Lab"]))
      # selected <- choices[which(sapply(split(tmp[, "L_flt"], factor(tmp[, "Lab"])), all))]
      selected = apm$analytes[[selected_tab()]]$lab_filter
      selectizeInput(
        inputId = session$ns("flt_labs"),
        label = "Filter Labs",
        choices = choices,
        selected = selected,
        multiple = TRUE
      )
    })
    
    observeEvent(certification(), {
      if (uploadsource_of_element(certification())=="RData" ) {
        updateNumericInput(inputId = "Fig01_width",value = certification()[["CertValPlot"]][["Fig01_width"]])
        updateNumericInput(inputId = "Fig01_height",value = certification()[["CertValPlot"]][["Fig01_height"]])
        updateSelectizeInput(inputId = "flt_labs",selected = certification()[["opt"]][["flt_labs"]])
      }
      
    }, ignoreNULL = TRUE)
    
    observeEvent(input$flt_labs,{
      # message(paste0("selected lab filter: ", input$flt_labs))
      apm$analytes[[selected_tab()]]$lab_filter = input$flt_labs
    })
    
    
    
    # CertVal Plot
    output$overview_CertValPlot <- renderPlot({
      CertValPlot(data = dat())
    }, height = reactive({
      input$Fig01_height
    }), width = reactive({
      input$Fig01_width
    }))
    
    return(dat)
  })
}