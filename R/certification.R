# CERTIFICATION MODULE -------------------------

#' Title
#'
#' @param id 
#'
#' @return
#' @export
.CertificationUI = function(id) {
  tabsetPanel(
    id = NS(id, "certificationPanel"),
    type = "hidden",
    # when nothing is loaded
    tabPanel(title = "standby-Panel", value  = "standby", "empty panel content"),
    # when something is loaded
    tabPanel(
      title = "active-Panel",
      value = "loaded",
      fluidRow(
        column(4,wellPanel("selection box")),
        column(8,
          wellPanel(
            # --- --- --- --- --- --- --- --- ---
            .analyteModuleUI(NS(id, "cert_filter")),
            # --- --- --- --- --- --- --- --- ---
          )
        )
      ),
      fluidRow(
        column(
          10,
          wellPanel(
            fluidRow(
              # --- --- --- --- --- --- ---
              .CertLoadedUI(NS(id,"loaded"))
              # --- --- --- --- --- --- ---
            )
          )),
        
        ##### Download-Teil
        column(
          2,
          wellPanel(
            fluidRow(strong("Download Report")),
            fluidRow(
              radioButtons(
                inputId = NS(id, 'output_file_format'),
                label = NULL,
                choices = c('PDF', 'HTML', 'Word'),
                inline = TRUE
              )
            ),
            fluidRow(
              column(6, align = "center", downloadButton('FinalReport', label = "Analyte")),
              column(
                6,
                align = "center",
                downloadButton('MaterialReport', label =
                                 "Material")
              )
            ),
            fluidRow(checkboxInput(
              inputId = NS(id, "show_code"), label = "Show Code in Report"
            ))
          )
        )), 
    )
    
  )
  
}

.CertificiationServer = function(id, d, datreturn) {
  #stopifnot(is.reactivevalues(d))
  moduleServer(id, function(input, output, session) {
    
    observeEvent(d(), {
      #if loaded (successfully), make area visible
      # AGAIN: SUCCESSFULLY LOADED HERE!
      if(!is.null(d())){
        message("Certification Module start")
        updateTabsetPanel(session = session,"certificationPanel", selected = "loaded")
        #### create the parameter list for the analytes ###
        # it will contain information about he selected analyte tab, and for
        # each analyte the wanted precision, the filtered sample id, which
        # sample ids are available to be filtered at all and, for completion,
        # the analyte name in case the list name fails
        param_template = list(
          "precision" = NULL, 
          "sample_filter" = NULL, # saving which samples where selected for filter
          "sample_ids" = NULL, # which samples are available for the filter
          "lab_filter" = NULL, # filter of laboratories (e.g. L1)
          "analytename" = NULL
        )
        analytes = reactive({levels(data_of_godelement(d())[, "analyte"])})
        # create list with lists of all analytes (i.e. a nested list)
        a_param_list = rep(list(param_template), length(analytes()))
        for (i in 1:length(a_param_list)) {
          # add analyte name to list
          a_param_list[[i]]$analytename = as.list(analytes())[[i]]
          # add available id's of samples to list
          tmp = data_of_godelement(d())
          ids = tmp[tmp[["analyte"]] == as.list(analytes())[[i]], "ID"]
          a_param_list[[i]]$sample_ids = ids[!is.na(ids)] # fill available ids
        }
        # set names of sublists to analyte names
        a_param_list = setNames(a_param_list, analytes())
        l = list("selected_tab" = NULL)
        l$analytes = a_param_list
        apm = do.call("reactiveValues", l) # finally, create reactiveValues
        # end param list
        
        # --- --- --- --- --- --- --- --- --- --- ---
        .analyteModuleServer("cert_filter", apm)
        # --- --- --- --- --- --- --- --- --- --- ---
        
        # --- --- --- --- --- --- --- --- --- --- ---
        dat = .CertLoadedServer("loaded",d = d, apm = apm)
        # --- --- --- --- --- --- --- --- --- --- ---
        
        # Calculates statistics for all available labs
        # formerly: lab_means()
        # Format example: 
        # Lab       mean           sd n
        # L1  L1 0.04551667 0.0012560520 6
        # L2  L2 0.05150000 0.0007563068 6
        # L3  L3 0.05126667 0.0004926121 6
        lab_statistics = reactive({
          # data <- dat()
          req(dat())
          out <-
            plyr::ldply(split(dat()$value, dat()$Lab), function(x) {
              data.frame(
                "mean" = mean(x, na.rm = T),
                "sd" = sd(x, na.rm = T),
                "n" = sum(is.finite(x))
              )
            }, .id = "Lab")
          rownames(out) <- out$Lab
          suppressWarnings(KS_p <-
                             stats::ks.test(
                               x = out$mean,
                               y = "pnorm",
                               mean = mean(out$mean),
                               sd = sd(out$mean)
                             )$p.value)
          
          # TODO
          # assign(
          #   "normality_statement",
          #   value = paste0(
          #     "The data is",
          #     ifelse(KS_p < 0.05, " not ", " "),
          #     "normally distributed (KS_p=",
          #     formatC(KS_p, format = "E", digits = 2),
          #     ")."
          #   ),
          #   envir = env_perm
          # )
          return(out)
        })
        
        observe({
          datreturn$lab_statistics = lab_statistics()
          message(".CertificationServer -- lab_statistics created")
          datreturn$selectedAnalyteDataframe = dat()
          # console log
          message(paste0(".CertificiationServer -- analyte selected: ",dat()[1,"analyte"]))
        })
        
        # observeEvent(lab_statistics(),{
        #   datreturn$lab_statistics = lab_statistics()
        #   message(".CertificationServer -- lab_statistics created")
        # }, ignoreInit = TRUE)
        # 
        # observeEvent(dat(),{
        #   datreturn$selectedAnalyteDataframe = dat()
        # 
        #   # console log
        #   message(paste0(".CertificiationServer -- analyte selected: ",dat()[1,"analyte"]))
        # })
        
        
      
        ### LOADED END ###s
      } else { 
        # else if nothing is loaded, keep Panel empty
        updateTabsetPanel(session = session,"certificationPanel", selected = "standBy")
      }
    }, ignoreInit = TRUE)

  })
}
# LOADED CERTIFICATION MODULE --------------
# following is processed when a certification was loaded

.CertLoadedUI = function(id) {
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
      fluidRow(column(6, textOutput("cert_mean")), column(6, textOutput("cert_sd"))),
    ),
    column(9, plotOutput(
      NS(id, "overview_CertValPlot"), inline = TRUE
    ))
  )
}

.CertLoadedServer = function(id, d, apm) {
  stopifnot(is.reactivevalues(apm))
  moduleServer(id, function(input, output, session) {
    selected_tab = reactive(apm$selected_tab)
    
    # this data.frame contains the following columns for each analyte: 
    # --> [ID, Lab, analyte, replicate, value, unit, S_flt, L_flt]
    dat = reactive({
      req(selected_tab())
      # subset data frame for currently selected analyte
      current_analy = apm$analytes[[selected_tab()]]
      
      a = ecerto::data_of_godelement(d())
      a[, "value"] = round(a[, "value"], current_analy$precision)
      a <- a[a[, "analyte"] %in% selected_tab(), ]
      a <-a[!(a[, "ID"] %in% current_analy$sample_filter), ]
      
      # Notify User in case that only 1 finite measurement remained within group
      validate(
        need(
          all(sapply(split(a[, "value"], a[, "Lab"]), length) >= 2),
          message = paste(names(which(
            sapply(split(a[, "value"], a[, "Lab"]), length) < 2
          ))[1], "has less than 2 replicates left. Drop an ID filter if necessary.")),
        need(
          is.numeric(current_analy$precision) &&
            current_analy$precision >= 0 &&
            current_analy$precision <= 6,
          message = "please check precision value: should be numeric and between 0 and 6"
        )
      )
      return(a)
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
    
    observeEvent(input$flt_labs,{
      # message(paste0("selected lab filter: ", input$flt_labs))
      apm$analytes[[selected_tab()]]$lab_filter = input$flt_labs
    })
    
    # # console log
    # observeEvent(dat(),{
    #   message(paste0(".CertLoadedServer -- currently: ", dat()[1,"analyte"]))
    # })
    
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



