# CERTIFICATION MODULE -------------------------

#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
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
      .analyteModuleUI(NS(id, "cert_filter")),

      fluidRow(
        column(
          10,
          wellPanel(
            fluidRow(
              # # Cert Value Plot start
              # column(
              #   3,
              #   fluidRow(strong("Certified Value Plot")),
              #   fluidRow(uiOutput(NS(id, "flt_labs"))),
              #   fluidRow(
              #     column(
              #       6,
              #       numericInput(
              #         inputId = NS(id, "Fig01_width"),
              #         label = "width",
              #         value = 400
              #       )
              #     ), 
              #     column(
              #       6,
              #       numericInput(
              #         inputId = NS(id, "Fig01_height"),
              #         label = "height",
              #         value = 400
              #       )
              #     )
              #   ),
              #   fluidRow(
              #     # column(6,
              #     #        numericInput(
              #     #          inputId = NS(id, "precision2"),
              #     #          label = "Precision",
              #     #          value = 4
              #     #        )),
              #     column(
              #       6,
              #       strong("Download"),
              #       br(),
              #       downloadButton(outputId = 'Fig01', label = "Figure")
              #     )
              #   ),
              #   fluidRow(column(6, strong("mean")), column(6, strong("sd"))),
              #   fluidRow(column(6, textOutput("cert_mean")), column(6, textOutput("cert_sd"))),
              #   # fluidRow(
              #   #   checkboxInput(
              #   #   inputId = NS(id, "pooling"),
              #   #   label = "pooling",
              #   #   value = FALSE
              #   # )
              #   # )
              # ),
              # column(9, plotOutput(
              #   NS(id, "overview_CertValPlot"), inline = TRUE
              # ))
              # # Cert Value Plot Ende
              .CertLoadedUI(NS(id,"loaded"))
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
        )), # Ende # CertValPlot and Export section
      # Overall Certification
      wellPanel(
        .materialtabelleUI(NS(id,"mat_cert"))
      )
    )
    
  )
  
}

#' Title
#'
#' @param id 
#' @param d reactive Certification 'reactive({rv$Certifications})'
#'
#' @return
#' @export
#'
#' @examples
.CertificiationServer = function(id, d) {
  #stopifnot(is.reactivevalues(d))
  moduleServer(id, function(input, output, session) {
    
    observeEvent(d(), {
        #if loaded (successfully), male area visible
        # AGAIN: SUCCESSFULLY LOADED HERE!
        if(!is.null(d())){
          updateTabsetPanel(session = session,"certificationPanel", selected = "loaded")
          #### create the parameter list for the analytes ###
          # it will contain information about he selected analyte tab, and for
          # each analyte the wanted precision, the filtered sample id, which
          # sample ids are available to be filtered at all and, for completion,
          # the analyte name in case the list name fails
          param_template = list(
            "precision" = NULL, 
            "sample_filter" = NULL, # saving which samples where selected for filter
            "sample_ids" = NULL, # which samples are available
            "analytename" = NULL
          )
          analytes = reactive({levels(data_of_godelement(d())[, "analyte"])})
          a_param_list = rep(list(param_template),length(analytes()))
          for (i in 1:length(a_param_list)) {
            # add analyte name to list
            a_param_list[[i]]$analytename = as.list(analytes())[[i]]
            # add available id's of samples to list
            tmp = data_of_godelement(d())
            ids = tmp[tmp[["analyte"]] == as.list(analytes())[[i]], "ID"]
            a_param_list[[i]]$sample_ids = ids[!is.na(ids)]
          }
          # set names of sublists to analyte names
          a_param_list = setNames(a_param_list, analytes())
          l = list("selected_tab" = NULL)
          l$analytes = a_param_list
          apm = do.call("reactiveValues", l) # finally, create reactiveValues
          # end param list
          
          ##########################
          .analyteModuleServer("cert_filter",apm)
          ##########################

          # selected_tab = reactive(apm$selected_tab)
          # 
          # # this data.frame contains the following columns for each analyte: 
          # # --> [ID, Lab, analyte, replicate, value, unit, S_flt, L_flt]
          # dat = eventReactive(selected_tab(), {
          #   
          #   # subset data frame for currently selected analyte
          #   current_analy = apm$analytes[[selected_tab()]]
          #   
          #   a = ecerto::data_of_godelement(d())
          #   a[, "value"] = round(a[, "value"], current_analy$precision)
          #   a <- a[a[, "analyte"] %in% selected_tab(), ]
          #   a <-a[!(a[, "ID"] %in% current_analy$sample_filter), ]
          # 
          #   # Notify User in case that only 1 finite measurement remained within group
          #   validate(
          #     need(
          #       all(sapply(split(a[, "value"], a[, "Lab"]), length) >= 2),
          #       message = paste(names(which(
          #         sapply(split(a[, "value"], a[, "Lab"]), length) < 2
          #       ))[1], "has less than 2 replicates left. Drop an ID filter if necessary.")),
          #     need(
          #       is.numeric(current_analy$precision) &&
          #         current_analy$precision >= 0 &&
          #         current_analy$precision <= 6,
          #       message = "please check precision value: should be numeric and between 0 and 6"
          #     )
          #   )
          #   return(a)
          # }, ignoreNULL = TRUE,ignoreInit = TRUE)
          # 
          # # BOXPLOT
          # output$overview_boxplot <- renderPlot({
          #   #if (input$opt_show_files == "boxplot") {
          #     TestPlot(data = dat())
          # })
          # 
          # # CertVal Plot
          # output$overview_CertValPlot <- renderPlot({
          #   CertValPlot(data = dat())
          # }, height = reactive({
          #   input$Fig01_height
          # }), width = reactive({
          #   input$Fig01_width
          # }))
          dat = .CertLoadedServer("loaded",d = d, apm = apm)
          
          # Geht's nicht auch ohne "observeEvent()"?
          observeEvent(dat(),{
            .materialtabelleServer("mat_cert", dat)
          }, ignoreNULL = TRUE)
         
        ### LOADED END ###s
        } else { 
          # else if nothing is loaded, keep Panel empty
          updateTabsetPanel(session = session,"certificationPanel", selected = "standBy")
        }
        
      }, ignoreInit = TRUE)
   
  })
}

.CertLoadedServer = function(id, d, apm) {
  #stopifnot(is.reactivevalues(d))
  moduleServer(id, function(input, output, session) {
    selected_tab = reactive(apm$selected_tab)
    
    # this data.frame contains the following columns for each analyte: 
    # --> [ID, Lab, analyte, replicate, value, unit, S_flt, L_flt]
    dat = eventReactive(selected_tab(), {
      
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
    }, ignoreNULL = TRUE,ignoreInit = TRUE)
    
    # BOXPLOT
    output$overview_boxplot <- renderPlot({
      #if (input$opt_show_files == "boxplot") {
      TestPlot(data = dat())
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
        # column(6,
        #        numericInput(
        #          inputId = NS(id, "precision2"),
        #          label = "Precision",
        #          value = 4
        #        )),
        column(
          6,
          strong("Download"),
          br(),
          downloadButton(outputId = 'Fig01', label = "Figure")
        )
      ),
      fluidRow(column(6, strong("mean")), column(6, strong("sd"))),
      fluidRow(column(6, textOutput("cert_mean")), column(6, textOutput("cert_sd"))),
      # fluidRow(
      #   checkboxInput(
      #   inputId = NS(id, "pooling"),
      #   label = "pooling",
      #   value = FALSE
      # )
      # )
    ),
    column(9, plotOutput(
      NS(id, "overview_CertValPlot"), inline = TRUE
    ))
  )
  
# Cert Value Plot Ende
  
  
}

