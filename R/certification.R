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
      # CertValPlot and Export section
      fluidRow(
        column(
          10,
          wellPanel(
            fluidRow(
              column(
                3,
                # verbatimTextOutput(NS(id,"testoutput_apm")),
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
                fluidRow(checkboxInput(
                  inputId = NS(id, "pooling"),
                  label = "pooling",
                  value = FALSE
                ))
              ),
              column(9, plotOutput(
                NS(id, "overview_CertValPlot"), inline = TRUE
              ))
            )
          )),
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
        .tabelleganzuntenUI(NS(id,"mat_cert"))
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

          selected_tab = reactive(apm$selected_tab)
          
          # this data.frame contains for each analyte the following columns: ID,
          # Lab, analyte, replicate, value, unit, S_flt and L_flt
          dat = eventReactive(selected_tab(),{
            # select currently selected analyte
            
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
            a
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
          
          # Geht's nicht auch ohne "observeEvent()"?
          observeEvent(dat(),{
            .tabelleganzuntenServer("mat_cert", dat)
          }, ignoreNULL = TRUE)
         
        ### LOADED END ###s
        } else { 
          # else if nothing is loaded
          updateTabsetPanel(session = session,"certificationPanel", selected = "standBy")
        }
        
      }, ignoreInit = TRUE)
   
  })
}



# MATERIAL CERTIFICATION MODULE ------

.tabelleganzuntenUI <- function(id) {
  # ns <- NS(id)
  # tagList(
    fluidRow(
      column(
        2,
        strong("Material Certification"),
        br(),
        actionButton(inputId = NS(id,"show_table"), label = "recalculate"),
        uiOutput(NS(id,"c_fix_col_names")),
        # validate(need(input$sel_analyt, message = "please select analyte"))
        # cert_vals <- getData("cert_vals")
        # selectInput(
        #   inputId = "c_fix_col_names",
        #   label = "select internal column names",
        #   selectize = TRUE,
        #   choices = attr(cert_vals, "col_code")[, "ID"]
        # )
        uiOutput(NS(id,"c_displayed_col_name")),
        # validate(need(input$c_fix_col_names, message = "please select col name"))
        # cert_vals <- getData("cert_vals")
        # textInput(
        #   inputId = "c_displayed_col_name",
        #   label = "modify displayed column name",
        #   value = attr(cert_vals, "col_code")[attr(cert_vals, "col_code")[, "ID"] ==
        #                                         input$c_fix_col_names, "Name"]
        # )
        helpText(
          "In this interactive table you can:",
          tags$br(),
          "(1) Modify values by double click on the respective cells in the table (please note that some columns are protected).",
          tags$br(),
          "(2) Click 'recalculate' to update calculations.",
          tags$br(),
          "(3) Modify the column name for editable columns.",
          tags$br(),
          "(4) Delete editable columns completely by selecting 'delete' as column name (Caution! irreversible)."
        )
      ),
       column(10, 
             DT::DTOutput(NS(id,"matreport"))
              )
    )
  # )
}

.tabelleganzuntenServer <- function(id, dat) {
 # stopifnot(is.reactive(dat))
 # req(dat)
  moduleServer(id, function(input, output, session) {
    #dat = reactive(d()$data)

    precision2 = NULL
    
    cert_mean <- reactive({
      # req(dat(), input$precision2)
      data <- dat()[!dat()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs from calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      ifelse(input$pooling,
             roundMT(mean(data[, "value"], na.rm = T), precision2),
             roundMT(mean(sapply(
               split(data[, "value"], data[, "Lab"]), mean, na.rm = T
             )), precision2)
            )
    })

    cert_sd <- reactive({
      # req(input$precision2)
      data <- dat()[!dat()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs from calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      ifelse(input$pooling,
             roundMT(sd(data[, "value"], na.rm = T), precision2),
             roundMT(sd(sapply(
               split(data[, "value"], data[, "Lab"]), mean, na.rm = T
             )), precision2))
    })

    a = levels(dat()[["analyte"]])
    cert_vals <-
      data.frame(
        "analyte" = a,
        "mean" = NA,
        "F1" = 1,
        "F2" = 1,
        "F3" = 1,
        "cert_val" = NA,
        "sd" = NA,
        "n" = NA,
        "char" = 0,
        "U2" = 0,
        "U3" = 0,
        "U4" = 0,
        "U5" = 0,
        "U6" = 0,
        "U7" = 0,
        "com" = NA,
        "k" = 2,
        "U" = NA
      )
     
    tmp_cert_vals = cert_vals
    output$matreport = DT::renderDT(tmp_cert_vals)
    # output$matreport <-
    #   DT::renderDT(
    #     DT::datatable(
    #       data = tmp_cert_vals,
    #       editable = list(target = "cell", disable = list(
    #         columns = attr(tmp_cert_vals, "disable")
    #       )),
    #       options = list(paging = FALSE, searching = FALSE),
    #       rownames = NULL
    #     ),
    #     server = TRUE
    #   )
    
  })
}
