#' Title
#'
#' @param id 
#' @param d 
#'
#' @return
#' @export
#'
#' @examples
.CertificiationServer = function(id, d) {
  #stopifnot(is.reactivevalues(d))
  moduleServer(id, function(input, output, session) {
    
    observeEvent(d(), ignoreInit = TRUE,
                 {
                   
                   #if loaded (successfully)
                   if(!is.null(d())){
                     updateTabsetPanel(session = session,"certificationPanel", selected = "loaded")
                     filt = filterServer("cert_filter",d)
                     
                     dat = reactive({
                       a = ecerto::data_of_godelement(d())
                       a[, "value"] = round(a[, "value"], filt$precision() )
                       a <- a[a[, "analyte"] %in% filt$analyte(), ]
                       a <-a[!(a[, "ID"] %in% filt$id_filt()), ]
                       
                       # Notify User in case that only 1 finite measurement remained within group
                       validate(
                         need(
                           all(sapply(split(a[, "value"], a[, "Lab"]), length) >= 2),
                           message = paste(names(which(
                             sapply(split(a[, "value"], a[, "Lab"]), length) < 2
                           ))[1], "has less than 2 replicates left. Drop an ID filter if necessary.")),
                         need(
                           is.numeric(filt$precision()) &&
                             filt$precision() >= 0 &&
                             filt$precision() <= 6,
                           message = "please check precision value: should be numeric and between 0 and 6"
                         )
                       )
                       # 
                       a
                     }) 
                     # 
                     # # BOXPLOT
                     # output$overview_boxplot <- renderPlot({
                     #   #if (input$opt_show_files == "boxplot") {
                     #     TestPlot(data = dat())
                     # })
                     
                     # CertVal Plot
                     output$overview_CertValPlot <- renderPlot({
                       CertValPlot(data = dat())
                     }, height = reactive({
                       input$Fig01_height
                     }), width = reactive({
                       input$Fig01_width
                     }))
                     
                   } else {
                     updateTabsetPanel(session = session,"certificationPanel", selected = "standBy")
                   }
                   
                 }
    )
    
  })
}

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
    id = NS(id,"certificationPanel"),
    type = "hidden",
    # when nothing is loaded
    tabPanel(title = "standby-Panel",value  = "standby", "empty panel content"),
    # when something is loaded
    tabPanel(title = "active-Panel", value = "loaded",
             filterUI(NS(id,"cert_filter")),
             # CertValPlot and Export section
             fluidRow(column(10,
                             wellPanel(
                               fluidRow(
                                 column(
                                   3,
                                   fluidRow(strong("Certified Value Plot")),
                                   fluidRow(uiOutput(NS(id,"flt_labs"))),
                                   fluidRow(column(
                                     6,
                                     numericInput(
                                       inputId = NS(id,"Fig01_width"),
                                       label = "width",
                                       value = 400
                                     )
                                   ), column(
                                     6,
                                     numericInput(
                                       inputId = NS(id,"Fig01_height"),
                                       label = "height",
                                       value = 400
                                     )
                                   )),
                                   fluidRow(
                                     column(
                                       6,
                                       numericInput(
                                         inputId = NS(id,"precision2"),
                                         label = "Precision",
                                         value = 4
                                       )
                                     ),
                                     column(
                                       6,
                                       strong("Download"),
                                       br(),
                                       downloadButton(outputId = 'Fig01', label = "Figure")
                                     )
                                   ),
                                   fluidRow(column(6, strong("mean")), column(6, strong("sd"))),
                                   fluidRow(column(6, textOutput("cert_mean")), column(6, textOutput("cert_sd"))),
                                   fluidRow(
                                     checkboxInput(
                                       inputId = NS(id,"pooling"),
                                       label = "pooling",
                                       value = FALSE
                                     )
                                   )
                                 ),
                                 column(9, plotOutput(NS(id,"overview_CertValPlot"), inline = TRUE))
                               )
                             )),
                      column(
                        2,
                        wellPanel(
                          fluidRow(strong("Download Report")),
                          fluidRow(
                            radioButtons(
                              inputId = NS(id,'output_file_format'),
                              label = NULL,
                              choices = c('PDF', 'HTML', 'Word'),
                              inline = TRUE
                            )
                          ),
                          fluidRow(
                            column(6, align = "center", downloadButton('FinalReport', label = "Analyte")),
                            column(6, align = "center", downloadButton('MaterialReport', label =
                                                                         "Material"))
                          ),
                          fluidRow(
                            checkboxInput(inputId = NS(id,"show_code"), label = "Show Code in Report")
                          )
                        )
                      )),
    )    
    
  )
  
}
