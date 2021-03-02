# FILTER MODULE----------------------------------------------------

#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
.filterUI = function(id){
  .analyteModuleUI(NS(id,"analyte"))
  # fluidRow(
  #   column(3, 
  #          tags$div(title="Select an analyte (e.g. SI for Silicon)",
  #                   selectInput(
  #                     inputId = NS(id,"sel_analyt"),
  #                     label = "analyte",
  #                     choices = NULL, #levels(d()[, "analyte"]),
  #                     selected = NULL #levels(d()[, "analyte"])[1]
  #                   )
  #                   #uiOutput(NS(id,"sel_analyt_ui"))
  #          )
  #   ),
  #   column(3, 
  #          tags$div(title="Filter samples by ID",
  #                   
  #                   selectizeInput(
  #                     inputId = NS(id,"flt_samples"),
  #                     label = "Filter Sample IDs",
  #                     choices = NULL,
  #                     selected = NULL,
  #                     multiple = TRUE
  #                   )
  #          )
  #   ),
  #   column(
  #     3,
  #     numericInput(
  #       inputId = NS(id,"precision"),
  #       label = "Precision",
  #       value = 4
  #     )
  #   ),
  # )
 
  
}

#' Title
#'
#' @param id 
#' @param d 
#'
#' @return
#' @export
#'
#' @examples
.filterServer = function(id, d) {
  stopifnot(is.reactive(d))
  moduleServer(id, function(input, output, session){
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
      
      # add possible id's of samples to list
      tmp = data_of_godelement(d())
      ids = tmp[tmp[["analyte"]] == as.list(analytes())[[i]], "ID"]
      a_param_list[[i]]$sample_ids = ids[!is.na(ids)]
    }
    a_param_list = setNames(a_param_list, analytes())
    apm = do.call("reactiveValues", a_param_list)
    .analyteModuleServer("analyte", apm)
    
    # # ID Filter
    # observeEvent(input$sel_analyt,{
    #   tmp = data_of_godelement(d())
    #   choices = tmp[tmp[["analyte"]] == input$sel_analyt, "ID"]
    #   choices = choices[!is.na(choices)]
    #   selected = choices[which(tmp[tmp[["analyte"]] == input$sel_analyt, "S_flt"])]
    #   updateSelectizeInput(
    #     inputId = "flt_samples",
    #     label = "Filter Sample IDs",
    #     choices = choices,
    #     selected = selected
    #   )
    # }, ignoreInit = TRUE)
    
    
    # list(
    #   analyte = reactive({input$sel_analyt}),
    #   id_filt = reactive({input$flt_samples}),
    #   precision = reactive({input$precision})
    # )
    
    
    # precision:
    # n <- input$precision
    # return(data.frame(
    #   data[, 1, drop = F],
    #   round(data[, -1, drop = F], digits = n),
    #   "mean" = round(apply(data[, -1, drop = F], 1, mean, na.rm = T), digits = n),
    #   "sd" = round(apply(data[, -1, drop = F], 1, sd, na.rm = T), digits = n)
    # ))
    
  })
}
# ANALYTE MODULE --------
.analyteModuleUI <- function(id) {
  wellPanel(
    tabsetPanel(id = NS(id,"tabs")))
}

.analyteModuleServer <- function(id, analytelist){
  moduleServer(id, function(input, output, session) {
    analytes = isolate(reactiveValuesToList(analytelist))
    for (a in analytes) {
      a.name = a$analytename
      # selected = a$sample_filter # initially selected value 
      prependTab(
        inputId = "tabs", select = TRUE,
        tabPanel(title = a.name, 
          fluidRow(
            column(6,
                   tags$div(
                     title="Filter samples by ID",
                     selectizeInput(
                       inputId = NS(id,"flt_samples"),
                       label = "Filter Sample IDs",
                       choices = a$sample_ids,
                       selected = a$sample_filter,
                       multiple = TRUE
                     )
                   )
            ),
            column(6,
              numericInput(
                inputId = NS(id,"precision"),
                label = "Precision",
                value = 4
              )
            ),
          )
        ),
      )
    }
    
    # # ID Filter
    observeEvent(input$sel_analyt,{
      #selected = choices[which(tmp[tmp[["analyte"]] == input$sel_analyt, "S_flt"])]
      selected = analytes[analytes$analytename == input$tabs]$sample_filter # initially selected value 
      updateSelectizeInput(
        inputId = "flt_samples",
        label = "Filter Sample IDs",
        choices = choices,
        # selected = selected
      )
    }, ignoreInit = TRUE)
  }) 
}