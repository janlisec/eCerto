# FILTER MODULE UI ----------------------------------------------------

#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
filterUI = function(id){
  
  fluidRow(
    column(3, 
           tags$div(title="Select an analyte (e.g. SI for Silicon)",
                    selectInput(
                      inputId = NS(id,"sel_analyt"),
                      label = "analyte",
                      choices = NULL, #levels(d()[, "analyte"]),
                      selected = NULL #levels(d()[, "analyte"])[1]
                    )
                    #uiOutput(NS(id,"sel_analyt_ui"))
           )
    ),
    column(3, 
           tags$div(title="Filter samples by ID",
                    
                    selectizeInput(
                      inputId = NS(id,"flt_samples"),
                      label = "Filter Sample IDs",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE
                    )
                    #uiOutput(NS(id,"flt_samples"))
           )
    ),
    column(
      3,
      numericInput(
        inputId = NS(id,"precision"),
        label = "Precision",
        value = 4
      )
    ),
  )
  # column(
  #   3,
  #   selectInput(
  #     inputId = "opt_show_files",
  #     label = "Data view",
  #     choices = c("none", "kompakt", "standard", "boxplot"),
  #     selected = "none"
  #   )
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
filterServer = function(id, d) {
  stopifnot(is.reactive(d))
  moduleServer(id, function(input, output, session){
    
    observeEvent(d(),{
      updateSelectInput(
        inputId ="sel_analyt",
        label = "analyte",
        choices = levels(data_of_godelement(d())[, "analyte"]),
        selected = levels(data_of_godelement(d())[, "analyte"])[1]
      )
    })
    
    observeEvent(input$sel_analyt,{

      tmp = data_of_godelement(d())
      choices <- tmp[tmp[, "analyte"] == input$sel_analyt, "ID"]
      selected <-
        choices[which(tmp[tmp[, "analyte"] == input$sel_analyt, "S_flt"])]
      updateSelectizeInput(
        inputId = "flt_samples",
        label = "Filter Sample IDs",
        choices = choices,
        selected = selected
      )
      #})
    }, ignoreInit = TRUE)
    
    reactive({
      list(
        analyte = reactive({input$sel_analyt}),
        id = reactive({input$flt_samples})
      )
    })
    
    
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