#' @title Data View Module
#' @name mod_DataView
#'
#' @param id Id when called in module.
#' @param dat filtered data for particular measurements specified by user
#' @param current_apm current analyte-parameter-list for selected analyte
#'
#' @return Will return UI and Server logic for the stability tab.
#' @export
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_DataViewUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'  datasetflt = reactiveVal(structure(
#'  list(
#'    ID = c(1L,10L,18,25), Lab = factor(c("L1","L1","L2","L2")), analyte = rep("Si",4), replicate = c(1,2,1,2),
#'     value = runif(4, 0, 1),unit = rep("0.05",4),File = rep("Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx",4),
#'     S_flt = rep(FALSE,4),L_flt = rep(FALSE, 4),row.names = c(1L,10L,18,25),class = "data.frame")))
#'     
#'    m_DataViewServer(
#'      id = "test",
#'      dataset_flt = datasetflt,
#'      current_apm  = reactiveVal(list(precision = 3))
#'    )
#'  }
#' )
#' }
#'

m_DataViewUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::wellPanel(
      shiny::selectInput(
        width = "200px",
        inputId = ns("data_view_select"), # previously opt_show_files
        label = "Data view",
        choices = c("kompakt", "standard"),
        selected = "none"
      ),
      DT::dataTableOutput(ns("flt_Input_Data"))
    )
  )
}

#' @rdname mod_DataView
#' @export
m_DataViewServer <- function(id, dataset_flt, current_apm) {
  
  shiny::moduleServer(id, function(input, output, session) {
    # Generate an HTML table view of filtered single analyt data
    output$flt_Input_Data <- DT::renderDataTable({
      if (input$data_view_select == "kompakt") {
        return(dataset_komp())
      }
      if (input$data_view_select == "standard") {
        return(dataset_flt()[, c("ID", "Lab", "value", "unit", "replicate", "File", "L_flt")])
      } else {
        return()
      }
    }, options = list(paging = FALSE, searching = FALSE), rownames = NULL)
    
    
    
    
    # prepare a compact version of the data table
    dataset_komp <- reactive({
      req(dataset_flt())
      browser()
      data <- dataset_flt()
      n_reps <- sort(unique(data$replicate))
      data <- plyr::ldply(split(data, data$Lab), function(x) {
        out <- rep(NA, length(n_reps))
        out[x$replicate] <- x$value
        matrix(out,
               ncol = length(n_reps),
               dimnames = list(NULL, paste0("R", n_reps)))
      }, .id = "Lab")
      n <- current_apm()$precision
      return(data.frame(
        data[, 1, drop = F],
        round(data[, -1, drop = F], digits = n),
        "mean" = round(apply(data[, -1, drop = F], 1, mean, na.rm = T), digits = n),
        "sd" = round(apply(data[, -1, drop = F], 1, sd, na.rm = T), digits = n)
      ))
    })
    
  })
}
