#' @title Data View Module
#' @name mod_DataView
#'
#' @param id Id when called in module.
#' @param dataset_flt Filtered data for particular measurements specified by user.
#' @param precision Precision to round the input values.
#'
#' @return kompakt data representation
#' @export
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(m_DataViewUI(id = "test")),
#'  server = function(input, output, session) {
#'    tmp <- reactiveVal(cbind(
#'      eCerto::test_certification()[["data"]],
#'      data.frame("File"="")
#'    ))
#'    m_DataViewServer(id = "test", dataset_flt = tmp)
#'  }
#' )
#' }
#'

m_DataViewUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 10,
        DT::dataTableOutput(ns("flt_Input_Data"))
      ),
      shiny::column(
        width = 2,
        shiny::wellPanel(
          shiny::selectInput(
            width = "200px",
            inputId = ns("data_view_select"), # previously opt_show_files
            label = "Data view",
            choices = c("kompakt", "standard"),
            selected = "none"
          )
        )
      )
    )
  )
}

#' @rdname mod_DataView
#' @export
m_DataViewServer <- function(id, dataset_flt, precision = shiny::reactiveVal(3)) {

  shiny::moduleServer(id, function(input, output, session) {
    # Generate an HTML table view of filtered single analyt data
    output$flt_Input_Data <- DT::renderDataTable({
      if (input$data_view_select == "kompakt") {
        #browser()
        dt <- DT::datatable(
          data = dataset_komp(),
          options = list(paging = FALSE, searching = FALSE), rownames = NULL
        )
        # round with input precision
        dt <- DT::formatCurrency(table = dt, columns = 2:(ncol(dataset_komp())-2), currency = "", digits = precision())
        # round with output precision (JL: currently the same; adjust and remove comment if requested by users)
        dt <- DT::formatCurrency(table = dt, columns = (ncol(dataset_komp())-1):ncol(dataset_komp()), currency = "", digits = precision())
      }
      if (input$data_view_select == "standard") {
        dt <- DT::datatable(
          data = dataset_flt()[, c("ID", "Lab", "value", "unit", "replicate", "File")],
          options = list(paging = FALSE, searching = FALSE), rownames = NULL
        )
        dt <- DT::formatCurrency(table = dt, columns = 3, currency = "", digits = precision())
      }
      return(dt)
    })

    # prepare a compact version of the data table
    dataset_komp <- shiny::reactive({
      shiny::req(dataset_flt())
      data <- dataset_flt()
      n_reps <- sort(unique(data$replicate))
      data <- plyr::ldply(split(data, data$Lab), function(x) {
        out <- rep(NA, length(n_reps))
        out[x$replicate] <- x$value
        matrix(out,
               ncol = length(n_reps),
               dimnames = list(NULL, paste0("R", n_reps)))
      }, .id = "Lab")
      n <- precision()
      return(data.frame(
        data[, 1, drop = F],
        round(data[, -1, drop = F], digits = n),
        "mean" = round(apply(data[, -1, drop = F], 1, mean, na.rm = T), digits = n),
        "sd" = round(apply(data[, -1, drop = F], 1, stats::sd, na.rm = T), digits = n)
      ))
    })
    return(dataset_komp)
  })
}
