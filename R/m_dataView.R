#' @title Data View Module
#' @name mod_DataView
#'
#'@param id Name when called as a module in a shiny app.
#'@param rv eCerto R6 object, which includes a 'materialtabelle'.
#'
#'@return Nothing. Will show the imported data for one analyte from an eCerto R6 object.
#'@noRd
#'@keywords internal
#'
#'@examples
#'if (interactive()) {
#'shiny::shinyApp(
#'  ui = shiny::fluidPage(m_DataViewUI(id = "test")),
#'  server = function(input, output, session) {
#'    rv <- eCerto:::test_rv()
#'    # set S_flt and L_flt for testing
#'    shiny::isolate(apm <- getValue(rv, c("General","apm")))
#'    apm[[rv$c_analyte]][["sample_filter"]] <- 4
#'    apm[[rv$c_analyte]][["lab_filter"]] <- "L1"
#'    shiny::isolate(setValue(rv, c("General","apm"), apm))
#'    gargoyle::init("update_c_analyte")
#'    m_DataViewServer(id = "test", rv = rv)
#'  }
#')
#'}
#'

m_DataViewUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 10,
        DT::dataTableOutput(ns("tab1"))
      ),
      shiny::column(
        width = 2,
        shiny::wellPanel(
          shiny::selectInput(
            width = "200px",
            inputId = ns("data_view_select"), # previously opt_show_files
            label = "Data view",
            choices = c("kompakt", "standard")
          )
        )
      )
    )
  )
}

#'@noRd
#'@keywords internal
m_DataViewServer <- function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    # prepare a analyte specific (filtered) version of the input data table
    dataset_flt <- shiny::reactive({
      gargoyle::watch("update_c_analyte")
      df <- getValue(rv, c("Certification","data"))
      apm <- getValue(rv, c("General","apm"))
      an <- rv$c_analyte
      df <- df[df[,"analyte"]==an,]
      if (!"File" %in% colnames(df)) df <- cbind(df, "File"="")
      #df[df[,"S_flt"] %in% apm[[an]][["sample_filter"]],"S_flt"]
      return(df)
    })

    # prepare a compact version of the data table
    dataset_komp <- shiny::reactive({
      shiny::req(dataset_flt())
      df <- dataset_flt()
      p <- getValue(rv, c("General","apm"))[[rv$c_analyte]][["precision"]]
      n_reps <- sort(unique(df$replicate))
      data <- plyr::ldply(split(df, df$Lab), function(x) {
        out <- rep(NA, length(n_reps))
        out[x$replicate] <- x$value
        matrix(out, ncol = length(n_reps), dimnames = list(NULL, paste0("R", n_reps)))
      }, .id = "Lab")
      id_idx <- plyr::ldply(split(df, df$Lab), function(x) {
        out <- rep(NA, length(n_reps))
        out[x$replicate] <- x$ID
        matrix(out, ncol = length(n_reps), dimnames = list(NULL, paste0("R", n_reps)))
      }, .id = "Lab")
      df <- data.frame(
        data[, 1, drop = F],
        round(data[, -1, drop = F], digits = p),
        "mean" = round(apply(data[, -1, drop = F], 1, mean, na.rm = T), digits = p),
        "sd" = round(apply(data[, -1, drop = F], 1, stats::sd, na.rm = T), digits = p)
      )
      attr(df, "id_idx") <- id_idx
      return(df)
    })

    # Generate an HTML table view of filtered single analyt data
    output$tab1 <- DT::renderDataTable({
      type <- input$data_view_select
      if (type == "kompakt") x <- dataset_komp() else x <- dataset_flt()[, c("ID", "Lab", "value", "unit", "replicate", "File")]
      styleTabC0(x = x, ap=getValue(rv, c("General","apm"))[[rv$c_analyte]], type=type)
    })

  })
}
