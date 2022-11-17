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

    precision <- shiny::reactiveVal(4)
    # observeEvent(getValue(rv, c("General","apm")) {
    #
    # })
    dataset_flt <- shiny::reactive({
      #browser()
      gargoyle::watch("update_c_analyte")
      df <- getValue(rv, c("Certification","data"))
      apm <- getValue(rv, c("General","apm"))
      an <- rv$c_analyte
      precision(apm[[an]][["precision"]])
      df <- df[df[,"analyte"]==an,]
      if (!"File" %in% colnames(df)) df <- cbind(df, "File"="")
      #df[df[,"S_flt"] %in% apm[[an]][["sample_filter"]],"S_flt"]
      return(df)
    })
    # Generate an HTML table view of filtered single analyt data
    output$tab1 <- DT::renderDataTable({
      apm <- getValue(rv, c("General","apm"))[[rv$c_analyte]]
      if (input$data_view_select == "kompakt") {
        dt <- DT::datatable(
          data = dataset_komp(),
          options = list(
            dom = "t", paging = FALSE, searching = FALSE
          ),
          rownames = NULL,
        )
        idx <- attr(dataset_komp(), "id_idx")
        if (!is.null(apm[["sample_filter"]])) {
          for (s_idx in apm[["sample_filter"]]) {
            coln <- colnames(idx)[ceiling(which(idx==s_idx)/nrow(idx))]
            cval <- unlist(dataset_komp())[which(idx==s_idx)]
            dt <- DT::formatStyle(
              table = dt, columns = coln,
              color = DT::styleEqual(levels = cval, values = "red"),
              fontWeight = DT::styleEqual(levels = cval, values = "bold")
            )
          }
        }
        if (!is.null(apm[["lab_filter"]])) {
          for (l_idx in apm[["lab_filter"]]) {
            rown <- which(idx[,"Lab"]==apm[["lab_filter"]])
            dt <- DT::formatStyle(
              table = dt, target = 'row', columns = 2:ncol(idx), rows = rown,
              color = DT::styleRow(rows = rown, values = "red"),
              fontWeight = DT::styleRow(rows = rown, values = "bold")
            )
          }
        }
        # round with input precision
        dt <- DT::formatCurrency(table = dt, columns = 2:(ncol(dataset_komp())-2), currency = "", digits = precision())
        # round with output precision (JL: currently the same; adjust and remove comment if requested by users)
        dt <- DT::formatCurrency(table = dt, columns = (ncol(dataset_komp())-1):ncol(dataset_komp()), currency = "", digits = precision())
      }
      if (input$data_view_select == "standard") {
        dt <- DT::datatable(
          data = dataset_flt()[, c("ID", "Lab", "value", "unit", "replicate", "File")],
          options = list(
            dom = "t", paging = FALSE, searching = FALSE,
            autoWidth = TRUE, scrollY = "250px", pageLength = -1
          ), rownames = NULL
        )
        dt <- DT::formatCurrency(table = dt, columns = 3, currency = "", digits = precision())
        if (!is.null(apm[["sample_filter"]])) {
          dt <- DT::formatStyle(
            table = dt, columns = "ID",
            color = DT::styleEqual(levels = apm[["sample_filter"]], values = "red"),
            fontWeight = DT::styleEqual(levels = apm[["sample_filter"]], values = "bold")
          )
        }
        if (!is.null(apm[["lab_filter"]])) {
          dt <- DT::formatStyle(
            table = dt, columns = "Lab",
            color = DT::styleEqual(levels = apm[["lab_filter"]], values = "red"),
            fontWeight = DT::styleEqual(levels = apm[["lab_filter"]], values = "bold")
          )
        }
      }
      return(dt)
    })

    # prepare a compact version of the data table
    dataset_komp <- shiny::reactive({
      shiny::req(dataset_flt())
      df <- dataset_flt()
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
        round(data[, -1, drop = F], digits = precision()),
        "mean" = round(apply(data[, -1, drop = F], 1, mean, na.rm = T), digits = precision()),
        "sd" = round(apply(data[, -1, drop = F], 1, stats::sd, na.rm = T), digits = precision())
      )
      attr(df, "id_idx") <- id_idx
      return(df)
    })

  })
}
