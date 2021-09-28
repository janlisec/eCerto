#' @name xlsx_range_select
#' @aliases m_xlsx_range_select_UI
#' @aliases m_xlsx_range_select_Server
#'
#' @title A module to preview and select a range from a XLSX File.
#'
#' @description \code{xlsx_range_select} will provide a preview for an excel data file and allow the user to specify a range by mouse click(s).
#'
#' @details not yet
#'
#' @param id Module ID when called in a shiny app.
#' @param current_file_input Shiny fileInput referencing excel file(s).
#' @param sheet Number of the sheet to preview.
#' @param excelformat Selected sub format as reactive string.
#' @param silent Option to print or omit status messages.
#'
#' @return A reactiveValues list with \code{start_col}, \code{end_col}, \code{tab_flt}
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    shiny::fluidRow(
#'      shiny::column(
#'       3,
#'       shiny::fileInput(
#'        inputId = "x",
#'        label = "Modul parameter: x",
#'        accept = "xlsx",
#'        multiple =TRUE
#'      )),
#'      shiny::column(
#'        3,
#'        shiny::numericInput(
#'         inputId = "sheet", label = "Modul parameter: sheet", value = 1
#'        )
#'      ),
#'      shiny::column(
#'         3,
#'         shiny::selectInput(
#'          inputId = "excelformat",
#'          label = "Modul parameter: excelformat",
#'          choices = c("Certification","Homogeneity","Stability")
#'         )
#'      ),
#'      shiny::column(
#'        3,
#'        shiny::selectInput(
#'          inputId = "silent",
#'          label = "Modul parameter: silent", choices = c("TRUE","FALSE")
#'        )
#'      )
#'    ),
#'    shiny::hr(),
#'    m_xlsx_range_select_UI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'   out <- m_xlsx_range_select_Server(
#'     id = "test",
#'     current_file_input = reactive({input$x}),
#'     sheet = reactive({input$sheet}),
#'     excelformat = reactive({input$excelformat})
#'    )
#'   observeEvent(out$rng, { print(out$rng) })
#'  }
#' )
#' }
#'
#' @rdname xlsx_range_select
#' @export

m_xlsx_range_select_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(outputId = ns('uitxt')),
    DT::DTOutput(outputId = ns('uitab'))
  )
}

#' @rdname xlsx_range_select
#' @export
m_xlsx_range_select_Server <- function(id, current_file_input=NULL, sheet=NULL, excelformat=shiny::reactive({"Certification"}), silent=FALSE) {

  stopifnot(shiny::is.reactive(current_file_input))
  stopifnot(shiny::is.reactive(sheet))
  ns <- shiny::NS(id)

  shiny::moduleServer(id, function(input, output, session) {


    getRngTxt <- function(sc=1, sr=1, ec=1, er=1) {
      paste0(LETTERS[sc], sr, ":", LETTERS[ec], er)
    } #getRngTxt(tab_param$start_col, tab_param$start_row, tab_param$end_col, tab_param$end_row)

    tab <- shiny::reactive({
      shiny::req(current_file_input(), sheet())

      # use different modes of fnc_load_xlsx to import data depending on file type
      if (!silent) message("m_xlsx_range_select_Server: reactive(tab): load ", nrow(current_file_input()), " files")
      if (shiny::isolate(excelformat())=="Certification") {
        l <- lapply(current_file_input()$datapath, function(x) { fnc_load_xlsx(filepath = x, sheet = sheet(), method="tidyxl") })
        shiny::validate(
          shiny::need(all(!sapply(l, is.null)),"uploaded Excel files contain an empty one"),
          shiny::need(length(l)>=2,"less than 2 laboratory files uploaded. Upload more!")
        )
        # check if all tables have the same dimensions
        test <- length(unique(sapply(l, nrow)))==1 && length(unique(sapply(l, ncol)))==1
        if (!test) { warning("m_xlsx_range_select_Server: Certification Excel Files contain different dimensions.") }
      } else if(shiny::isolate(excelformat())=="Stability") {
        # for Stability, all sheets are loaded in Background
        l <- lapply(sheet(),function(x) {
          fnc_load_xlsx(
            filepath = current_file_input()$datapath[1],
            sheet = x,
            method="openxlsx"
            )
        })
        # TODO Tabelle nicht editierbar machen!
      } else {
        l <- list(fnc_load_xlsx(filepath = current_file_input()$datapath[1], sheet = sheet(), method="openxlsx"))
      }
      return(l)
    })

    tab_param <- shiny::reactiveValues("tab"=NULL, "start_row"=1, "end_row"=1, "start_col"=1, "end_col"=1, "tab_flt"=matrix(1), "rng"="A1:A1")
    # event: upload of excel file(s)
    shiny::observeEvent(tab(), {
      if (!silent) message("m_xlsx_range_select_Server: observeEvent(tab): table uploaded; set initial crop parameters")
      tab_param$tab <- tab()
      tab_param$tab_upload <- shiny::isolate(tab()) # unchanged table from upload (for checking if row and column was selected)
      tab_param$start_row <- 1
      tab_param$start_col <- 1
      tab_param$end_row <- nrow(tab()[[1]])
      tab_param$end_col <- ncol(tab()[[1]])
      # as user response in UI
      tab_param$rng <- getRngTxt(tab_param$start_col, tab_param$start_row, tab_param$end_col, tab_param$end_row)
    })

    # das Proxy der Tabelle, um sicherstellen zu können, dass immer nur 2 Zellen angewählt werden können (proxy version kann serverseitig manipuliert werden)
    uitab_proxy <- DT::dataTableProxy("uitab")

    # if rows and columns in the DT() have been selected
    shiny::observeEvent(input$uitab_cells_selected, {
      if (!silent) message("m_xlsx_range_select_Server: observeEvent(input$uitab_cells_selected)")
      cs <- input$uitab_cells_selected
      if (nrow(cs)>=2) {
        check_cs <- function(x, exc_fmt="Certification") {
          min_rows <- switch(exc_fmt, "Certification"=0, 1)
          min_cols <- switch(exc_fmt, "Certification"=2, 1)
          diff(range(x[,1]))>=min_rows &&
            diff(range(x[,2]))>=min_cols &&
            any(tab_param$start_col != min(x[,2]), tab_param$end_col != max(x[,2]), tab_param$start_row != min(cs[,1]), tab_param$end_row != max(cs[,1]))
        }
        check_new_point <- function(x) {
          x[3,1]>=min(x[-3,1]) & x[3,1]<=max(x[-3,1]) & x[3,2]>=min(x[-3,2]) & x[3,2]<=max(x[-3,2])
        }
        update_cs <- function() {
          tab_param$start_col = min(cs[,2])
          tab_param$end_col = max(cs[,2])
          tab_param$start_row = min(cs[,1])
          tab_param$end_row = max(cs[,1])
          if (is.list(tab())) {
            tab_param$tab <- lapply(tab(), function(x) { x[min(cs[,1]):max(cs[,1]), min(cs[,2]):max(cs[,2]), drop=FALSE]})
          } else {
            tab_param$tab <- tab()[min(cs[,1]):max(cs[,1]), min(cs[,2]):max(cs[,2]), drop=FALSE]
          }
          tab_param$rng <- getRngTxt(tab_param$start_col, tab_param$start_row, tab_param$end_col, tab_param$end_row)
          DT::selectCells(proxy = uitab_proxy, selected = matrix(c(tab_param$start_row, tab_param$end_row, tab_param$start_col, tab_param$end_col), ncol=2))
        }
        # the final row is the cell selected last by the user
        if (nrow(cs)==2 && check_cs(x=cs, exc_fmt=excelformat())) {
          update_cs()
        }
        # did the user select a third point ?
        if (nrow(cs)>2) {
          # is this third point outside or inside the current range
          if (check_new_point(x=cs)) {
            # when inside --> open a modal to inform the user that he needs to deselect another cell first
            shiny::showModal(shiny::modalDialog(
              shiny::HTML("You selected a cell within the current range.<br>Please deselect one of the two outer cells first.")
            ))
            DT::selectCells(proxy = uitab_proxy, selected = matrix(c(tab_param$start_row, tab_param$end_row, tab_param$start_col, tab_param$end_col), ncol=2))
          } else {
            # when outside --> increase selected range automatically
            if (check_cs(x=cs)) {
              update_cs()
            }
          }
          DT::selectCells(proxy = uitab_proxy, selected = matrix(c(tab_param$start_row, tab_param$end_row, tab_param$start_col, tab_param$end_col), ncol=2))
        }
      }
    })

    uitab_proxy <- DT::dataTableProxy("uitab")
    output$uitab <- DT::renderDT({
      shiny::req(tab())
      out <- tab()[[1]]
      if (prod(dim(out))>1) {
        # limit preview to 10 characters per cell
        out <- apply(out, 2, substr, start=1, stop=10)
      }
      return(out)
    },
    #options=list("dom"="start_row", pageLength=nrow(tab()[[1]]),ordering=FALSE),
    options = list("dom"="t", pageLength=nrow(tab()[[1]]),ordering=FALSE),
    # editable = ifelse(excelformat()=="Stability",FALSE,TRUE),
    selection = if(excelformat()=="Stability"){
      "none"
    } else {
      list(target="cell", selectable=matrix(-1*c(1:nrow(tab()[[1]]), rep(0,nrow(tab()[[1]]))), ncol=2))
    }
    )

    output$uitxt <- shiny::renderUI({
      shiny::req(tab())
      str1 <- ifelse(is.null(current_file_input()), "", paste("You see a preview of File:", current_file_input()$name[1]))
      if(excelformat()=="Stability") {
        str2 = ""
      } else {
        str2 <- "You may select 2 cells (top left and bottom right) by mouse click to specify a range."
      }
      #str3 <- paste("Currently selected range:", paste0(LETTERS[tab_param$start_col], tab_param$start_row, ":", LETTERS[tab_param$end_col], tab_param$end_row))
      str3 <- paste("Currently selected range:", tab_param$rng)
      shiny::HTML(paste(str1, str2, str3, sep = '<br/>'))
    })

    return(tab_param)

  })

}
