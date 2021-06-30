#' @name xlsx_range_select
#' @aliases xlsx_range_select_UI
#' @aliases xlsx_range_select_Server
#'
#' @title A module to preview and select a range from a XLSX File.
#'
#' @description \code{xlsx_range_select} will provide a preview for an excel data file and allow the user to specify a range by mouse click(s).
#'
#' @details not yet
#'
#' @param id Module ID when called in a shiny app.
#' @param x Shiny fileInput referencing excel file(s).
#' @param sheet Number of the sheet to preview.
#' @param excelformat Selected sub format as reactive string.
#' @param silent Option to print or omit status messages.
#'
#' @return A reactiveValues list.
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
#'          choices = c("Certifications","Homogeneity","Stability")
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
#'    xlsx_range_select_UI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'   out <- xlsx_range_select_Server(
#'     id = "test",
#'     x = reactive({input$x}),
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

xlsx_range_select_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(outputId = ns('uitxt')),
    DT::DTOutput(outputId = ns('uitab'))
  )
}

#' @rdname xlsx_range_select
#' @export
xlsx_range_select_Server <- function(id, x=NULL, sheet=NULL, excelformat=shiny::reactive({"Certifications"}), silent=TRUE) {

  stopifnot(shiny::is.reactive(x))
  stopifnot(shiny::is.reactive(sheet))
  ns <- shiny::NS(id)

  shiny::moduleServer(id, function(input, output, session) {

    
    getRngTxt <- function(sc=1, sr=1, ec=1, er=1) {
      paste0(LETTERS[sc], sr, ":", LETTERS[ec], er)
    } #getRngTxt(tab_param$start_col, tab_param$start_row, tab_param$end_col, tab_param$end_row)

    tab <- shiny::reactive({
      shiny::req(x(), sheet())
      # use different modes of fnc_load_xlsx to import data depending on file type
      if (!silent) message("xlsx_range_select_Server: reactive(tab): load ", nrow(x()), " files")
      # @Frederik: gibt es einen Grund 'excelformat' als reactive zu übergeben,
      #  wenn wir es intern nur als Konstante nutzen (isolate)?
      # @Jan (23. Juni): Hier ist die Reaktivität tatsächlich nicht notwendig,
      #   aber wird es vielleicht mal wenn mehr Sonderregeln für Homg und Stab
      #   dazukommen
      if (shiny::isolate(excelformat())=="Certifications") {
        l <- lapply(x()$datapath, function(x) { ecerto::fnc_load_xlsx(filepath = x, sheet = sheet(), method="tidyxl") })
        shiny::validate(
          shiny::need(all(!sapply(l, is.null)),"uploaded Excel contain an empty one"),
          shiny::need(length(l)>=2,"less than 2 laboratory files uploaded. Upload more!")
        )
        # check if all tables have the same dimensions
        test <- length(unique(sapply(l, nrow)))==1 && length(unique(sapply(l, ncol)))==1
        if (!test) { warning("xlsx_range_select_Server: Certification Excel Files contain different dimensions.") }
      } else {
        l <- list(ecerto::fnc_load_xlsx(filepath = x()$datapath[1], sheet = sheet(), method="openxlsx"))
      }
      return(l)
    })

    tab_param <- shiny::reactiveValues("tab"=NULL, "start_row"=1, "end_row"=1, "start_col"=1, "end_col"=1, "tab_flt"=matrix(1), "rng"="A1:A1")
    # event: upload of excel file(s)
    shiny::observeEvent(tab(), {
      if (!silent) 
        message("xlsx_range_select_Server: observeEvent(tab): table uploaded; set initial crop parameters")
      tab_param$tab <- tab()
      tab_param$start_row <- 1
      tab_param$start_col <- 1
      tab_param$end_row <- nrow(tab()[[1]])
      tab_param$end_col <- ncol(tab()[[1]])
      tab_param$rng <-
        getRngTxt(tab_param$start_col,
                  tab_param$start_row,
                  tab_param$end_col,
                  tab_param$end_row)
    })

    # @Frederick: Wenn Du Änderungen vornimmst, versuche zu überlegen, ob diese wirklich an die von Dir gewählte Stelle gehören oder
    # besser woanders hin sollten. Dieses Modul ist eigentlich nur dazu da ein Excel-File zu laden und den Nutzer einen Bereich auswählen zu lassen
    # ich hätte den Dateinamen daher als Spalte erst außerhalb des Moduls angehängt (mod_ExcelUploadControl?), oder übersehe ich etwas? (Kann aber so bleiben, nur ein Kommentar)
    # @Jan Einverstanden (23. Juni)
    shiny::observeEvent(tab_param$tab,{
      if (!silent) message("xlsx_range_select_Server: observeEvent(tab_param$tab): add File column")
      if(!is.null(unlist(tab_param$tab))){
        tab_param$tab_flt = tab_param$tab
        for (i in 1:length(tab_param$tab_flt)) {
          tab_param$tab_flt[[i]][["File"]] = rep(x()$name[i], nrow(tab_param$tab_flt[[i]]))
        }
      }
    })

    # das Proxy der Tabelle, um sicherstellen zu können, dass immer nur 2 Zellen angewählt werden können (proxy version kann serverseitig manipuliert werden)
    uitab_proxy <- DT::dataTableProxy("uitab")

    # if rows and columns in the DT() have been selected
    shiny::observeEvent(input$uitab_cells_selected, {
      if (!silent) message("xlsx_range_select_Server: observeEvent(input$uitab_cells_selected)")
      cs <- input$uitab_cells_selected
      check_cs <- function(x) {
        diff(range(x[,1]))>=1 && diff(range(x[,2]))>=1 && any(tab_param$start_col != min(x[,2]), tab_param$end_col != max(x[,2]), tab_param$start_row != min(cs[,1]), tab_param$end_row != max(cs[,1]))
      }
      check_new_point <- function(x) {
        x[3,1]>=min(x[-3,1]) & x[3,1]<=max(x[-3,1]) & x[3,2]>=min(x[-3,2]) & x[3,2]<=max(x[-3,2])
      }
      update_cs <- function() {
        tab_param$start_col = min(cs[,2])
        tab_param$end_col = max(cs[,2])
        tab_param$start_row = min(cs[,1])
        tab_param$end_row = max(cs[,1])
        tab_param$tab = crop_dataframes(
          # @Frederik: hier tab_param$tab zu nehmen hat zu Fehlern geführt, wenn
          # man als user mehrere Zellen an und wieder abgewählt hat
          # @Jan (23. Juni) Generell eine kritische Stelle...
          dfs = tab(),
          rows = as.numeric(tab_param$start_row):as.numeric(tab_param$end_row),
          cols = as.numeric(tab_param$start_col):as.numeric(tab_param$end_col)
        )
        tab_param$rng <- getRngTxt(tab_param$start_col, tab_param$start_row, tab_param$end_col, tab_param$end_row)
        DT::selectCells(proxy = uitab_proxy, selected = matrix(c(tab_param$start_row, tab_param$end_row, tab_param$start_col, tab_param$end_col), ncol=2))
      }
      # the final row is the cell selected last by the user
      if (nrow(cs)==2 && check_cs(x=cs)) {
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
      # the final row is the cell selected last by the user
      if (nrow(cs)==2 && check_cs(x=cs)) {
        update_cs()
      }
      # did the user select a third point
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
      }
    })

    shiny::observeEvent(tab_param$tab,{
      if (!silent) message("xlsx_range_select_Server: observeEvent(tab_param$tab): add File column")
      if(!is.null(unlist(tab_param$tab))){
        tab_param$tab_flt = tab_param$tab
        for (i in 1:length(tab_param$tab_flt)) {
          tab_param$tab_flt[[i]][["File"]] = rep(x()$name[i], nrow(tab_param$tab_flt[[i]]))
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
      selection = list(target="cell", selectable=matrix(-1*c(1:nrow(tab()[[1]]), rep(0,nrow(tab()[[1]]))), ncol=2))
    )

    output$uitxt <- shiny::renderUI({
      shiny::req(tab())
      str1 <- ifelse(is.null(x()), "", paste("You see a preview of File:", x()$name[1]))
      str2 <- "You may select 2 cells (top left and bottom right) by mouse click to specify a range."
      #str3 <- paste("Currently selected range:", paste0(LETTERS[tab_param$start_col], tab_param$start_row, ":", LETTERS[tab_param$end_col], tab_param$end_row))
      str3 <- paste("Currently selected range:", tab_param$rng)
      shiny::HTML(paste(str1, str2, str3, sep = '<br/>'))
    })

    return(tab_param)

  })

}
