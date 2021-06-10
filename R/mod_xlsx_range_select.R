#'@title xlsx_range_select.
#'
#'@description \code{xlsx_range_select} will provide a preview for an excel data
#'file and allow the user to specify a range by mouse click.
#'
#'@details not yet
#'
#'@param id Module ID when called in a shiny app.
#'@param x Shiny inputFile referencing excel file(s).
#'@param sheet Number of the sheet to preview.
#'@param silent Option to print or omit status messages.
#'
#'@return A reactiveValues list.
#'
#'@export
#'

xlsx_range_select_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::uiOutput(outputId = ns('uitxt')),
    DT::DTOutput(outputId = ns('uitab'))
  )
}

#' @export
xlsx_range_select_Server <- function(id, x=NULL, sheet=NULL, excelformat=shiny::reactive({"Certifications"}), silent=FALSE) {
  stopifnot(is.reactive(x))
  stopifnot(is.reactive(sheet))
  ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    tab <- shiny::reactive({
      req(x(),sheet())
      # validate(
      #   need(x(), message = FALSE),
      #   need(sheet(), message = FALSE),
      #   need(all(tools::file_ext(x()$datapath) == "xlsx"), message = "Please upload Excel only")
      # )
      # use different modes of fnc_load_xlsx to import data depending on file type
      message("xlsx_range_select_Server: reactive(tab): load files")
      if (isolate(excelformat())=="Certifications") {
        l <- lapply(x()$datapath, function(x) { fnc_load_xlsx(filepath = x, sheet = sheet(), method="tidyxl") })
         validate(
          need(all(!sapply(l, is.null)),"uploaded Excel contain an empty one"),
          need(length(l)>=2,"less than 2 laboratory files uploaded. Upload more!")
          )
        # validate(
        #   # Check if no empty dataframe
        #   need(all(!unlist(lapply(l, is.null))), "Excel file must not be empty")
        # )
        # check if all tables have the same dimensions
        test <- length(unique(sapply(l, nrow)))==1 && length(unique(sapply(l, ncol)))==1
        if (!test) { warning("xlsx_range_select_Server: Certification Excel Files contain different dimensions.") }
      } else {
        l <- list(fnc_load_xlsx(filepath = x()$datapath[1], sheet = sheet(), method="openxlsx"))
      }

      return(l)
    })
    
    tab_param <- shiny::reactiveValues("tab"=NULL, "start_row"=1, "end_row"=1, "start_col"=1, "end_col"=1, "tab_flt"=matrix(1))
    # after upload of excel file(s)
    shiny::observeEvent(tab(), {
      tab_param$tab <- tab()
      tab_param$start_row <- 1
      tab_param$start_col <- 1
      tab_param$end_row <- nrow(tab()[[1]])
      tab_param$end_col <- ncol(tab()[[1]])
    })
    
    # if rows and columns in the DT() have been selected
    shiny::observeEvent(input$uitab_cells_selected, {
      cs <- input$uitab_cells_selected
      # test1 <- prod(dim(cs))>2 && diff(range(cs[,1]))>=1 && diff(range(cs[,2]))>=1
      # test2 <- any(tab_param$start_col != min(cs[,2]), tab_param$end_col != max(cs[,2]), tab_param$start_row != min(cs[,1]), tab_param$end_row != max(cs[,1]))
      if ( prod(dim(cs))>2 && diff(range(cs[,1]))>=1 && diff(range(cs[,2]))>=1 && any(tab_param$start_col != min(cs[,2]), tab_param$end_col != max(cs[,2]), tab_param$start_row != min(cs[,1]), tab_param$end_row != max(cs[,1]))){
        tab_param$start_col = min(cs[,2])
        tab_param$end_col = max(cs[,2])
        tab_param$start_row = min(cs[,1])
        tab_param$end_row = max(cs[,1])
        message("xlsx_range_select_Server: observeEvent(input$uitab_cells_selected): crop dataframe(s)")
        tab_param$tab = crop_dataframes(
          dfs = tab_param$tab,
          rows = as.numeric(tab_param$start_row):as.numeric(tab_param$end_row),
          cols = as.numeric(tab_param$start_col):as.numeric(tab_param$end_col)
        )
      }
    })

    
    shiny::observeEvent(tab_param$tab,{
      if(!is.null(unlist(tab_param$tab))){
        tab_param$tab_flt = tab_param$tab
        message("xlsx_range_select_Server: observeEvent(tab_param$tab): add File column")
        for (i in 1:length(tab_param$tab_flt)) {
          tab_param$tab_flt[[i]][["File"]] = rep(x()$name[i], nrow(tab_param$tab_flt[[i]]))
        }
      }
    })


    # output$uitab <- DT::renderDT({
    #   shiny::req(tab())
    #   out <- tab()[[1]]
    #   if (prod(dim(out))>1) {
    #     # limit preview to 10 characters per cell
    #     out <- apply(out, 2, substr, start=1, stop=10)
    #   }
    #   return(out)
    # }, options=list("dom"="t", pageLength=nrow(tab()[[1]]), ordering=FALSE),
    # selection=list(
    #   target="cell",
    #   selected=matrix(c(tab_param$start_row, tab_param$end_row, tab_param$start_col, tab_param$end_col), ncol=2),
    #   selectable=matrix(-1*c(1:nrow(tab()[[1]]), rep(0,nrow(tab()[[1]]))), ncol=2)
    # )
    # )
    output$uitab <- DT::renderDT({
      req(tab())
      out <- tab()[[1]]
      if (prod(dim(out))>1) {
        # limit preview to 10 characters per cell
        out <- apply(out, 2, substr, start=1, stop=10)
      }
      return(out)
    },
    options=list("dom"="start_row", pageLength=nrow(tab()[[1]]),ordering=FALSE),
    selection=list(
      target="cell",
      selectable=matrix(-1*c(1:nrow(tab()[[1]]), rep(0,nrow(tab()[[1]]))), ncol=2)
    )
    )


    output$uitxt <- shiny::renderUI({
      shiny::req(tab())
      str1 <- ifelse(is.null(x()), "", paste("You see a preview of File:", x()$name[1]))
      str2 <- "You may select 2 cells (top left and bottom right) by mouse click to specify a range."
      str3 <- paste("Currently selected range:", paste0(LETTERS[tab_param$start_col], tab_param$start_row, ":", LETTERS[tab_param$end_col], tab_param$end_row))
      shiny::HTML(paste(str1, str2, str3, sep = '<br/>'))
    })

    return(tab_param)

  })
}