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

xlsx_range_select_Server <- function(id, x=NULL, sheet=NULL, excelformat=shiny::reactive({"Certifications"}), silent=FALSE) {
  
  ns <- shiny::NS(id)
  
  shiny::moduleServer(id, function(input, output, session) {
    
    tab <- shiny::reactive({
      validate(
        need(x(), message = FALSE),
        need(sheet(), message = FALSE),
        need(all(tools::file_ext(x()$datapath) == "xlsx"), message = "Please upload Excel only")
      )
      # use different modes of fnc_load_xlsx to import data depending on file type
      message("xlsx_range_select_Server: reactive(tab): load files")
      if (isolate(excelformat())=="Certifications") {
        l <- lapply(x()$datapath, function(x) { fnc_load_xlsx(filepath = x, sheet = sheet(), method="tidyxl") })
        validate(need(length(l)>=2,"less than 2 laboratory files uploaded. Upload more!"))
        validate(
          # Check if no empty dataframe
          need(all(!unlist(lapply(l, is.null))), "Excel file must not be empty")
        )
        # check if all tables have the same dimensions
        test <- length(unique(sapply(l, nrow)))==1 && length(unique(sapply(l, ncol)))==1
        if (!test) { warning("xlsx_range_select_Server: Certification Excel Files contain different dimensions.") }
      } else {
        l <- list(fnc_load_xlsx(filepath = x()$datapath[1], sheet = sheet(), method="openxlsx"))
      }
      
      return(l)
    })
    
    
    rv <- shiny::reactiveValues("tab"=matrix(1), "start_row"=1, "end_row"=1, "start_col"=1, "end_col"=1, "tab_flt"=matrix(1))
    
    # after upload of excel file(s)
    shiny::observeEvent(tab(), {
      rv$tab <- tab()
      rv$start_row <- 1
      rv$start_col <- 1
      rv$end_row <- nrow(tab()[[1]])
      rv$end_col <- ncol(tab()[[1]])
      # rv$tab_flt <- rv$tab
    })
    
    # if rows and columns in the DT() have been selected
    shiny::observeEvent(input$uitab_cells_selected, {
      cs <- input$uitab_cells_selected
      test1 <- prod(dim(cs))>2 && diff(range(cs[,1]))>=1 && diff(range(cs[,2]))>=1
      test2 <- any(rv$start_col != min(cs[,2]), rv$end_col != max(cs[,2]), rv$start_row != min(cs[,1]), rv$end_row != max(cs[,1]))
      if (test1 & test2) {
        rv$start_col = min(cs[,2])
        rv$end_col = max(cs[,2])
        rv$start_row = min(cs[,1])
        rv$end_row = max(cs[,1])
          message("xlsx_range_select_Server: observeEvent(input$uitab_cells_selected): crop dataframe(s)")
          rv$tab = crop_dataframes(
            dfs = rv$tab,
            rows = as.numeric(rv$start_row):as.numeric(rv$end_row),
            cols = as.numeric(rv$start_col):as.numeric(rv$end_col)
          )
        #   message("xlsx_range_select_Server: observeEvent(input$uitab_cells_selected): add File column")
        #   for (i in 1:length(rv$tab_flt)) {
        #     rv$tab_flt[[i]][["File"]] = rep(x()$name[i], nrow(rv$tab_flt[[i]]))
        #   }
        # }
      }
    })

    shiny::observeEvent(rv$tab,{
      rv$tab_flt = rv$tab
      for (i in 1:length(rv$tab_flt)) {
        rv$tab_flt[[i]][["File"]] = rep(x()$name[i], nrow(rv$tab_flt[[i]]))
      }
    })
    
    
    output$uitab <- DT::renderDT({
      shiny::req(tab())
      out <- rv$tab[[1]]
      if (prod(dim(out))>1) {
        # limit preview to 10 characters per cell
        out <- apply(out, 2, substr, start=1, stop=10)
      }
      return(out)
    }, options=list("dom"="t", pageLength=nrow(rv$tab[[1]]), ordering=FALSE),
    selection=list(
      target="cell",
      selected=matrix(c(rv$start_row, rv$end_row, rv$start_col, rv$end_col), ncol=2),
      selectable=matrix(-1*c(1:nrow(rv$tab[[1]]), rep(0,nrow(rv$tab[[1]]))), ncol=2)
    )
    )
    
    output$uitxt <- shiny::renderUI({
      shiny::req(tab())
      str1 <- ifelse(is.null(x()), "", paste("You see a preview of File:", x()$name[1]))
      str2 <- "You may select 2 cells (top left and bottom right) by mouse click to specify a range."
      str3 <- paste("Currently selected range:", paste0(LETTERS[rv$start_col], rv$start_row, ":", LETTERS[rv$end_col], rv$end_row))
      shiny::HTML(paste(str1, str2, str3, sep = '<br/>'))
    })
    
    return(rv)
    
  })
}