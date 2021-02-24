#
# Long Term Stability (Server Code)
#
LTS_Data <- reactive({
  if (is.null(getData("LTS_dat")) && !is.null(input$LTS_input_file)) {
    file.type <- tools::file_ext(input$LTS_input_file$datapath)
    if (tolower(file.type)=="rdata") {
      tryCatch({ load(input$LTS_input_file$datapath[1]) }, error = function(e) { stop(safeError(e)) })
      if (!exists("LTS_dat")) {
        warning("Did load RData backup but could not find object 'LTS_dat' inside.")
        LTS_dat <- NULL
      }
    } else {
      LTS_dat <- read_lts_input(file = input$LTS_input_file$datapath[1])
      check_validity <- TRUE; i <- 0
      while (check_validity & i < length(LTS_dat)) {
        for (i in 1:length(LTS_dat)) {
          validate(
            need(c("KW","KW_Def","KW_Unit","CertVal","U","U_Def","Device","Method") %in% colnames(LTS_dat[[i]][["def"]]), "No all required input columns found in input file 'definition' part."),
            need(c("Value","Date","File") %in% colnames(LTS_dat[[i]][["val"]]), "No all required input columns found in input file 'data' part.")
          )
          if (class(LTS_dat[[i]][["val"]][,"Date"])!="Date") {
            LTS_dat[[i]][["val"]][,"Date"] <- as.Date.character(LTS_dat[[i]][["val"]][,"Date"],tryFormats = c("%Y-%m-%d","%d.%m.%Y","%Y/%m/%d"))
          }
          validate(need(class(LTS_dat[[i]][["val"]][,"Date"])=="Date", "Sorry, could not convert column 'Date' into correct format."))
        }
      }
    }
    assign("LTS_dat", value=LTS_dat, envir = env_perm)
    return(LTS_dat)
  } else {
    return(getData("LTS_dat"))
  }
})

output$LTS_fileUploaded <- reactive({
  return(!is.null(LTS_Data()))
})
outputOptions(output, 'LTS_fileUploaded', suspendWhenHidden=FALSE)

LTS_KWs <- reactive({
  req(LTS_Data())
  sapply(LTS_Data(), function(x) { x[["def"]][,"KW"] })
})

output$LTS_sel_KW <- renderUI({
  req(LTS_KWs())
  selectInput(inputId="LTS_sel_KW", label="KW", choices=LTS_KWs())
})

i <- reactive({
  req(input$LTS_sel_KW)
  which(LTS_KWs() %in% input$LTS_sel_KW)
})

LTS_new_val <- data.frame("Value"=0.0, "Date"=as.Date(format(Sys.time(), "%Y-%m-%d")), "File"=as.character("filename"), stringsAsFactors = FALSE)
LTS_tmp_val <- reactiveVal(LTS_new_val)

# Data Tables
output$LTS_vals <- DT::renderDataTable({
  req(LTS_Data())
  input$LTS_ApplyNewValue
  getData("LTS_dat")[[i()]][["val"]]
}, options = list(paging = TRUE, pageLength = 25, searching = FALSE), rownames=NULL, server = FALSE, selection = 'single')


output$LTS_def <- DT::renderDataTable({
  req(LTS_Data())
  LTS_Data()[[i()]][["def"]]
}, options = list(paging = FALSE, searching = FALSE, ordering=FALSE, dom='t'), rownames=NULL)

output$LTS_NewVal <- DT::renderDataTable({
  LTS_new_val
}, options = list(paging = FALSE, searching = FALSE, ordering=FALSE, dom='t'), rownames=NULL, editable=TRUE)

# calculation month
# TODO export to a package function
mondf <- function(d_start, d_end) { 
  lt <- as.POSIXlt(as.Date(d_start, origin="1900-01-01"))
  d_start <- lt$year*12 + lt$mon 
  lt <- as.POSIXlt(as.Date(d_end, origin="1900-01-01"))
  d_end <- lt$year*12 + lt$mon 
  return(d_end - d_start )
}


d = reactive({
  x = getData("LTS_dat")[[i()]]
  vals <- x[["val"]][,"Value"]
  rt <- x[["val"]][,"Date"]
  mon <- sapply(rt, function(x) { mondf(d_start = rt[1], d_end = x) })
  data.frame(mon, vals)
})

previous = reactiveVal(-1)

# # REPORT LTS
# output$Report <- downloadHandler(
#   filename = paste0("LTS_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf" ),
#   content = function(file) {
#     # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
#     owd <- setwd(tempdir(check = TRUE))
#     on.exit(setwd(owd))
#     writeLines(text = Report_Vorlage_Analyt(), con = 'Report_Vorlage_tmp.Rmd')
#     out <- rmarkdown::render(
#       input = 'Report_Vorlage_tmp.Rmd',
#       output_format = rmarkdown::pdf_document(),
#       params = list("res" = c_res()),
#       # !!! das ist die Liste mit Eingabewerten für die weitere Verarbeitung im Report
#       envir = new.env(parent = globalenv())
#     )
#     file.rename(out, file)
#   }
# )


observeEvent(input$LTS_vals_rows_selected, {
  req(LTS_Data(), commentlist)
  if(!is.null(input$LTS_vals_rows_selected)){
    e = commentlist[["df"]][[input$LTS_vals_rows_selected]]
    shinyjs::enable(id = "datacomment")
  } else {
    # when no row is selected
    e = NA
    shinyjs::disable(id = "datacomment")
  }
  updateTextInput(session,"datacomment", value = e) # clear textInput when deselected
}, ignoreNULL = FALSE)

# create list for populating with comments (3 steps)
d_length = reactive({
  n = nrow(d())
  rep(NA, n)
})
commentlist = reactiveValues("df"= NULL) #  reactive({nrow(d())})
observe({
  commentlist$df = d_length()
})


observeEvent(input$datacomment, {
  req(input$LTS_vals_rows_selected)
  
  if(input$datacomment != "" ){
    # for some reason, after switching from commented row to another, shiny gives ""
    commentlist[["df"]][[input$LTS_vals_rows_selected]] = input$datacomment
  }
})

# Data Figures
output$LTS_plot1_1 <- shiny::renderPlot({ 
  s = input$LTS_vals_rows_selected  
  input$LTS_ApplyNewValue
  req(LTS_Data())
  plot_lts_data(x = getData("LTS_dat")[[i()]], type=1)
  ### select a point in data table and mark in plot
  if (length(s)) points(x = d()[s,"mon"],y = d()[s,"vals"], pch = 19, cex = 2)
  ### point selection in data table -- plot 1 -- end
})

output$LTS_plot1_2 = shiny::renderPlot({
  input$LTS_ApplyNewValue
  req(LTS_Data())
  # par(mfrow=c(2,1))
  plot_lts_data(x = getData("LTS_dat")[[i()]], type=2)
})

# select Rows in data table proxy when clicking on a point in the plot
proxy = DT::dataTableProxy("LTS_vals")
observeEvent(input$plot1_click, {
  a = nearPoints(d(), input$plot1_click, xvar = "mon", yvar = "vals", addDist = TRUE)
  ind = which(d()$mon==a$mon & d()$vals==a$vals)
  DT::selectRows(proxy, ind)
})

# small information which point was clicked
output$click_info <- renderPrint({
  s = input$LTS_vals_rows_selected
  if (length(s)){
    c = commentlist[["df"]][[input$LTS_vals_rows_selected]]
   #if(!is.na(c)){
     data.frame(d()[s,],"comment"=c)
  #  } else {
 #   data.frame(d()[s,])
  #  }
  }
})

output$LTS_plot2 <- shiny::renderPlot({
  input$LTS_ApplyNewValue
  req(LTS_Data())
  tmp <- getData("LTS_dat")[[i()]]
  if(nrow(tmp[["val"]])>4) {
    est <- sapply(4:nrow(tmp[["val"]]), function(i) { 
      x <- tmp; x[["val"]] <- x[["val"]][1:i,]
      plot_lts_data(x = x, type=0) 
    })
    plot(x=tmp[["val"]][-c(1:3),"Date"], y=est, xlab="Measurement Point", ylab="LTS month estimate")
  }
})

#Add/Remove Value
observeEvent(input[["LTS_NewVal_cell_edit"]], {
  cell <- input[["LTS_NewVal_cell_edit"]]
  i <- cell$row
  j <- 1+cell$col
  tmp <- LTS_tmp_val()
  tmp[i, j] <- DT::coerceValue(val = cell$value, old = tmp[i, j])
  LTS_tmp_val(tmp)
})

observeEvent(input$LTS_ApplyNewValue, ignoreNULL = TRUE, ignoreInit = TRUE, {
  if (input$LTS_ApplyNewValue>=1) {
    LTS_dat <- getData("LTS_dat")
    LTS_dat[[i()]][["val"]] <- rbind(LTS_dat[[i()]][["val"]], LTS_tmp_val())
    assign("LTS_dat", value=LTS_dat, envir = env_perm)
  }
})

# BACKUP
output$LTS_Save <- downloadHandler(
  filename = function() { paste0(getData("LTS_dat")[[i()]][["def"]][,"RM"], '.RData') },
  content = function(file) {
    LTS_dat <- getData("LTS_dat")
    save(LTS_dat, file = file)
  },
  contentType = "RData"
)
