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
}, options = list(paging = TRUE, pageLength = 25, searching = FALSE), rownames=NULL)

output$LTS_def <- DT::renderDataTable({
  req(LTS_Data())
  LTS_Data()[[i()]][["def"]]
}, options = list(paging = FALSE, searching = FALSE, ordering=FALSE, dom='t'), rownames=NULL)

output$LTS_NewVal <- DT::renderDataTable({
  LTS_new_val
}, options = list(paging = FALSE, searching = FALSE, ordering=FALSE, dom='t'), rownames=NULL, editable=TRUE)

# Data Figures
output$LTS_plot <- shiny::renderPlot({
  input$LTS_ApplyNewValue
  req(LTS_Data())
  par(mfrow=c(2,1))
  plot_lts_data(x = getData("LTS_dat")[[i()]], type=1)
  plot_lts_data(x = getData("LTS_dat")[[i()]], type=2)
  par(mfrow=c(2,1))
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
