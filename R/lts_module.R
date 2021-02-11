#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
.longtermstabilityServer = function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    datalist = reactiveValues("lts_data" = NULL, "comment"= NULL)
    
    LTS_Data <- reactive({
      if (!is.null(input$LTS_input_file)) {
      file.type <- tools::file_ext(input$LTS_input_file$datapath)
      if (tolower(file.type)=="rdata") {
          # tryCatch({ load(input$LTS_input_file$datapath[1]) }, error = function(e) { stop(safeError(e)) })
          # if (!exists("LTS_dat")) {
          #   warning("Did load RData backup but could not find object 'LTS_dat' inside.")
          #   LTS_dat <- NULL
          # }
        showModal(modalDialog(
          title = "Somewhat important message",
          "Currently not implemented for RData",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        LTS_dat <- read_lts_input(file = input$LTS_input_file$datapath[1])
        # plot(LTS_dat)
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
      #        assign("LTS_dat", value=LTS_dat, envir = env_perm)
      
      return(LTS_dat)
      #      } else {
      #        return(getData("LTS_dat"))
      }
    })
    
    observeEvent(LTS_Data(),{
      datalist$lts_data = LTS_Data()
    })
    
    output$LTS_fileUploaded <- reactive({
      return(!is.null(datalist$lts_data))
    })
    outputOptions(output, 'LTS_fileUploaded', suspendWhenHidden=FALSE)
    
    LTS_KWs <- reactive({
      #req(LTS_Data())
      sapply(datalist$lts_data, function(x) { x[["def"]][,"KW"] })
    })
    
    output$LTS_sel_KW <- renderUI({
      req(LTS_KWs())
      ns <- session$ns
      selectInput(inputId=ns("LTS_sel_KW"), label="KW", choices=LTS_KWs())
    })
    
    i <- reactive({
      req(input$LTS_sel_KW)
      which(LTS_KWs() %in% input$LTS_sel_KW)
    })
    
    LTS_new_val <- data.frame("Value"=0.0, "Date"=as.Date(format(Sys.time(), "%Y-%m-%d")), "File"=as.character("filename"), stringsAsFactors = FALSE)
    LTS_tmp_val <- reactiveVal(LTS_new_val)
    
    # Data Tables
    output$LTS_vals <- DT::renderDataTable({
      # req(LTS_Data())
      input$LTS_ApplyNewValue
      datalist$lts_data[[i()]][["val"]]
      # getData("LTS_dat")[[i()]][["val"]]
    }, options = list(
      paging = TRUE, 
      pageLength = 25, 
      searching = FALSE,
      stateSave = TRUE # for SelectPage
      ), rownames=NULL, server = FALSE, selection = 'single')
    
    output$LTS_def <- DT::renderDataTable({
      req(datalist$lts_data)
      datalist$lts_data[[i()]][["def"]]
    }, options = list(paging = FALSE, searching = FALSE, ordering=FALSE, dom='t'), rownames=NULL)
    
    output$LTS_NewVal <- DT::renderDataTable({
      LTS_new_val
    }, options = list(paging = FALSE, searching = FALSE, ordering=FALSE, dom='t'), rownames=NULL, editable=TRUE)
    
    d = reactive({
      x = datalist$lts_data[[i()]]
      # x = getData("LTS_dat")[[i()]]
      vals <- x[["val"]][,"Value"]
      rt <- x[["val"]][,"Date"]
      mon <- sapply(rt, function(x) { mondf(d_start = rt[1], d_end = x) })
      data.frame(mon, vals)
    })
    

    
    # when a row in table was selected
    observeEvent(input$LTS_vals_rows_selected, {
      req(datalist$lts_data, datalist)
      if(!is.null(input$LTS_vals_rows_selected)){
        # when a row is selected
        e = datalist[["comment"]][[input$LTS_vals_rows_selected]]
        s = input$LTS_vals_rows_selected # selected row
        shinyjs::enable(id = "datacomment")
        # change title when value was selected in table or plot
        updateTextInput(
          session = session,
          inputId = "datacomment",
          label = paste0("data comment for month ", 
            d()[s,"mon"], 
            " and value ", 
            d()[s,"vals"]
            ) 
        )
      } else {
        # when row gets deselected/ no row is selected
        e = NA
        shinyjs::disable(id = "datacomment")
        updateTextInput(
          session = session,
          inputId = "datacomment",
          label  = paste0(
            "data comment"
          ) 
        )
      }
      # when a value was entered before already
      updateTextInput(session,"datacomment", value = e) # clear textInput when deselected
    }, ignoreNULL = FALSE)
    
    
    # create list for populating with comments (2 steps)
    d_NAvec = reactive({ rep(NA, nrow(d())) })
    observe({ datalist$comment = d_NAvec() })
    
    # when some comment was entered, save in reactivevalues
    observeEvent(input$datacomment, {
      req(input$LTS_vals_rows_selected)
      if(input$datacomment != "" ){
        # for some reason, after switching from commented row to another, shiny gives ""
        datalist[["comment"]][[input$LTS_vals_rows_selected]] = input$datacomment
      }
    })
    
    # Data Figures
    output$LTS_plot1_1 <- shiny::renderPlot({ 
      s = input$LTS_vals_rows_selected
      c =  datalist[["comment"]]
      input$LTS_ApplyNewValue
      req(datalist$lts_data)
      plot_lts_data(x = datalist$lts_data[[i()]], type=1)
      ### select a point in data table and mark in plot -- plot 1 --
      if (length(s)) points(x = d()[s,"mon"],y = d()[s,"vals"], pch = 19, cex = 2)
      ### select a point in data table and mark in plot -- plot 1 -- end
      ### change color of triangle for commented points
      if(sum(!is.na(c))>=1) points(x = d()[!is.na(c),"mon"],y = d()[!is.na(c),"vals"], pch=24, bg="red")
    })
    
    output$LTS_plot1_2 = shiny::renderPlot({
      c =  datalist[["comment"]]
      input$LTS_ApplyNewValue
      req(datalist$lts_data)
      plot_lts_data(x = datalist$lts_data[[i()]], type=2)
      if(sum(!is.na(c))>=1) points(x = d()[!is.na(c),"mon"],y = d()[!is.na(c),"vals"], pch=24, bg="red")
    })
    
    output$LTS_plot2 <- shiny::renderPlot({
      c =  datalist[["comment"]]
      # c = c[-c(1:3)]
      input$LTS_ApplyNewValue
      req(datalist$lts_data)
      tmp <- datalist$lts_data[[i()]]
      if(nrow(tmp[["val"]])>4) {
        est <- sapply(4:nrow(tmp[["val"]]), function(i) { 
          x <- tmp; x[["val"]] <- x[["val"]][1:i,]
          plot_lts_data(x = x, type=0) 
        })
        plot(x=tmp[["val"]][-c(1:3),"Date"], y=est, xlab="Measurement Point", ylab="LTS month estimate")
        if(sum(!is.na(c))>=1) points(x = tmp[["val"]][!is.na(c),"Date"],y = est[!is.na(c)], pch=24, bg="red")
      }
    })
    
    # proxy for changing the table
    proxy = DT::dataTableProxy("LTS_vals") 
    
    #  when clicking on a point in the plot, select Rows in data table proxy
    observeEvent(input$plot1_click, {
      # 1/3 nearest point to click location 
      a = nearPoints(d(), input$plot1_click, xvar = "mon", yvar = "vals", addDist = TRUE)
      # 2/3 index in table
      ind = which(d()$mon==a$mon & d()$vals==a$vals) 
      # 3/3 
      DT::selectRows(proxy = proxy, selected =  ind)
      DT::selectPage(
        proxy = proxy,
        page = ind %/% input$LTS_vals_state$length + 1)
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
    
    # add new value
    observeEvent(input$LTS_ApplyNewValue, ignoreNULL = TRUE, ignoreInit = TRUE, {
      if (input$LTS_ApplyNewValue>=1) {
        datalist$lts_data[[i()]][["val"]] <- rbind(datalist$lts_data[[i()]][["val"]], LTS_tmp_val())
      }
    })
    
    # REPORT LTS
    output$Report <- downloadHandler(
      filename = paste0("LTS_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf" ),
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        rmdfile = system.file("rmd","LTSreport.Rmd", package = "ecerto")
        tempReport <- file.path(tempdir(), "LTSreport.Rmd")
        file.copy(rmdfile, tempReport, overwrite = TRUE)
        
        # Set up parameters to pass to Rmd document
        params <- list(
          c = datalist[["comment"]], 
          dat = datalist$lts_datas
          )
        
        if(tinytex::tinytex_root() == "") tinytex::install_tinytex()
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    # BACKUP
    output$LTS_Save <- downloadHandler(
      filename = function() { paste0(datalist$lts_data[[i()]][["def"]][,"RM"], '.RData') },
      content = function(file) {
       # LTS_dat <- getData("LTS_dat")
        save(datalist$lts_data, file = file)
      },
      contentType = "RData"
    )
    
    
  })
}

#' User Interface of LTS module
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
.longtermstabilityUI = function(id) {
   wellPanel(
     conditionalPanel(
       condition="output.LTS_fileUploaded == false",
       ns = NS(id), # namespace of current module
      fileInput(
        inputId = NS(id,"LTS_input_file"), 
        label = "Import Excel/RData File",
        multiple = FALSE, 
        accept = c("xls","xlsx","RData")
        ),
       helpText("Example Table"), imageOutput(NS(id,"myImage11a"), inline = TRUE),
     ),
    conditionalPanel(
      condition="output.LTS_fileUploaded == true",
      ns = NS(id), # namespace of current module
      fluidRow(
        column(4, DT::dataTableOutput(NS(id,"LTS_vals"))),
        column(
          8, 
          fluidRow(
            column(12, DT::dataTableOutput(NS(id,"LTS_def"))), 
            style = "margin-bottom:15px;"
            ),
          fluidRow(
            column(2, uiOutput(NS(id,"LTS_sel_KW"))),
            column(2, strong("Save LTS Data"), p(),downloadButton(NS(id,"LTS_Save"), label="Backup")),
            tags$style(type="text/css", "#LTS_Save {margin-top:-3%;}"),
            column(6, DT::dataTableOutput(NS(id,"LTS_NewVal"))),
            tags$style(type="text/css", "#LTS_NewVal {margin-top:-3%;}"),
            column(2, strong("New Entry"), p(), actionButton(inputId = NS(id,"LTS_ApplyNewValue"), label = "Add data")),
            tags$style(type="text/css", "#LTS_ApplyNewValue {margin-top:-1%;}")
            ),
          verbatimTextOutput(NS(id,"click_info")),
          fluidRow(
            column(
              8,
              shinyjs::disabled(
                textInput(
                  inputId = NS(id,"datacomment"), 
                  label = "data comment",
                  value = "",  
                  placeholder = "select point or row to comment"
                  )
                )
            ),
            column(
              4,
              wellPanel(
                fluidRow(strong("Download Report"),align = "center"),
                br(),
                fluidRow(downloadButton(NS(id,"Report")),align = "center") 
              )
            )
          ),
          fluidRow(column(12, plotOutput(NS(id,"LTS_plot1_1"), height = "450px", click = NS(id,"plot1_click")))),
          fluidRow(column(12, plotOutput(NS(id,"LTS_plot1_2"), height = "450px"))),
          fluidRow(column(12, plotOutput(NS(id,"LTS_plot2"), height = "450px")))
        )
      )
    ) # conditionalPanel
  ) # wellPanel
}
