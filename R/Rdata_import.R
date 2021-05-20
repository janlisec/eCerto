.RDataImport_UI <- function(id) {
  tagList(
    wellPanel(
      fileInput(
        inputId = NS(id,"in_file_ecerto_backup"),
        label = "Load Previous Analysis",
        multiple = FALSE,
        accept = c("RData")
      ),
    ),
    
    shinyjs::disabled(
      wellPanel(
        id = NS(id,"savepanel"),
        strong("Save"),
        fluidRow(
          column(6, textInput(
            inputId = NS(id,"user"),
            label = "User",
            value = "FK"
          )),
          column(
            6,
            textInput(
              inputId = NS(id,"study_id"),
              label = "Study ID",
              value = "TEST"
            )
          ),
          column(
            3,
            downloadButton(outputId = 'ecerto_backup', label = "Backup")
          ),
        )
      )
    )
  )
}

.RDataImport_Server = function(id, rv) {
  stopifnot(is.reactivevalues(rv))
  shiny::moduleServer(id, function(input, output, session) {
    
    # observeEvent(input$in_file_ecerto_backup,{
    rdata = eventReactive(input$in_file_ecerto_backup,{
      
      file.type <-
        tools::file_ext(input$in_file_ecerto_backup$datapath)
      validate(
        need(tolower(file.type) == "rdata","Only RData allowed."),
        need(length(file.type) == 1,"Please select only one RData file.")
      )
      file.type <- "RData"
      tryCatch({
        load(input$in_file_ecerto_backup$datapath[1])
      }, error = function(e) {
        stop(safeError(e))
      })
      
      
    }, ignoreNULL = TRUE)
    
    observeEvent(rdata(),{
      message("RData uploaded")
      browser()
      if("Certification" %in% names(res)){
        message("RData version 26")
        rv$Certifications$data = res[["Certification"]][["data_input"]]
        rv$Certifications$input_files = res[["Certification"]][["input_files"]]
        set_listUploadsource(rv = rv, m = "Certifications",uploadsource = "RData")
        # save
        rv$Certifications$user = res$Certification$user
        rv$Certifications$study_id = res$Certification$study_id
        # processing
        rv$Certifications$lab_means = res[["Certification"]][["lab_means"]]
        rv$Certifications$cert_mean = res[["Certification"]][["cert_mean"]]
        rv$Certifications$cert_sd = res[["Certification"]][["cert_sd"]]
        rv$Certifications$normality_statement = res[["Certification"]][["normality_statement"]]
        rv$Certifications$precision = res[["Certification"]][["precision"]]
        rv$Certifications$data_kompakt = res[["Certification"]][["data_kompakt"]]
        rv$Certifications$CertValPlot = res[["Certification"]][["CertValPlot"]]
        rv$Certifications$stats = res[["Certification"]][["stats"]]
        rv$Certifications$boxplot = res[["Certification"]][["boxplot"]]
        rv$Certifications$opt = res[["Certification"]][["opt"]]
        rv$Certifications$mstats = res[["Certification"]][["mstats"]]
        # materialtabelle
        rv$Certifications$materialtabelle = res[["Certification"]][["cert_vals"]]
        
        rv$Homogeneity$data = res[["Homogeneity"]][["h_dat"]]
        set_listUploadsource(rv = rv, m = "Homogeneity",uploadsource = "RData")
        rv$Homogeneity$h_file = res[["Homogeneity"]][["h_file"]]
        # Processing
        rv$Homogeneity$h_vals =  res[["Homogeneity"]][["h_vals"]]
        rv$Homogeneity$h_sel_analyt = res[["Homogeneity"]][["h_sel_analyt"]]
        rv$Homogeneity$h_precision = res[["Homogeneity"]][["h_precision"]]
        rv$Homogeneity$h_Fig_width = res[["Homogeneity"]][["h_Fig_width"]]
        
        rv$Stability$s_file = res[["Stability"]][["s_file"]]
        rv$Stability$data = res[["Stability"]][["s_dat"]]
        set_listUploadsource(rv = rv, m = "Stability",uploadsource = "RData")
        rv$Stability$s_vals = res[["Stability"]][["s_vals"]]
      } else {
        
        # rv should contain all variables from uploaded res
        resnames = names(unlist(res, recursive = FALSE))
        rvnames = names(unlist(reactiveValuesToList(rv), recursive = FALSE))
        validate(need(all(resnames %in% rvnames),"variable names differ"))
        
        list12 = Map(c, reactiveValuesToList(rv), res) 
        rv_tmp = do.call("reactiveValues", res)
      }

    })
    
    observeEvent(rv$Certifications$user,{
      
      if(!is.null(rv$Certifications$user)){
        shinyjs::enable(id = "savepanel")
        
        updateTextInput(
          session = session,
          inputId = "user",
          value = rv$Certifications$user
        )
      }
    })
  })
}

