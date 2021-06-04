#'@title RDataImport.
#'
#'@description
#'\code{RDataImport} will provide a module to upload/backup Rdata files for certification trial data.
#'
#'@details
#'not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv ReavtiveValues $$.
#' @param silent Option to print or omit status messages.
#'
#'@return
#'A reactive dataframe.
#'
#'@examples
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(m_RDataImport_UI(id = "test")),
#'  server = function(input, output, session) { 
#'    out <- m_RDataImport_Server(id = "test");  
#'    observeEvent(out$Certifications$time_stamp, {
#'      print(out$Certifications$time_stamp)
#'    }) 
#'  }
#' )
#'
#'@export
#'
m_RDataImport_UI <- function(id) {
  tagList(
    wellPanel(
      fileInput(
        inputId = NS(id,"in_file_ecerto_backup"),
        label = "Load Previous Analysis",
        multiple = FALSE,
        accept = c("RData")
      ),
    ),
    
    #shinyjs::disabled(
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
          downloadButton(outputId = NS(id,'ecerto_backup'), label = "Backup")
        ),
      )
    )
    #)
  )
}

#' @export
m_RDataImport_Server = function(id, rv=reactiveClass$new(init_rv())) {
  stopifnot(R6::is.R6(rv))
  stopifnot(is.reactivevalues(rv$get()))
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # observeEvent(input$in_file_ecerto_backup,{
    rdata <- eventReactive(input$in_file_ecerto_backup, {
      file.type <- tools::file_ext(input$in_file_ecerto_backup$datapath)
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
      return(res)
    }, ignoreNULL = TRUE)
    
    observeEvent(rdata(),{
      message("RDataImport_Server: RData uploaded")
      res <- rdata()
      shiny::reactiveValuesToList(rv$get())
      if ("Certifications.dataformat_version" %in% names(unlist(res, recursive = FALSE))) {
        # import functions for defined data_format schemes
        if ( res$Certifications$dataformat_version=="2021-05-27") {
          # rv should contain all variables from uploaded res
          resnames <- names(unlist(res, recursive = FALSE))
          rvnames <- names(unlist(reactiveValuesToList(rv$get()), recursive = FALSE))
          if (all(resnames %in% rvnames)) {
            # browser()
            #list12 <- Map(c, reactiveValuesToList(rv), res)
            #rv_tmp <- do.call("reactiveValues", res)
            #rv <- rv_tmp
            # Transfer list elements
            # $$ToDo$$ one might provide a warning to the user in case he will
            # overwrite non empty fields i.e. he did load Stab data and now
            # reads an RData backup which already contains Stab data
            for (i in names(res)) {
              # könnte schieflaufen hier
              browser()
              setValue(rv,i,res[[i]])
            }
            setValue(rv,c("Certifications","time_stamp"),Sys.time())
            # rv$Certifications$time_stamp <- Sys.time()
          } else {
            err <- c(paste0("file_", resnames), paste0("expected_", rvnames))[c(resnames, rvnames) %in% names(which(table(c(resnames, rvnames))==1))]
            shinyalert::shinyalert(title = "m_RDataImport_Server", text = paste("The following components were inconsistent between loaded RData file and internal data structure:", paste(err, collapse=", ")), type = "warning")
          }
        }
      } else {
        # import functions for legacy data_format schemes
        if ("Certification" %in% names(res) && !is.null(res$Certification)) {
          message("RDataImport_Server: Cert data transfered")
          setValue(rv,c("Certifications","data"),res[["Certification"]][["data_input"]])
          setValue(rv,c("Certifications","input_files"),res[["Certification"]][["input_files"]])
          set_listUploadsource(rv = rv, m = "Certifications",uploadsource = "RData")
          # save
          setValue(rv,c("Certifications","user"),res$Certification$user)
          setValue(rv,c("Certifications","study_id"),res$Certification$study_id)
          # processing
          setValue(rv,c("Certifications","lab_means"), res[["Certification"]][["lab_means"]])
          setValue(rv,c("Certifications","cert_mean"),res[["Certification"]][["cert_mean"]])
          setValue(rv,c("Certifications","cert_sd"),res[["Certification"]][["cert_sd"]])
          setValue(rv,c("Certifications","normality_statement"),res[["Certification"]][["normality_statement"]])
          setValue(rv,c("Certifications","precision"),res[["Certification"]][["precision"]])
          
          setValue(rv,c("Certifications","data_kompakt"),res[["Certification"]][["data_kompakt"]])
          setValue(rv,c("Certifications","CertValPlot"),res[["Certification"]][["CertValPlot"]])
          setValue(rv,c("Certifications","stats"),res[["Certification"]][["stats"]])
          setValue(rv,c("Certifications","boxplot"),res[["Certification"]][["boxplot"]])
          setValue(rv,c("Certifications","opt"),res[["Certification"]][["opt"]])
          setValue(rv,c("Certifications","mstats"),res[["Certification"]][["mstats"]])
          
          setValue(rv,c("Certifications","materialtabelle"),res[["Certification"]][["cert_vals"]])
          
          # materialtabelle
          setValue(rv,c("Certifications","materialtabelle"),res[["Certification"]][["cert_vals"]])
        }
        if ("Homogeneity" %in% names(res) && !is.null(res$Homogeneity)) {
          message("RDataImport_Server: Homog data transfered")
          setValue(rv,c("Homogeneity","data"),res[["Homogeneity"]][["h_dat"]])
          set_listUploadsource(rv = rv, m = "Homogeneity", uploadsource = "RData")
          setValue(rv,c("Homogeneity","h_file"),res[["Homogeneity"]][["h_file"]])
          # Processing
          setValue(rv,c("Homogeneity","h_vals"),res[["Homogeneity"]][["h_vals"]])
          setValue(rv,c("Homogeneity","h_sel_analyt"),res[["Homogeneity"]][["h_sel_analyt"]])
          setValue(rv,c("Homogeneity","h_precision"),res[["Homogeneity"]][["h_precision"]])
          setValue(rv,c("Homogeneity","h_Fig_width"),res[["Homogeneity"]][["h_Fig_width"]])
        }
        if ("Stability" %in% names(res) && !is.null(res$Stability)) {
          message("RDataImport_Server: Stab data transfered")
          setValue(rv,c("Stability","file"),res[["Stability"]][["s_file"]])
          setValue(rv,c("Stability","data"),res[["Stability"]][["s_dat"]])
          set_listUploadsource(rv = rv, m = "Stability", uploadsource = "RData")
          setValue(rv,c("Stability","s_vals"),res[["Stability"]][["s_vals"]])
        }
        setValue(rv,c("Certifications","time_stamp"),Sys.time())
        # rv$Certifications$time_stamp <- Sys.time()
      }
      
    })
    
    observeEvent(getValue(rv,"Certifications")$time_stamp , {
      #message("observeEvent(rv$Certifications$time_stamp")
      updateTextInput(
        session = session,
        inputId = "user",
        value = getValue(rv,c("Certifications","user"))
      )
      updateTextInput(
        session = session,
        inputId = "study_id",
        value =  getValue(rv,c("Certifications","study_id"))
      )
    })
    
    observeEvent(input$study_id, {
      setValue(rv,c("Certifications","study_id"),input$study_id)
      # rv$Certifications$study_id <- input$study_id
    })
    
    observeEvent(input$user, {
      setValue(rv,c("Certifications","user"),input$user)
      # rv$Certifications$user <- input$user
    })
    
    output$ecerto_backup <- downloadHandler(
      filename = function() { 
        paste0(ifelse(is.null(getValue(rv,c("Certifications","study_id"))), "TEST", getValue(rv,c("Certifications","study_id"))), '.RData') 
      },
      content = function(file) {
        res <- shiny::reactiveValuesToList(getValue(rv))
        # browser()
        res$Certifications$dataformat_version = "2021-05-27"
        save(res, file = file)
      },
      contentType = "RData"
    )
    
    # return(rv)
    
  })
  
}

