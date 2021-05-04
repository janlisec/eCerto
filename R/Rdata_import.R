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
      message("RData uploaded")
      file.type <-
        tools::file_ext(input$in_file_ecerto_backup$datapath)
      validate(need(
        length(file.type) == 1 &
          tolower(file.type) == "rdata",
        "Please select only one RData file."
      ))
      file.type <- "RData"
      tryCatch({
        load(input$in_file_ecerto_backup$datapath[1])
      }, error = function(e) {
        stop(safeError(e))
      })
      if (!is.null(res[["Certification"]]))
        cert_data_in_backup_file <- TRUE
      
    }, ignoreNULL = TRUE)
    
    observeEvent(rdata(),{
      set_listUploadsource(rv = rv, m = "Certifications",uploadsource = "RData")
      rv$Certifications$user = res$Certification$user
      rv$Certifications$study_id = res$Certification$study_id
      
    })
    
    observeEvent(rv$Certifications$user,{
      
      if(!is.null(rv$Certifications$user)){
        print(rv$Certifications$user)
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

