#' @name RDataImport
#' @aliases m_RDataImport_UI
#' @aliases m_RDataImport_Server
#'
#' @title RDataImport.
#'
#' @description
#' \code{RDataImport} will provide a module to upload/backup Rdata files for certification trial data.
#'
#' @details
#' not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv ReavtiveValues $$.
#' @param silent Option to print or omit status messages.
#'
#' @return rdata A reactive, but only for notifying the navbarpanel
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(ecerto::m_RDataImport_UI(id = "test")),
#'  server = function(input, output, session) {
#'    ecerto::m_RDataImport_Server(id = "test")
#'  }
#' )
#' }
#'
#' @rdname RDataImport
#' @export
#'
m_RDataImport_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::wellPanel(
      shiny::fileInput(
        inputId = ns("in_file_ecerto_backup"),
        label = "Load Previous Analysis",
        multiple = FALSE,
        accept = c("RData")
      ),
    ),
    shiny::wellPanel(
      id = ns("savepanel"),
      shiny::strong("Save"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::textInput(
            inputId = ns("user"),
            label = "User",
            value = "FK"
          )
        ),
        shiny::column(
          width = 6,
          shiny::textInput(
            inputId = ns("study_id"),
            label = "Study ID",
            value = "TEST"
          )
        ),
        shiny::column(
          width = 3,
          shiny::downloadButton(outputId = ns('ecerto_backup'), label = "Backup")
        )
      )
    )
  )
}

#' @rdname RDataImport
#' @export
m_RDataImport_Server = function(id, rv=reactiveClass$new(init_rv()), silent=FALSE) {
  stopifnot(R6::is.R6(rv))
  stopifnot(is.reactivevalues(rv$get()))

  shiny::moduleServer(id, function(input, output, session) {

    rdata <- shiny::eventReactive(input$in_file_ecerto_backup, {
      file.type <- tools::file_ext(input$in_file_ecerto_backup$datapath)
      shiny::validate(
        shiny::need(tolower(file.type) == "rdata","Only RData allowed."),
        shiny::need(length(file.type) == 1,"Please select only one RData file.")
      )
      file.type <- "RData"
      load_envir <- new.env()
      tryCatch({
        load(input$in_file_ecerto_backup$datapath[1], envir = load_envir)
      }, error = function(e) {
        stop(shiny::safeError(e))
      })
      return(get(x = "res", envir = load_envir))
    }, ignoreNULL = TRUE)

    shiny::observeEvent(rdata(),{
      if (!silent) message("m_RDataImport_Server: observeEvent(rdata()): RData uploaded")
      res <- rdata()
      # @Frederick: die nächste Zeile hat keine Zuweisung. Kann sie raus oder ist sie aus reaktiven Gründen drin?
      # @Jan Konnte raus, war nur für print (23. Juni)
      if ("Certifications.dataformat_version" %in% names(unlist(res, recursive = FALSE))) {
        # import functions for defined data_format schemes
        if ( res$Certifications$dataformat_version=="2021-05-27") {
          # rv should contain all variables from uploaded res
          resnames <- names(unlist(res, recursive = FALSE))
          rvnames <- names(unlist(shiny::reactiveValuesToList(rv$get()), recursive = FALSE))
          if (all(resnames %in% rvnames)) {
            # Transfer list elements
            # $$ToDo$$ one might provide a warning to the user in case he will
            # overwrite non empty fields i.e. he did load Stab data and now
            # reads an RData backup which already contains Stab data
            #browser()
            for (i in names(res)) {
              # @Frederick: ja, ist glaube ich schief gelaufen...
              # könnte schieflaufen hier
              #setValue(rv,i,res[[i]])
              for (j in names(res[[i]])) {
                setValue(rv, c(i,j), res[[i]][[j]])
              }
            }
            # reset time_stamp with current $$ToDo think if this is really desirable
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
          if (!silent) message("RDataImport_Server: Cert data transfered")
          #browser()
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
          # materialtabelle
          setValue(rv,c("Certifications","materialtabelle"),res[["Certification"]][["cert_vals"]])

          # @Frederick: Warum gibt es diese Zeile 2x? Notwendig wegen reactive oder Versehen?
          # @Jan war ein Versehen beim Ändern, --> gelöscht (23. Juni)
   
        }
        if ("Homogeneity" %in% names(res) && !is.null(res$Homogeneity)) {
          if (!silent) message("RDataImport_Server: Homog data transfered")
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
          if (!silent) message("RDataImport_Server: Stab data transfered")
          setValue(rv,c("Stability","file"),res[["Stability"]][["s_file"]])
          setValue(rv,c("Stability","data"),res[["Stability"]][["s_dat"]])
          set_listUploadsource(rv = rv, m = "Stability", uploadsource = "RData")
          setValue(rv,c("Stability","s_vals"),res[["Stability"]][["s_vals"]])
        }
        setValue(rv,c("Certifications","time_stamp"),Sys.time())
      }
      })

    # shiny::observeEvent(getValue(rv,c("Certifications","time_stamp)) , {
    #   #if (!silent) message("observeEvent(rv$Certifications$time_stamp")
    #   shiny::updateTextInput(
    #     session = session,
    #     inputId = "user",
    #     value = getValue(rv,c("Certifications","user"))
    #   )
    #   shiny::updateTextInput(
    #     session = session,
    #     inputId = "study_id",
    #     value =  getValue(rv,c("Certifications","study_id"))
    #   )
    # })

    shiny::observeEvent(getValue(rv,c("Certifications", "user")) , {
      if (!silent) message("m_RDataImport_Server: observeEvent(getValue(rv,'Certifications')$user")
      shiny::updateTextInput(
        session = session,
        inputId = "user",
        value = getValue(rv,c("Certifications","user"))
      )
      # shiny::updateTextInput(
      #   session = session,
      #   inputId = "study_id",
      #   value =  getValue(rv,c("Certifications","study_id"))
      # )
    })

    shiny::observeEvent(input$study_id, {
      if (!silent) message("m_RDataImport_Server: observeEvent(input$study_id")
      setValue(rv,c("Certifications","study_id"),input$study_id)
      # rv$Certifications$study_id <- input$study_id
    })

    shiny::observeEvent(input$user, {
      if (!silent) message("m_RDataImport_Server: observeEvent(input$user")
      setValue(rv,c("Certifications","user"),input$user)
      # rv$Certifications$user <- input$user
    })

    output$ecerto_backup <- shiny::downloadHandler(
      filename = function() {
        paste0(ifelse(is.null(getValue(rv,c("Certifications","study_id"))), "TEST", getValue(rv,c("Certifications","study_id"))), '.RData')
      },
      content = function(file) {
        res <- shiny::reactiveValuesToList(getValue(rv))
        res$Certifications$dataformat_version = "2021-05-27"
        save(res, file = file)
      },
      contentType = "RData"
    )

    return(rdata)

  })

}

