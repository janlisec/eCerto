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
#' @return rdata A reactive, but only for notifying the navbarpanel to change
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
m_RDataImport_Server = function(id, rv = reactiveClass$new(init_rv()), silent=FALSE) {
  stopifnot(R6::is.R6(rv))
  stopifnot(shiny::is.reactivevalues(rv$get()))

  shiny::moduleServer(id, function(input, output, session) {

    continue = reactiveVal(NULL) # NULL -> don't continue
    
    # Upload
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

    # Is anything already uploaded via Excel? If so, show Window Dialog
    shiny::observeEvent(rdata(),{
      ttt = sapply(rv$names(), function(x) {!is.null(getValue(rv,c(x,"uploadsource")))},simplify = "array")
      if(any(ttt)){
        showModal(
          modalDialog(
            title = "Existent data",
            htmltools::HTML("Modul(s) <u>", paste(names(ttt[ttt==TRUE]),collapse=", "), "</u> are already existent. Are you sure you want to continue?"),
            footer = tagList(
              actionButton(shiny::NS(id,"cancel"), "Cancel"),
              actionButton(shiny::NS(id,"overwrite"), "Overwrite", class = "btn btn-danger")
            )
          )
        )
      } else {
        continue(TRUE)
      }
    })
    
    # the observers from before
    # shall be overwritten?
    observeEvent(input$overwrite, {
      continue(TRUE)
      showNotification("Overwritten")
      removeModal()
    })
    # shall be cancelled?
    observeEvent(input$cancel, {
      removeModal()
    })
    
    shiny::observeEvent(continue(),{
      # whereami::cat_where(where = "RData_import: RData uploaded", color = "grey")
      res <- rdata()

      if ("General.dataformat_version" %in% names(unlist(res, recursive = FALSE))) 
        {
        # Non-legacy upload #####
        # import functions for defined data_format schemes
        if ( res$General$dataformat_version=="2021-05-27") {
          # rv should contain all variables from uploaded res
          resnames <- listNames(l = res, maxDepth = 2) # names(unlist(res, recursive = FALSE))
          rvnames <- listNames(shiny::reactiveValuesToList(rv$get()),2) # names(unlist(shiny::reactiveValuesToList(rv$get()), recursive = FALSE))

          
          if (all(resnames %in% rvnames)) {
            # Transfer list elements
            # $$ToDo$$ one might provide a warning to the user in case he will
            # overwrite non empty fields i.e. he did load Stab data and now
            # reads an RData backup which already contains Stab data
            
            for (i in names(res)) {
              # for (j in names(res[[i]])) {
              #   setValue(rv, c(i,j), res[[i]][[j]])
              # }
              setValue(rv,i,res[[i]])
            }
            # reset time_stamp with current $$ToDo think if this is really desirable
            setValue(rv,c("General","time_stamp"),Sys.time())
            setValue(rv,c("Certification","uploadsource"),value = "RData")
            message("RDataImport: Non-legacy upload finished")
          } else {
            allgivenexpected = c(paste0("file: ", resnames), paste0("\nexpected: ", rvnames))
            found_table = names(which(table(c(resnames, rvnames))==1))
            err <- allgivenexpected[c(resnames, rvnames) %in% found_table]
            shinyalert::shinyalert(title = "m_RDataImport_Server", text = paste("The following components were inconsistent between loaded RData file and internal data structure:\n", paste(err, collapse=", ")), type = "warning")
          }
        }
        # Legacy upload
      } else {
        if ("Certification" %in% names(res) && !is.null(res$Certification)) {
          if (!silent) message("RDataImport_Server: Cert data transfered")
          setValue(rv,c("Certification","data"),res[["Certification"]][["data_input"]])
          setValue(rv,c("Certification","input_files"),res[["Certification"]][["input_files"]])
          setValue(rv,c("Certification","uploadsource"),value = "RData")
          # save
          setValue(rv,c("General","user"),res$Certification$user)
          setValue(rv,c("General","study_id"),res$Certification$study_id)
          # processing
          setValue(rv,c("Certification.processing","lab_means"), res[["Certification"]][["lab_means"]])
          setValue(rv,c("Certification.processing","cert_mean"),res[["Certification"]][["cert_mean"]])
          setValue(rv,c("Certification.processing","cert_sd"),res[["Certification"]][["cert_sd"]])
          setValue(rv,c("Certification.processing","normality_statement"),res[["Certification"]][["normality_statement"]])
          setValue(rv,c("Certification.processing","precision"),res[["Certification"]][["precision"]])

          setValue(rv,c("Certification.processing","data_kompakt"),res[["Certification"]][["data_kompakt"]])
          setValue(rv,c("Certification.processing","CertValPlot"),res[["Certification"]][["CertValPlot"]])
          setValue(rv,c("Certification.processing","stats"),res[["Certification"]][["stats"]])
          setValue(rv,c("Certification.processing","boxplot"),res[["Certification"]][["boxplot"]])
          setValue(rv,c("Certification.processing","opt"),res[["Certification"]][["opt"]])
          setValue(rv,c("Certification.processing","mstats"),res[["Certification"]][["mstats"]])
          # materialtabelle
          setValue(rv,c("materialtabelle"),res[["Certification"]][["cert_vals"]])
        }
        if ("Homogeneity" %in% names(res) && !is.null(res$Homogeneity)) {
          if (!silent) message("RDataImport_Server: Homog data transfered")
          setValue(rv,c("Homogeneity","data"),res[["Homogeneity"]][["h_dat"]])
          set_uploadsource(rv = rv, m = "Homogeneity", uploadsource = "RData")
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
          set_uploadsource(rv = rv, m = "Stability", uploadsource = "RData")
          setValue(rv,c("Stability","s_vals"),res[["Stability"]][["s_vals"]])
        }
        setValue(rv,c("General","time_stamp"),Sys.time())
      }
      },ignoreNULL = TRUE)
    


    shiny::observeEvent(getValue(rv,c("General", "user")) , {
      # if (!silent) whereami::cat_where(whereami::whereami(),color = "blue")
      shiny::updateTextInput(
        session = session,
        inputId = "user",
        value = getValue(rv,c("General","user"))
      )
    })

    shiny::observeEvent(getValue(rv,c("General", "study_id")), {
      # if (!silent) message("m_RDataImport_Server: observeEvent(input$study_id")
      shiny::updateTextInput(
        session = session,
        inputId = "study_id",
        value =  getValue(rv,c("General","study_id"))
      )
      # setValue(rv,c("General","study_id"),input$study_id)
    })

    shiny::observeEvent(input$user, {
      setValue(rv,c("General","user"),input$user)
    })
    
    shiny::observeEvent(input$study_id, {
      setValue(rv,c("General","study_id"),input$study_id)
    })

    # DOWNLOAD
    output$ecerto_backup <- shiny::downloadHandler(
      filename = function() {
        paste0(ifelse(is.null(getValue(rv,c("General","study_id"))), "TEST", getValue(rv,c("General","study_id"))), '.RData')
      },
      content = function(file) {
        res <- shiny::reactiveValuesToList(getValue(rv))
        res$General$dataformat_version = "2021-05-27"
        save(res, file = file)
      },
      contentType = "RData"
    )

    return(continue)

  })

}

