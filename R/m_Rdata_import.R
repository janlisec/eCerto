#' @name RDataImport
#' @aliases m_RDataImport_UI
#' @aliases m_RDataImport_Server
#'
#' @title RDataImport.
#'
#' @description \code{RDataImport} will provide a module to upload/backup Rdata
#'   files for certification trial data.
#'
#' @details not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param modules c("Certification","Homogeneity","Stability").
#' @param uploadsources contains which of the \code{modules} has which
#'   uploadsource or \code{NULL}. For example if Certification has been
#'   uploaded, the argument would look like list("Certification" =
#'   "Excel","Homogeneity"=NULL,"stability"=NULL).
#' @param silent Option to print or omit status messages.
#'
#' @return rdata A reactive, but only for notifying the navbarpanel to change
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(ecerto::m_RDataImport_UI(id = "test")),
#'  server = function(input, output, session) {
#'    ecerto::m_RDataImport_Server(
#'      id = "test",
#'      modules = reactiveVal(c("Ceritification","Stability","Homogeneity")),
#'      uploadsource = reactiveVal("Excel")
#'      )
#'  }
#' )}
#'
#' @rdname RDataImport
#' @export
#' 
m_RDataImport_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::wellPanel(
      shiny::strong("Load"),
      shiny::fileInput(
        inputId = ns("in_file_ecerto_backup"),
        label = "Select Previous Analysis",
        multiple = FALSE,
        accept = c("RData")
      ),
    )
  )
}

#' @rdname RDataImport
#' @export
m_RDataImport_Server = function(id, modules, uploadsources, silent=FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    rvreturn = shiny::reactiveVal(NULL)
    continue <- shiny::reactiveVal(NULL) # NULL -> don't continue

    # Upload
    rdata <- shiny::eventReactive(input$in_file_ecerto_backup, {
      if(!silent) message("RData uploaded")
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
    shiny::observeEvent(rdata(), {
      
      ttt = sapply(modules(), function(x) {!is.null(uploadsources()[[x]])},simplify = "array")
      if(any(ttt)){
        if(!silent) message("RDataImport: Found existing data. Overwrite?")
        shiny::showModal(
          shiny::modalDialog(
            title = "Existent data",
            htmltools::HTML("Modul(s) <u>", paste(names(ttt[ttt==TRUE]),collapse=", "), "</u> are already existent. Are you sure you want to continue?"),
            footer = shiny::tagList(
              shiny::actionButton(shiny::NS(id,"cancel"), "Cancel"),
              shiny::actionButton(shiny::NS(id,"overwrite"), "Overwrite", class = "btn btn-danger")
            )
          )
        )
      } else {
        continue(TRUE)
      }
    })

    # the observers from before
    # shall be overwritten?
    shiny::observeEvent(input$overwrite, {
      continue(TRUE)
      shiny::showNotification("Overwritten")
      shiny::removeModal()
    })
    # shall be cancelled?
    shiny::observeEvent(input$cancel, {
      shiny::removeModal()
    })

    shiny::observeEvent(continue(), {
      res <- rdata()
      rv = reactiveClass$new(init_rv())
      if ("General.dataformat_version" %in% names(unlist(res, recursive = FALSE)))
        {

        # import functions for defined data_format schemes
        if ( res$General$dataformat_version=="2021-05-27") {
          # Non-legacy upload #####
          if(!silent) message("RDataImport: Non-legacy upload started")
          # rv should contain all variables from uploaded res split must be
          # false here, otherwise one name list is of class character the other
          # of class list -> Error
          resnames <- listNames(l = res, maxDepth = 2, split = FALSE) 
          rvnames <-listNames(rv)
          if (all(resnames %in% rvnames)) {
            # Transfer list elements
            for (i in strsplit(resnames,split = ".", fixed = TRUE)) {
              # set uploadsource to "RData" if something was uploaded in saved RData
              if(i[length(i)] == "uploadsource" && !is.null(res[[i]])) {
                set_uploadsource(rv = rv, m = i[1], uploadsource = "RData")
              } else {
                # if current element to-be-inserted is not "uploadsource",
                # --> proceed
                setValue(rv,i,res[[i]])
              }
            }
            # reset time_stamp with current
            # $$ToDo think if this is really desirable
            setValue(rv,c("General","time_stamp"), Sys.time())
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
        message("RDataImport: Legacy upload started")
        if ("Certification" %in% names(res) && !is.null(res$Certification)) {
          if (!silent) message("RDataImport_Server: Cert data transfered")
          setValue(rv,c("Certification","data"),res[["Certification"]][["data_input"]])
          setValue(rv,c("Certification","input_files"),res[["Certification"]][["input_files"]])
          # setValue(rv,c("Certification","uploadsource"),value = "RData")
          set_uploadsource(rv = rv, m = "Certification", uploadsource = "RData")
          # save
          setValue(rv,c("General","user"),res$Certification$user)
          setValue(rv,c("General","study_id"),res$Certification$study_id)
          # processing
          setValue(rv,c("Certification_processing","lab_means"), res[["Certification"]][["lab_means"]])
          setValue(rv,c("Certification_processing","cert_mean"),res[["Certification"]][["cert_mean"]])
          setValue(rv,c("Certification_processing","cert_sd"),res[["Certification"]][["cert_sd"]])
          setValue(rv,c("Certification_processing","normality_statement"),res[["Certification"]][["normality_statement"]])
          setValue(rv,c("Certification_processing","precision"),res[["Certification"]][["precision"]])

          setValue(rv,c("Certification_processing","data_kompakt"),res[["Certification"]][["data_kompakt"]])
          setValue(rv,c("Certification_processing","CertValPlot"),res[["Certification"]][["CertValPlot"]])
          setValue(rv,c("Certification_processing","stats"),res[["Certification"]][["stats"]])
          setValue(rv,c("Certification_processing","boxplot"),res[["Certification"]][["boxplot"]])
          setValue(rv,c("Certification_processing","opt"),res[["Certification"]][["opt"]])
          setValue(rv,c("Certification_processing","mstats"),res[["Certification"]][["mstats"]])
          # materialtabelle
          setValue(rv,"materialtabelle",res[["Certification"]][["cert_vals"]])
        }
        if ("Homogeneity" %in% names(res) && !is.null(res$Homogeneity)) {
          if (!silent) message("RDataImport_Server: Homog data transfered")
          setValue(rv,c("Homogeneity","data"),res[["Homogeneity"]][["h_dat"]])
          set_uploadsource(rv = rv, m = "Homogeneity", uploadsource = "RData")
          setValue(rv,c("Homogeneity","input_files"),res[["Homogeneity"]][["h_file"]])
          # Processing
          setValue(rv,c("Homogeneity","h_vals"),res[["Homogeneity"]][["h_vals"]])
          setValue(rv,c("Homogeneity","h_sel_analyt"),res[["Homogeneity"]][["h_sel_analyt"]])
          setValue(rv,c("Homogeneity","h_precision"),res[["Homogeneity"]][["h_precision"]])
          setValue(rv,c("Homogeneity","h_Fig_width"),res[["Homogeneity"]][["h_Fig_width"]])
        }
        if ("Stability" %in% names(res) && !is.null(res$Stability)) {
          if (!silent) message("RDataImport_Server: Stab data transfered")
          setValue(rv,c("Stability","input_files"),res[["Stability"]][["s_file"]])
          setValue(rv,c("Stability","data"),res[["Stability"]][["s_dat"]])
          set_uploadsource(rv = rv, m = "Stability", uploadsource = "RData")
          setValue(rv,c("Stability","s_vals"),res[["Stability"]][["s_vals"]])
        }
        setValue(rv,c("General","time_stamp"),Sys.time())
      }
      continue(NULL)
      rvreturn(rv)
      },ignoreNULL = TRUE)

    return(rvreturn)

  })

}

