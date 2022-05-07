#'@title fnc_load_RData.
#'
#'@description
#'\code{fnc_load_RData} will check and convert values in an RData file and prepare the internal reactive values list 'rv'.
#'
#'@details
#'tbd.
#'
#'@param x A list of values.
#'
#'@return
#'A reactive values list 'rv'.
#'
#'@keywords internal
#'
fnc_load_RData <- function(x = NULL) {

  silent <- get_golem_config("silent")

  shiny::isolate({
    rv <- reactiveClass$new(init_rv())
    if ("General.dataformat_version" %in% names(unlist(x, recursive = FALSE))) {
      # import functions for defined data_format schemes
      if (x$General$dataformat_version=="2021-05-27") {
        # Non-legacy upload #####
        if (!silent) message("RDataImport: Non-legacy upload started")
        # rv should contain all variables from uploaded x split must be
        # false here, otherwise one name list is of class character the other
        # of class list -> Error
        xnames <- listNames(l = x, maxDepth = 2, split = FALSE)
        rvnames <- listNames(rv)
        if (all(xnames %in% rvnames)) {
          # Transfer list elements
          for (i in strsplit(xnames, split = ".", fixed = TRUE)) {
            # set uploadsource to "RData" if something was uploaded in saved RData
            if(i[length(i)] == "uploadsource" && !is.null(x[[i]])) {
              set_uploadsource(rv = rv, m = i[1], uploadsource = "RData")
            } else {
              # if current element to-be-inserted is not "uploadsource",
              # --> proceed
              setValue(rv, i, x[[i]])
            }
          }
          # reset time_stamp with current
          # $$ToDo think if this is really desirable
          setValue(rv, c("General", "time_stamp"), Sys.time())
          if (!silent) message("RDataImport: Non-legacy upload finished")
        } else {
          #browser()
          allgivenexpected <- c(paste0("file: ", xnames), paste0("\nexpected: ", rvnames))
          found_table <- names(which(table(c(xnames, rvnames))==1))
          err <- allgivenexpected[c(xnames, rvnames) %in% found_table]
          if (!interactive()) {
            shinyalert::shinyalert(title = "m_RDataImport_Server", text = paste("The following components were inconsistent between loaded RData file and internal data structure:\n", paste(err, collapse=", ")), type = "warning")
          }
        }
      }
      # Legacy upload
    } else {
      if (!silent) message("[RDataImport]: Legacy upload started")
      if ("Certification" %in% names(x) && !is.null(x$Certification)) {
        if (!silent) message("RDataImport_Server: Cert data transfered")
        setValue(rv,c("Certification","data"),x[["Certification"]][["data_input"]])
        setValue(rv,c("Certification","input_files"),x[["Certification"]][["input_files"]])
        # setValue(rv,c("Certification","uploadsource"),value = "RData")
        set_uploadsource(rv = rv, m = "Certification", uploadsource = "RData")
        # save
        setValue(rv,c("General","user"),x$Certification$user)
        setValue(rv,c("General","study_id"),x$Certification$study_id)
        # processing
        setValue(rv,c("Certification_processing","lab_means"), x[["Certification"]][["lab_means"]])
        setValue(rv,c("Certification_processing","cert_mean"),x[["Certification"]][["cert_mean"]])
        setValue(rv,c("Certification_processing","cert_sd"),x[["Certification"]][["cert_sd"]])
        setValue(rv,c("Certification_processing","normality_statement"),x[["Certification"]][["normality_statement"]])
        setValue(rv,c("Certification_processing","precision"),x[["Certification"]][["precision"]])
        setValue(rv,c("Certification_processing","data_kompakt"),x[["Certification"]][["data_kompakt"]])
        setValue(rv,c("Certification_processing","CertValPlot"),x[["Certification"]][["CertValPlot"]])
        setValue(rv,c("Certification_processing","stats"),x[["Certification"]][["stats"]])
        setValue(rv,c("Certification_processing","boxplot"),x[["Certification"]][["boxplot"]])
        setValue(rv,c("Certification_processing","opt"),x[["Certification"]][["opt"]])
        setValue(rv,c("Certification_processing","mstats"),x[["Certification"]][["mstats"]])
        # materialtabelle
        mt <- x[["Certification"]][["cert_vals"]]
        setValue(rv,c("General","materialtabelle"), mt)
        # apm
        setValue(rv,c("General","apm"), init_apm(x[["Certification"]][["data_input"]]))
        apm <- getValue(rv,c("General","apm"))
        # ensure that analytes in apm and materialtabelle are in similar order
        if (!all(names(apm)==as.character(mt[,"analyte"]))) {
          setValue(rv,c("General","apm"), apm[sapply(as.character(mt[,"analyte"]), function(an) { which(names(apm)==an) })])
        }
      }
      if ("Homogeneity" %in% names(x) && !is.null(x$Homogeneity)) {
        if (!silent) message("RDataImport_Server: Homog data transfered")
        setValue(rv,c("Homogeneity","data"),x[["Homogeneity"]][["h_dat"]])
        set_uploadsource(rv = rv, m = "Homogeneity", uploadsource = "RData")
        setValue(rv,c("Homogeneity","input_files"),x[["Homogeneity"]][["h_file"]])
        # Processing
        setValue(rv,c("Homogeneity","h_vals"),x[["Homogeneity"]][["h_vals"]])
        setValue(rv,c("Homogeneity","h_sel_analyt"),x[["Homogeneity"]][["h_sel_analyt"]])
        setValue(rv,c("Homogeneity","h_precision"),x[["Homogeneity"]][["h_precision"]])
        setValue(rv,c("Homogeneity","h_Fig_width"),x[["Homogeneity"]][["h_Fig_width"]])
      }
      if ("Stability" %in% names(x) && !is.null(x$Stability)) {
        if (!silent) message("RDataImport_Server: Stab data transfered")
        setValue(rv,c("Stability","input_files"),x[["Stability"]][["s_file"]])
        setValue(rv,c("Stability","data"),x[["Stability"]][["s_dat"]])
        set_uploadsource(rv = rv, m = "Stability", uploadsource = "RData")
        setValue(rv,c("Stability","s_vals"),x[["Stability"]][["s_vals"]])
      }
      setValue(rv,c("General","time_stamp"),Sys.time())
      shinyalert::shinyalert(text = "This is an import from a previous data fromat. Please note that some additional parameters are available in the current version of eCerto which could not be restored from this RData file but are set to standard values (e.g. 'precision export' and 'pooling').", type = "info")
    }
  })

  return(rv)

}