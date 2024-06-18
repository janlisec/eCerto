#' @title list2rv.
#' @description \code{list2rv} will check and convert values from a list (e.g.
#'     as stored in an RData file) and prepare the internal reactive values list
#'     'rv'.
#' @details `eCerto` allows to store imported data and user specified parameter
#'     values in RData files for backup. The files can be re-imported to `eCerto`
#'     at later time points. At this point values need to be put into the correct
#'     slots of an `eCerto`object. To achieve this and test for potential
#'     inconsistencies is the purpose of this function.
#' @param x A list of values as stored e.g. in an RData file for backup.
#' @return  A reactive values list 'rv'.
#' @keywords internal
#' @noRd
list2rv <- function(x = NULL) {

  # extract list elements of x by name
  xnames <- listNames(l = x, maxDepth = 2, split = FALSE)

  shiny::isolate({
    # set up new R6 object (current version of the App) and extract the names of object elements
    rv <- eCerto$new(init_rv())
    rvnames <- listNames(rv)
    if ("General.dataformat_version" %in% xnames) {
      # import functions for defined data_format schemes
      if (x$General$dataformat_version == "2021-05-27") {
        # Non-legacy upload #####
        e_msg("Non-legacy upload started")
        # rv should contain all variables from uploaded x except for deprecated once
        # split must be false here, otherwise one name list is of class character
        # the other of class list -> Error

        # remove deprecated elements from 'x'
        deprecated_elements <- c(
          "Certification_processing.opt",
          "Certification_processing.data_kompakt",
          "Certification_processing.lab_means",
          "Certification_processing.normality_statement",
          "Certification_processing.precision",
          "Certification_processing.boxplot",
          "Homogeneity.h_precision",
          "Certification.uploadsource",
          "Homogeneity.uploadsource",
          "Stability.uploadsource"
        )
        if (any(xnames %in% deprecated_elements)) {
          xnames <- xnames[!xnames %in% deprecated_elements]
        }
        if (!all(xnames %in% rvnames)) {
          allgivenexpected <- c(paste0("\nfile: ", xnames), paste0("\nexpected: ", rvnames))
          found_table <- names(which(table(c(xnames, rvnames)) == 1))
          err <- allgivenexpected[c(xnames, rvnames) %in% found_table]
          err_msg <- paste0(
            "The following components were inconsistent between loaded RData [file]\n",
            "and current internal data structure [expected]:",
            paste(err, collapse = ", ")
          )
          if (!is.null(shiny::getDefaultReactiveDomain())) {
            shinyWidgets::show_alert(title = "m_RDataImport_Server", text = err_msg, type = "warning")
          } else {
            warning(err_msg)
          }
          # remove items which are not present in stored RData file
          # these are most likely replaced by an internal function of the R6 object
          # as it was e.g. implemented for 'lab_means' on 28.06.2022
          xnames <- xnames[xnames %in% rvnames]
        }
        for (i in strsplit(xnames, split = ".", fixed = TRUE)) {
          setValue(rv, i, x[[i]])
        }
        # reset time_stamp with current
        # $$ToDo think if this is really desirable
        setValue(rv, c("General", "time_stamp"), Sys.time())
        e_msg("RDataImport: Non-legacy upload finished")
      }
    } else {
      # Legacy upload
      e_msg("Legacy upload started")
      if ("Certification" %in% names(x) && !is.null(x$Certification)) {
        e_msg("Certification data transfered")
        setValue(rv, c("Certification", "data"), x[["Certification"]][["data_input"]])
        setValue(rv, c("Certification", "input_files"), x[["Certification"]][["input_files"]])
        # save
        setValue(rv, c("General", "user"), x[["Certification"]][["user"]])
        setValue(rv, c("General", "study_id"), x[["Certification"]][["study_id"]])
        # processing
        setValue(rv, c("Certification_processing", "cert_mean"), x[["Certification"]][["cert_mean"]])
        setValue(rv, c("Certification_processing", "cert_sd"), x[["Certification"]][["cert_sd"]])
        setValue(rv, c("Certification_processing", "CertValPlot"), x[["Certification"]][["CertValPlot"]])
        setValue(rv, c("Certification_processing", "stats"), x[["Certification"]][["stats"]])
        setValue(rv, c("Certification_processing", "mstats"), x[["Certification"]][["mstats"]])
        # materialtabelle
        mt <- x[["Certification"]][["cert_vals"]]
        setValue(rv, c("General", "materialtabelle"), mt)
        # apm (this is a new object in eCerto data structure)
        apm <- init_apm(x[["Certification"]][["data_input"]])
        # ensure that analytes in apm and materialtabelle are in similar order
        if (!all(names(apm) == as.character(mt[, "analyte"]))) {
          consistent_order <- sapply(as.character(mt[, "analyte"]), function(an) {
            which(names(apm) == an)
          })
          apm <- apm[consistent_order]
        }
        setValue(rv, c("General", "apm"), apm)
      }
      if ("Homogeneity" %in% names(x) && !is.null(x$Homogeneity)) {
        e_msg("Homogeneity data transfered")
        setValue(rv, c("Homogeneity", "data"), x[["Homogeneity"]][["h_dat"]])
        setValue(rv, c("Homogeneity", "input_files"), x[["Homogeneity"]][["h_file"]])
        # Processing
        setValue(rv, c("Homogeneity", "h_vals"), x[["Homogeneity"]][["h_vals"]])
        setValue(rv, c("Homogeneity", "h_sel_analyt"), x[["Homogeneity"]][["h_sel_analyt"]])
        setValue(rv, c("Homogeneity", "h_Fig_width"), x[["Homogeneity"]][["h_Fig_width"]])
      }
      if ("Stability" %in% names(x) && !is.null(x$Stability)) {
        e_msg("Stability data transfered")
        setValue(rv, c("Stability", "input_files"), x[["Stability"]][["s_file"]])
        setValue(rv, c("Stability", "data"), x[["Stability"]][["s_dat"]])
        setValue(rv, c("Stability", "s_vals"), x[["Stability"]][["s_vals"]])
      }
      setValue(rv, c("General", "time_stamp"), Sys.time())
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        shinyWidgets::show_alert(
          title = NULL,
          text = paste(
            "This is an import from a previous data format.",
            "Please note that some additional parameters are available",
            "in the current version of eCerto which could not be restored",
            "from this RData file but are set to standard values",
            "(e.g. 'precision export' and 'pooling')."
          ),
          type = "info"
        )
      }
    }
  })

  return(rv)
}
