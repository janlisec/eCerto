#' @title init_datreturn.
#'
#' @description \code{init_datreturn} Initiates a reactiveValues object used for
#' session-only and module-to-materialtabelle content
#'
#' @return datreturn
#' @export
#'
init_datreturn <- function() {
  shiny::reactiveValues(
    selectedAnalyteDataframe = NULL,    # The dataframe corresp. to the selected analyte
    h_vals = NULL,                      # values from Homogeneity-module materialtabelle via .TransferHomogeneity
    mater_table = NULL,                 # material table, formerly 'cert_vals'
    transfer = NULL,                    # when transfer was clicked (just to change the panel to Certification)
    lab_statistics = NULL,               # lab statistics (mean,sd) for materialtabelle
    cert_mean = NULL,
    cert_sd = NULL,
    current_apm = NULL                 # currently selected apm
    )
}

#' Initializes material table for materialtabelle module
#'
#' @param analytes the available analytes
#'
#' @return a data frame
#' @export
#'
#' @examples
#' analytes = c("Si","Ar")
#' matTab = init_materialTabelle(analytes)
init_materialTabelle <- function(analytes) {
  c = data.frame(
    "analyte" =  analytes,
    "mean" = NA,
    "F1" = 1,
    "F2" = 1,
    "F3" = 1,
    "cert_val" = NA,
    "sd" = NA,
    "n" = NA,
    "char" = 0,
    "U2" = 0,
    "U3" = 0,
    "U4" = 0,
    "U5" = 0,
    "U6" = 0,
    "U7" = 0,
    "com" = NA,
    "k" = 2,
    "U" = NA
  )
  attr(c, "col_code") <-
    data.frame(
      "ID" = c(paste0("F", 1:3), paste0("U", 2:7)),
      "Name" = c(paste0("F", 1:3), paste0("U", 2:7)),
      stringsAsFactors = FALSE
    )
  return(c)
}

#' Initialize permanent reactive values
#'
#' @description \code{init_rv} initializes the main reactive value (rv) to store
#'   the results from all modules. It therefore gets handed over multiple times.
#'   In further programming, it should be considered to be replaced by a class
#'   in OOP style. Note: If other modules besides Certification, Homogeneity and
#'   Stability added, adapt the modules list
#'
#' @return a reactiveValues
#' @export
#'
#' @examples rv = init_rv()
init_rv = function() {
  rv <-
    list(
      "modules" = c("Certification","Homogeneity","Stability"), # names of the modules
      "General" = shiny::reactiveValues(
        # save
        "user" = NULL,
        "study_id" = NULL,
        "time_stamp" = as.Date.POSIXct(0),
        "dataformat_version" = "2021-05-27",
        # filter
        "apm" = NULL
      ),
      # materialtabelle
      "materialtabelle" = NULL,
      # data input
      "Certification" = shiny::reactiveValues(
        "data" = NULL,
        "input_files" = NULL,
        "uploadsource" = NULL
      ),
      # processing
      "Certification_processing" = shiny::reactiveValues(
        "lab_means" = NULL,
        "cert_mean" = NULL,
        "cert_sd" = NULL,
        "normality_statement" = NULL,
        "precision" = NULL,
        "data_kompakt" = NULL,
        "CertValPlot" = list(
          "show" = NULL,
          "fnc" = NULL,
          "call" = NULL,
          "Fig01_width" = NULL,
          "Fig01_height" = NULL
        ),
        "stats" = NULL,
        "boxplot" = NULL,
        "opt" = NULL,
        "mstats" = NULL
      ),
      "Homogeneity" = shiny::reactiveValues(
        # upload
        "data" = NULL, # formerly h_dat
        "uploadsource" = NULL,
        "input_files" = NULL,
        # Processing
        "h_vals" = NULL,
        "h_sel_analyt" = NULL,
        "h_precision" = NULL,
        "h_Fig_width" = NULL
      ),
      "Stability" = shiny::reactiveValues(
        "input_files" = NULL,
        "data" = NULL,
        "uploadsource" = NULL,
        "s_vals" = NULL
      )
    )

}

#' Analyte Parameter List (apm)
#'
#' @description \code{init_apm} creates for each analyte the
#'   parameter list. Each sublist contains information about the selected analyte
#'   tab, and for each analyte the specified precision, the filtered sample id,
#'   which sample ids are available to be filtered at all and, for completion,
#'   the analyte name in case the list name fails
#'
#' @param certification data frame of certification data
#'
#' @return reactiveValues
#'
#' @export
#'
#' @examples
#' apm <- shiny::reactiveVal()
#' df <- data.frame("analyte"=gl(n = 2, k = 10, labels = c("A1","A2")))
#' apm(init_apm(df))
#' apm_tmp <- shiny::isolate(apm())
#' apm_tmp[["A1"]]$confirmed = TRUE
#' apm(apm_tmp)
#' shiny::isolate(apm()[["A1"]]$confirmed) # TRUE
#'
init_apm = function(certification = NULL) {
  if(!is.null(certification)){
    stopifnot(is.factor(certification[, "analyte"]))
  }
  param_template = list(
    "precision" = NULL,
    "sample_filter" = NULL, # saving which samples where selected for filter
    "sample_ids" = NULL, # which samples are available for the filter
    "lab_filter" = NULL, # filter of laboratories (e.g. L1)
    "analytename" = NULL,
    "confirmed" = FALSE # has the analyte manually been confirmed?
    
  )
  # l = list()

  analytes = levels(certification[, "analyte"])

  # create list with lists of all analytes (i.e. a nested list)
  a_param_list = rep(list(param_template), length(analytes))
  if(!is.null(certification)){
    for (i in 1:length(a_param_list)) {
      # add analyte name to list
      a_param_list[[i]]$analytename = as.list(analytes)[[i]]
      # add available id's of samples to list
      tmp = certification
      ids = tmp[tmp[["analyte"]] == as.list(analytes)[[i]], "ID"]
      a_param_list[[i]]$sample_ids = ids[!is.na(ids)] # fill available ids
    }
  }
  # set names of sublists to analyte names
  a_param_list = stats::setNames(a_param_list, analytes)
  # end param list
  return(a_param_list)
}
