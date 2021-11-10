#' Initializes material table for materialtabelle module
#'
#' @param analytes Character vector of analyte names.
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
    "cert_val" = NA,
    "sd" = NA,
    "n" = NA,
    "char" = 0,
    "com" = NA,
    "k" = 2,
    "U" = NA
  )
  attr(c, "col_code") <-
    data.frame("ID" = "", "Name" = "", stringsAsFactors = FALSE)[-1,,drop=FALSE]
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
init_rv <- function() {
  rv <- list(
    "modules" = c("Certification","Homogeneity","Stability"), # names of the modules
    "General" = shiny::reactiveValues(
      # save
      "user" = NULL,
      "study_id" = NULL,
      "time_stamp" = as.Date.POSIXct(0),
      "dataformat_version" = "2021-05-27",
      # analyte specific parameters
      "apm" = NULL,
      # materialtabelle
      "materialtabelle" = NULL
    ),
    # data input
    "Certification" = shiny::reactiveValues(
      "input_files" = NULL,
      "uploadsource" = NULL,
      "data" = NULL
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
      "mstats" = list(
        "show" = NULL,
        "data" = NULL
      )
    ),
    "Homogeneity" = shiny::reactiveValues(
      # upload
      "input_files" = NULL,
      "uploadsource" = NULL,
      "data" = NULL, # formerly h_dat
      # Processing
      "h_vals" = NULL,
      "h_sel_analyt" = NULL,
      "h_precision" = NULL,
      "h_Fig_width" = NULL
    ),
    "Stability" = shiny::reactiveValues(
      "input_files" = NULL,
      "uploadsource" = NULL,
      "data" = NULL,
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
#' @param x data frame of analyte names and sample IDs
#'
#' @return A empty list (if no data frame is provided) or a apm-List otherwise.
#'
#' @export
init_apm <- function(x = data.frame("ID"=1:20, "analyte"=gl(n = 2, k = 10, labels = c("A1","A2")), "Lab"=rep(rep(c("L1","L2"),each=5),2))) {
  templ <- list(
    "name" = NULL,
    "sample_ids" = NULL, # which samples are available for the filter
    "sample_filter" = NULL, # saving which samples where selected for filter
    "lab_ids" = NULL, # which labs have mesasured this analyte
    "lab_filter" = NULL, # filter of laboratories (e.g. L1)
    "confirmed" = FALSE, # has the analyte manually been confirmed?
    "pooling" = FALSE, # s pooling allowed for this analyte
    "precision" = 4, # rounding precision for imported data
    "precision_export" = 4 # rounding precision for export
  )
  if (!is.null(x) && is.data.frame(x) && all(c("ID","analyte","Lab") %in% colnames(x))) {
    if (!is.factor(x[, "analyte"])) {
      x[,"analyte"] <- factor(x[,"analyte"], levels=unique(x[,"analyte"]))
    }
    # create list with lists of all analytes (i.e. a nested list)
    apm <- sapply(levels(x[, "analyte"]), function(an) {
      out <- templ
      out$name <- an
      out$sample_ids <- x[as.character(x[,"analyte"])==an,"ID"]
      y <- x[as.character(x[,"analyte"])==an,,drop=FALSE]
      out$lab_ids <- unique(as.character(y[,"Lab"]))
      if ("S_flt" %in% colnames(y) && any(y[,"S_flt"])) out$sample_filter <- y[which(y[,"S_flt"]),"ID"]
      if ("L_flt" %in% colnames(y) && any(y[,"L_flt"])) out$lab_filter <- unique(as.character(y[which(y[,"L_flt"]),"Lab"]))
      return(out)
    }, simplify = FALSE)
    return(apm)
  } else {
    return(list())
  }
}
