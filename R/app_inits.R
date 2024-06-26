#' @title init_materialtabelle.
#' @description Initializes material table for `m_materialtabelle`.
#' @param analytes Character vector of analyte names.
#' @return a data frame
#' @keywords internal
#' @noRd
#' @examples matTab <- eCerto:::init_materialtabelle(analytes = c("Si", "Ar"))
init_materialtabelle <- function(analytes) {
  mt <- data.frame(
    "analyte" = analytes,
    "mean" = NA,
    "cert_val" = NA,
    "sd" = NA,
    "n" = NA,
    "u_char" = 0,
    "u_com" = NA,
    "k" = 2,
    "U" = NA,
    "U_abs" = NA,
    "unit" = "U"
  )
  attr(mt, "col_code") <- data.frame(
    "ID" = character(),
    "Name" = character(),
    stringsAsFactors = FALSE
  )
  return(mt)
}

#' @title init_rv
#' @description \code{init_rv} initializes the main reactive value (rv) to store
#'   the results from all modules. It therefore gets handed over multiple times.
#'   In further programming. It is now part of the eCerto class R6 object.
#'   Note: If other modules besides Certification, Homogeneity and Stability added,
#'   adapt the modules list.
#' @return A list of different reactiveValues sublists to be stored in the eCerto R6 class.
#' @keywords internal
#' @noRd
#' @example rv <- eCerto:::init_rv()
init_rv <- function() {
  list(
    # "modules" = c("Certification", "Homogeneity", "Stability"), # names of the modules
    "modules" = c("Homogeneity", "Stability", "Certification"), # names of the modules
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
      "data" = NULL
    ),
    # processing
    "Certification_processing" = shiny::reactiveValues(
      "cert_mean" = NULL,
      "cert_sd" = NULL,
      "CertValPlot" = list(
        "show" = NULL,
        "fnc" = NULL,
        "call" = NULL,
        "Fig01_width" = NULL,
        "Fig01_height" = NULL
      ),
      "stats" = NULL,
      "mstats" = list(
        "show" = NULL,
        "data" = NULL
      )
    ),
    "Homogeneity" = shiny::reactiveValues(
      # upload
      "input_files" = NULL,
      "data" = NULL,
      # Processing
      "h_vals" = NULL,
      "h_sel_analyt" = NULL,
      "h_Fig_width" = NULL
    ),
    "Stability" = shiny::reactiveValues(
      "input_files" = NULL,
      "data" = NULL,
      "s_vals" = NULL
    )
  )
}

#' @title init_apm
#' @description \code{init_apm} creates for each analyte the
#'   parameter list. Each sublist contains information about the selected analyte
#'   tab, and for each analyte the specified precision, the filtered sample id,
#'   which sample ids are available to be filtered at all and, for completion,
#'   the analyte name in case the list name fails.
#' @param x Analyte data frame containing at least columns `ID`, `analyte` and `Lab`.
#' @return The analyte parameter list (apm) including all individually settable options.
#' @keywords internal
#' @noRd
#' @example apm <- eCerto:::init_apm()
init_apm <- function(x) {
  if (missing(x)) {
    # default example data
    x <- data.frame(
      "ID" = 1:20,
      "analyte" = gl(n = 2, k = 10, labels = c("A1", "A2")),
      "Lab" = rep(rep(c("L1", "L2"), each = 5), 2)
    )
  } else {
    # check x
    stopifnot("'init_apm(x)' requires a data.frame as input" = is.data.frame(x))
    stopifnot("'init_apm(x)' is missing column 'ID' in data.frame 'x'" = "ID" %in% colnames(x))
    stopifnot("'init_apm(x)' is missing column 'analyte' in data.frame 'x'" = "analyte" %in% colnames(x))
    stopifnot("'init_apm(x)' is missing column 'Lab' in data.frame 'x'" = "Lab" %in% colnames(x))
    if (!is.factor(x[, "analyte"])) {
      x[, "analyte"] <- factor(x[, "analyte"], levels = unique(x[, "analyte"]))
    }
  }
  # the output template used for every analyte
  templ <- list(
    "name" = NULL,
    "sample_ids" = NULL, # which samples are available for the filter
    "sample_filter" = NULL, # saving which samples where selected for filter
    "lab_ids" = NULL, # which labs have measured this analyte
    "lab_filter" = NULL, # filter of laboratories (e.g. L1)
    "confirmed" = FALSE, # has the analyte manually been confirmed?
    "pooling" = FALSE, # s pooling allowed for this analyte
    "precision" = 4, # rounding precision for displayed values
    "precision_export" = 4, # rounding precision for certified value and uncertainty
    "unit" = "U" # unit this analyte is measured in
  )
  # create list with lists of all analytes (i.e. a nested list)
  apm <- sapply(levels(x[, "analyte"]), function(an) {
    out <- templ
    out$name <- an
    out$sample_ids <- x[as.character(x[, "analyte"]) == an, "ID"]
    y <- x[as.character(x[, "analyte"]) == an, , drop = FALSE]
    out$lab_ids <- unique(as.character(y[, "Lab"]))
    if ("S_flt" %in% colnames(y) && any(y[, "S_flt"])) out$sample_filter <- y[which(y[, "S_flt"]), "ID"]
    if ("L_flt" %in% colnames(y) && any(y[, "L_flt"])) out$lab_filter <- unique(as.character(y[which(y[, "L_flt"]), "Lab"]))
    if ("unit" %in% colnames(y)) out[["unit"]] <- as.character(unique(y[, "unit"])[1])
    # try to make an initial guess regarding the desired rounding according to DIN1333
    n <- try(digits_DIN1333(2 * stats::sd(sapply(split(y[, "value"], y[, "Lab"]), mean)) / sqrt(length(unique(y[, "Lab"])))), silent = TRUE)
    if (!inherits(n, "try-error") && is.finite(n)) {
      out$precision_export <- n
      out$precision <- n + 1
    }
    return(out)
  }, simplify = FALSE)
  return(apm)
}
