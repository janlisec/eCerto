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
      "s_vals" = NULL,
      "s_pars" = NULL
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
    out[["name"]] <- an
    out[["sample_ids"]] <- x[as.character(x[, "analyte"]) == an, "ID"]
    y <- x[as.character(x[, "analyte"]) == an, , drop = FALSE]
    out$lab_ids <- unique(as.character(y[, "Lab"]))
    if ("S_flt" %in% colnames(y) && any(y[, "S_flt"])) out[["sample_filter"]] <- y[which(y[, "S_flt"]), "ID"]
    if ("L_flt" %in% colnames(y) && any(y[, "L_flt"])) out[["lab_filter"]] <- unique(as.character(y[which(y[, "L_flt"]), "Lab"]))
    if ("unit" %in% colnames(y)) out[["unit"]] <- as.character(unique(y[, "unit"])[1])
    # try to make an initial guess regarding the desired rounding according to DIN1333
    n <- try(digits_DIN1333(2 * stats::sd(sapply(split(y[, "value"], y[, "Lab"]), mean)) / sqrt(length(unique(y[, "Lab"])))), silent = TRUE)
    if (!inherits(n, "try-error") && is.finite(n)) {
      out[["precision_export"]] <- n
      # limit the allowed rounding precision for tables similar to what shiny accepts through user input
      out[["precision"]] <- min(max(n + 1, 0), 6)
    }
    return(out)
  }, simplify = FALSE)
  return(apm)
}

#' @title init_V2_data
#' @param n_p Number of participating Labs.
#' @param n_q Number of tested Levels
#' @param n_k Number of Replicates per Level (can be a range).
#' @param seed seed. defaults to 5725.
#' @param digits Measurement precision (can be a range).
#' @param mn Vector of lab means to use.
#' @param sr Expected sd within Lab.
#' @param sL Expected bias.
#' @return A data frame containing example data for V2 module.
#' @keywords internal
#' @noRd
#' @example init_V2_data()
init_V2_data <- function(n_p = 12, n_q = 2, n_k = 3:5, seed = 5725, digits = 3:4, mn = c(10, 50), sr = c(0.25, 1), sL = c(0.3, 1.5)) {
  set.seed(seed)

  labs  <- paste0("Lab", sprintf("%02d", 1:n_p))
  levels <- paste0("Lev", sprintf("%02d", 1:n_q))

  # means per Level
  mn <- setNames(mn, levels)

  # sd (within Lab)
  sr <- setNames(sr, levels)

  # bias (between Labs)
  sL <- setNames(sL, levels)

  out <- do.call(
    rbind,
    lapply(levels, function(LV) {
      lab_bias <- rnorm(length(labs), mean = 0, sd = sL[LV])
      names(lab_bias) <- labs

      do.call(rbind, lapply(labs, function(LB) {
        n_rep_lab <- sample(n_k, size = 1)
        data.frame(
          "Lab" = LB,
          "Level" = LV,
          "Replicate" = 1:n_rep_lab,
          "Value" = round(rnorm(
            n_rep_lab,
            mean = mn[LV] + lab_bias[LB],
            sd = sr[LV]
          ), digits = sample(digits, 1))
        )
      }))
    })
  )

  out <- cbind(out, "ID"=1:nrow(out), "Filter"="")
  out[out[,"Lab"]=="Lab05" & out[,"Level"]=="Lev02", "Filter"] <- "removed Lab05 Lev02"
  out[out[,"ID"]==15, "Filter"] <- "removed ID 15"

  return(out)
}
