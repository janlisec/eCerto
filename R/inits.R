#' Initiate 'datreturn', which holds data returned from other modules meant for
#' temporary use
#'
#' @return datreturn
#'
#' @examples
init_datreturn = function() {
  reactiveValues(
    selectedAnalyteDataframe = NULL,    # The dataframe corresp. to the selected analyte
    h_vals = NULL,                      # values from Homogeneity-module for .TransferHomogeneity
    mater_table = NULL,                 # *READ-ONLY* material table, formerly 'cert_vals'
    t_H = NULL,                         # when Homogeneity is transferred
    lab_statistics = NULL               # lab statistics (mean,sd) for materialtabelle
  )
}

init_materialTabelle <- function(analytes) {
  c = data.frame(
    "analyte" =  analytes, # a,
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

init_rv = function() {
  rv <- do.call(
    "reactiveValues",
    list(
      "Certifications" = list(
        # upload
        "data" = NULL,
        "input_files" = NULL,
        "uploadsource" = NULL,
        # save
        "user" = NULL,
        "study_id" = NULL,
        "time_stamp" = Sys.time(),
        "dataformat_version" = "2021-05-27",
        # processing
        "lab_means" = NULL,
        "cert_mean" = NULL,
        "cert_sd" = NULL,
        "normality_statement" = NULL,
        "precision" = NULL,
        "data_kompakt" = NULL,
        "CertValPlot" = NULL,
        "stats" = NULL,
        "boxplot" = NULL,
        "opt" = NULL,
        "mstats" = NULL,
        # materialtabelle
        "materialtabelle" = NULL
      ),
      "Homogeneity" = list(
        # upload
        "data" = NULL, # formerly h_dat
        "uploadsource" = NULL,
        "h_file" = NULL,
        # Processing
        "h_vals" = NULL,
        "h_sel_analyt" = NULL,
        "h_precision" = NULL,
        "h_Fig_width" = NULL
      ),
      "Stability" = list(
        "s_file" = NULL,
        "data" = NULL,
        "uploadsource" = NULL,
        "s_vals" = NULL
      )
    )
  )
  return(rv)
}