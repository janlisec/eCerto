#'@title c_filter_data.
#'
#'@description
#'\code{c_filter_data} will limit imported data from 'rv' for a specified analyte
#'  according to parameters specified in 'apm'.
#'
#'@details
#'tbd.
#'
#'@param x The Cert data from an session R6 object.
#'@param c_apm The parameters of the current analyte from an session R6 object.
#'
#'@examples
#'rv <- eCerto:::test_rv()
#'shiny::isolate(c_filter_data(
#'  x = getValue(rv, c("Certification","data")),
#'  c_apm = getValue(rv, c("General","apm"))[["Si"]]
#'))
#'
#'@return
#'A filtered data frame (S_flt, L_flt) without missing values and rounded to a specified precision.
#'
#'@export
#'
c_filter_data <- function(x, c_apm) {
  message("[c_filter_data] filter certification dataset for analyte ", c_apm$name)
  if (c_apm$name %in% x[, "analyte"]) {
    # REMINDER! Do not round numbers before calculations but only round the result
    #x[, "value"] <- round(x[, "value"], c_apm[["precision"]])
    x <- x[x[, "analyte"] %in% c_apm$name, ]
    x <- x[!(x[, "ID"] %in% c_apm[["sample_filter"]]), ]
    x[, "L_flt"] <- x[, "Lab"] %in% c_apm[["lab_filter"]]
    # adjust factor levels
    x[, "Lab"] <- factor(x[, "Lab"])
    # Notify User in case that only 1 finite measurement remained within group
    shiny::validate(
      shiny::need(
        all(sapply(split(x[, "value"], x[, "Lab"]), length) >= 2),
        message = paste(names(which(
          sapply(split(x[, "value"], x[, "Lab"]), length) < 2
        ))[1], "has less than 2 replicates left. Please remove an Sample-ID filter.")),
      shiny::need(
        is.numeric(c_apm[["precision"]]) && c_apm[["precision"]] >= 0 && c_apm[["precision"]] <= 6,
        message = "please check precision value: should be numeric and between 0 and 6"
      )
    )
    return(x)
  } else {
    return(NULL)
  }
}