#'@title fnc_filter_data.
#'
#'@description
#'\code{fnc_filter_data} will check parameters in 'apm' and limit imported data from 'rv' for a specified analyte.
#'
#'@details
#'tbd.
#'
#'@param rv The session R6 object.
#'@param an The name of the current analyte.
#'
#'@examples
#'rv <- reactiveClass$new(init_rv())
#'shiny::isolate({
#'eCerto::setValue(rv, c("Certification","data"),
#'  eCerto:::test_certification()[["data"]])
#'eCerto::setValue(rv, c("General","apm"),
#'  eCerto::init_apm(eCerto:::test_certification()[["data"]]))
#'fnc_filter_data(rv = rv, an="Si")
#'})
#'
#'@return
#'A filtered data frame (S_flt, L_flt) without missing values.
#'
#'@export
#'
fnc_filter_data <- function(rv, an) {
  message("[fnc_filter_data] filter dataset for analyte ", an)
  #browser()
  #shiny::isolate(
  x <- getValue(rv, c("Certification","data")) # take the uploaded certification
  c_apm <- getValue(rv, c("General","apm"))[[an]]
  stopifnot(an %in% x[, "analyte"])
  x[, "value"] <- round(x[, "value"], c_apm[["precision"]])
  x <- x[x[, "analyte"] %in% an, ]
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
}