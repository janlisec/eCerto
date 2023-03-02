#' @title c_filter_data.
#' @description \code{c_filter_data} will limit imported data from 'rv' for a specified analyte
#'  according to parameters specified in 'apm'.
#' @details Filtered samples will be removed while filtered Labs will be retained but set to TRUE in column 'L_flt'.
#' @param x The imported certification data from an session R6 object.
#' @param c_apm The parameters of the current analyte from an session R6 object.
#' @examples
#' x <- shiny::isolate(eCerto::getValue(eCerto:::test_rv(), c("Certification", "data")))
#' c_apm <- list("name"="Si", "sample_filter"=NULL, "lab_filter"=NULL, "precision"=2)
#' eCerto:::c_filter_data(x = x, c_apm = c_apm)
#' c_apm[["lab_filter"]] <- "L1"
#' eCerto:::c_filter_data(x = x, c_apm = c_apm)
#' c_apm[["sample_filter"]] <- c(4,19)
#' eCerto:::c_filter_data(x = x, c_apm = c_apm)
#' @return A data frame.
#' @keywords internal
#' @noRd
c_filter_data <- function(x, c_apm) {
  if (!get_golem_config("silent")) message("[c_filter_data] filter certification dataset for analyte ", c_apm$name)
  if (c_apm$name %in% x[, "analyte"]) {
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