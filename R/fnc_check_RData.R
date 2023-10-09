#' @title check_RData.
#' @description \code{check_RData} will check a file path if it specifies an
#'     RData file containing an object 'res'.
#' @details `eCerto` allows to store imported data and user specified parameter
#'     values in RData files for backup. The files can be re-imported to `eCerto`
#'     at later time points. At this point values need to be put into the correct
#'     slots of an `eCerto`object. To pre-check such a backup file is the purpose
#'     of this function.
#' @param x Character vector specifying a path to an RData file.
#' @return A object 'res' from an RData file.
#' @keywords internal
#' @noRd
check_RData <- function(x = NULL) {
  file.type <- tools::file_ext(x)
  shiny::validate(
    shiny::need(length(file.type) == 1, "Multiple files provided. Please select a single RData file containing an object 'res'."),
    shiny::need(tolower(file.type) %in% c("rdata","rda"),"File extension different from RData. Please select a single RData file containing an object 'res'.")
  )
  file.type <- "RData"
  load_envir <- new.env()
  tryCatch({
    load(x[1], envir = load_envir)
  }, error = function(e) {
    stop(shiny::safeError(e))
  })
  # check if 'res' is contained in loaded workspace
  out <- base::get0(x = "res", envir = load_envir, inherits = FALSE, ifnotfound = NULL)
  shiny::validate(
    shiny::need(!is.null(out), "No object of name 'res' found. Please select a single RData file containing an object 'res'."),
  )
  return(out)
}
