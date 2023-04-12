#' @title get_local_file.
#'
#' @description \code{get_local_file} will provide a full file path using the
#'    currently specified 'www' folder specified as a recourcePaths. This path
#'    can be different depending on whether the app is run as development
#'    version, using the library function or serving via shinyapps.io.
#'
#' @param x File pattern to search for.
#'
#' @details Developing a shiny app as an R package in parallel leads to difficulties
#'    on where to find specific files after installation (e.g. font descriptions,
#'    markdown documents etc.). The function \code{get_local_file} will search
#'    a specified pattern in the current recourcePaths for 'www'.
#'
#' @return A file path.
#'
#' @keywords internal
#' @noRd
get_local_file <- function(x = NULL) {
  x <- as.character(x)
  stopifnot(length(x)==1)
  if (is.na(shiny::resourcePaths()["www"])) {
    warning("[get_local_file] No shiny resourcePaths 'www' defined.")
    return(NA)
  } else {
    out <- list.files(path = shiny::resourcePaths()["www"], pattern = x, recursive = TRUE, full.names = TRUE)
    if (length(out)==1) {
      return(out)
    } else {
      if (length(out)>=2) {
        warning(paste0("[get_local_file] Several files match search string '", x, "', return first only."))
        return(out[1])
      } else {
        warning(paste("[get_local_file] Search string", x, "not found."))
        return(NA)
      }
    }
  }
}