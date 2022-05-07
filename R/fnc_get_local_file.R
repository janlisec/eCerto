#'@title get_local_file.
#'
#'@description \code{get_local_file} will provide a full file path using the
#'    currently specified 'www' folder specified as a recourcePaths.
#'
#'@param x File pattern to search for.
#'
#'@details Developing a shiny app as an R package in parallel leads to difficulties
#'    on where to find specific files after installation (e.g. font descriptions,
#'    markdown documents etc.). The function \code{get_local_file} will search
#'    a specfied pattern in the current recourcePaths for 'www'.
#'
#'@return A file path.
#'
#'@keywords internal
#'
get_local_file <- function(x=NULL) {
  return(list.files(path = shiny::resourcePaths()["www"], pattern = x, recursive = TRUE, full.names = TRUE)[1])
}