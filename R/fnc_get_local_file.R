#'@title fnc_get_local_file.
#'
#'@description \code{fnc_get_local_file} will provide a file path either from a
#'package system file (if available) or from www subdir (if online app).
#'
#'@param x Filename to search for without path but with extension.
#'@param copy_to_tempdir If FALSE, file path is returned, if TRUE file is copied
#'  to temp dir and this temp file path is returned.
#'
#'@details Developing a shiny app as an R package in parallel leads to
#'difficulties on where to find specific files after installation (e.g. font
#'descriptions, markdown documents etc.). The function \code{fnc_get_local_file}
#'will check if a package of name pkg is installed and use system file to load,
#'assuming 'rmd' as the subdirectory to search in. If pgk is not available it
#'will provide a filepath using 'www' as the subdirectory for the shiny app
#'version.
#'
#'@examples
#'fnc_get_local_file(x="help_start.Rmd")
#'fnc_get_local_file(x="help_start.Rmd", copy_to_tempdir=FALSE)
#'
#'@return A file path.
#'
#'@export
#'
fnc_get_local_file <- function(x=NULL, copy_to_tempdir=TRUE) {
  if ("ecerto" %in% rownames(utils::installed.packages())) {
    # installed with package
    pkg_path <- system.file(package = "ecerto")
    file_path <- list.files(path=pkg_path, pattern = x, recursive = TRUE)[1]
    out <- file.path(pkg_path, file_path, fsep="\\")
  } else {
    # as available in ShinyApp
    out <- list.files(pattern = x, recursive = TRUE)
    #out <- paste("www", x, sep="/")
  }
  if (copy_to_tempdir) {
    tmp_file <- file.path(tempdir(), x, fsep="\\")
    file.copy(out, tmp_file, overwrite = TRUE)
    out <- tmp_file
  }
  if (out=="") message("Can't find file ", x)
  return(out)
}