#'@title fnc_get_local_file.
#'
#'@description \code{fnc_get_local_file} will provide a file path either from a
#'package system file (if available) or from www subdir (if online app).
#'
#'@param x Filename to search for without path but with extension.
#'@param copy_to_tempdir If TRUE file is copied to a tempdir location.
#'@param fsep The system path seperator.
#'  to temp dir and this temp file path is returned.
#'
#'@details Developing a shiny app as an R package in parallel leads to
#'difficulties on where to find specific files after installation (e.g. font
#'descriptions, markdown documents etc.). The function \code{fnc_get_local_file}
#'will check if a package 'of name pkg'ecerto' is installed and use system file to load.
#'If ecerto is not available it will search in the current wd.
#'
#'@examples
#'fnc_get_local_file(x="help_start.Rmd")
#'fnc_get_local_file(x="help_start.Rmd", copy_to_tempdir=FALSE)
#'
#'@return A file path (either of source or of a temp file depending on option 'copy_to_tempdir').
#'
#'@importFrom fs path_temp
#'
#'@export
#'
fnc_get_local_file <- function(x=NULL, copy_to_tempdir=TRUE, fsep=.Platform$file.sep) {
  # es gibt 3 relevante Ordner für den www folder der App
  # (1) dev version "inst/app/www"
  # (2) shiny.io --> "www"
  # (3) pkg --> package path 'www'
  www_dir <- "www"
  if (!file.exists(www_dir)) {
    www_dir <- "inst/app/www"
    if (!file.exists(www_dir)) {
      if ("eCerto" %in% rownames(utils::installed.packages())) {
        www_dir  <- system.file(package = "eCerto", lib.loc = .libPaths())
        file_path <- list.files(path=pkg_path, pattern = x, recursive = TRUE)[1]
        www_dir <- file.path(pkg_path, file_path, fsep=fsep)
      } else {
        www_dir <- NULL
      }
    }
  }
  #browser()
  stopifnot(!is.null(www_dir))
  if (nchar(www_dir)>=4) setwd(substr(www_dir,1,nchar(www_dir)-4))
  out <- list.files(pattern = x, recursive = TRUE)[1]
  stopifnot(length(out)>=1)
  out <- out[1]
  if (copy_to_tempdir) {
    tmp_file <- file.path(fs::path_temp(), x, fsep=fsep)
    file.copy(out, tmp_file, overwrite = TRUE)
    out <- tmp_file
  }
  if (out=="") message("Can't find file ", x)
  return(out)
}