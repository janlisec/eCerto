#' @title fnc_check_RData.
#' @description \code{fnc_check_RData} will check a filepath if it specifieds an
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
read_zenodo <- function(id) {
  record <- id# <- "8380870"
  base_url <- "https://zenodo.org/api/records/"

  req <- curl::curl_fetch_memory(paste0(base_url, record))
  content <- jsonlite::fromJSON(rawToChar(req$content))

  file_urls <- content$files$links$self
  if (length(file_urls)>=2) {
    message("[read_zenodo] More than one file in this zenodo record, please select a unique id. Reading first file only.")
    file_urls <- file_urls[1]
  }

  filenames <- basename(file_urls)
  if (!tolower(tools::file_ext(filenames)) %in% c("rdata","rda")){
    message("[read_zenodo] Expecting RData or rda file type.")
  }
  dest <- fs::path(tempdir(), filenames)


  curl::curl_download(url = file_urls, destfile = dest, quiet = FALSE)

  x <- load(dest)

}