#' @title read_zenodo.
#' @description \code{read_zenodo} will read (download to temp folder first) an
#'     RData file containing an object 'res' from a zenodo DOI.
#' @details `eCerto` allows to store imported data and user specified parameter
#'     values in RData files for backup. The files can be re-imported to `eCerto`
#'     at later time points. At this point values need to be put into the correct
#'     slots of an `eCerto`object. Such files can be stored in zenodo and imported
#'     via DOI.
#' @param id A zenodo DOI.
#' @return A object 'res' from an RData file.
#' @examples
#' x <- eCerto:::read_zenodo(id = "8380870")
#'
#' @keywords internal
#' @noRd
read_zenodo <- function(id) {

  # check for suggested packages being present
  verify_suggested(c("curl", "jsonlite", "fs"))

  # get content for zenodo record
  base_url <- "https://zenodo.org/api/records/"
  zen_record <- curl::curl_fetch_memory(paste0(base_url, id))
  content <- jsonlite::fromJSON(rawToChar(zen_record$content))

  # get and check url
  file_urls <- content$files$links$self
  if (length(file_urls)>=2) {
    message("[read_zenodo] More than one file in this zenodo record, please select a unique id. Reading first file only.")
    file_urls <- file_urls[1]
  }

  # download from url to temp file and load in R session
  dest <- fs::path(tempdir(), basename(file_urls))
  curl::curl_download(url = file_urls, destfile = dest, quiet = FALSE)
  return(check_RData(x = dest))

}