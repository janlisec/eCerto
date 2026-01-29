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
#' @examplesIf interactive() && curl::has_internet()
#' x <- eCerto:::read_zenodo(id = "8380870")
#'
#' @keywords internal
#' @noRd
read_zenodo <- function(id) {
  verify_suggested(c("curl", "jsonlite", "fs"))

  # Defensive input checks (Shiny-safe)
  if (length(id) != 1L || is.na(id) || !nzchar(as.character(id))) {
    warning_or_modal("Zenodo ID must be a non-empty scalar.")
    return(NULL)
  }
  id <- as.character(id)

  base_url <- "https://zenodo.org/api/records/"
  record_url <- paste0(base_url, id)

  # Curl handle: timeouts + redirects for cross-platform stability
  h <- curl::new_handle()
  curl::handle_setopt(
    h, timeout = 60, connecttimeout = 10, followlocation = TRUE,
    # a predictable UA can help debugging and some proxies
    useragent = sprintf("read_zenodo (R %s)", getRversion())
  )

  # Fetch metadata
  zen_record <- tryCatch(
    curl::curl_fetch_memory(record_url, handle = h),
    error = function(e) e
  )

  if (inherits(zen_record, "error")) {
    warning_or_modal(sprintf("Could not contact Zenodo: %s", conditionMessage(zen_record)))
    return(NULL)
  }

  if (!identical(zen_record$status_code, 200L)) {
    warning_or_modal(sprintf("Could not find Zenodo ID '%s' (HTTP %s).", id, zen_record$status_code))
    return(NULL)
  }

  # Content-Type from headers (case can vary)
  content <- tryCatch(
    jsonlite::fromJSON(rawToChar(zen_record$content)),
    error = function(e) e
  )
  if (inherits(content, "error")) {
    warning_or_modal(sprintf("Failed to parse Zenodo metadata: %s", conditionMessage(content)))
    return(NULL)
  }

  file_urls <- content$files$links$download
  if (is.null(file_urls)) file_urls <- content$files$links$self

  if (is.null(file_urls) || !length(file_urls)) {
    warning_or_modal("Sorry, no downloadable files found for this Zenodo record.")
    return(NULL)
  }

  if (length(file_urls) >= 2) {
    e_msg("More than one file in this zenodo record, please select a unique id. Reading first file only.")
    file_urls <- file_urls[1]
  }

  # if self link is used, ensure /content endpoint
  if (!grepl("/content$", file_urls)) file_urls <- paste0(file_urls, "/content")

  # Prefer Zenodo filename if present
  fname <- content$files$filename
  if (!is.null(fname) && length(fname)) {
    fname <- fname[1]
  } else {
    fname <- basename(gsub("/content$", "", file_urls))
  }

  dest <- fs::path(tempdir(), fs::path_sanitize(fname))

  out <- tryCatch(
    curl::curl_download(url = file_urls, destfile = dest, quiet = TRUE, handle = h),
    error = function(e) e
  )

  if (inherits(out, "error")) {
    warning_or_modal(sprintf("Sorry, download failed: %s", conditionMessage(out)))
    return(NULL)
  }

  if (!fs::file_exists(dest) || fs::file_size(dest) == 0) {
    warning_or_modal("Sorry, downloaded file is missing or empty.")
    return(NULL)
  }

  res <- tryCatch(check_RData_with_res_object(x = dest), error = function(e) e)
  if (inherits(res, "error")) {
    warning_or_modal(sprintf("Sorry, downloaded file could not be read: %s", conditionMessage(res)))
    return(NULL)
  }

  return(res)
}
