#' @title Help Window: opens a modal with in memory rendering of simple Rmd files
#' @param filename name of the file as string (if necessary, containing also path)
#' @param show_modal Will show the returned help_text in a modal. Can be suppressed by setting to FALSE for testing purposes.
#' @return Returns the help text as HTML (currently produces errors when used)
#' @examples
#' eCerto:::help_the_user_modal(filename = system.file("app/www/rmd/start_gethelp.Rmd", package = "eCerto"), show_modal = FALSE)
#' @noRd
#' @keywords internal
#' @importFrom markdown markdownToHTML
help_the_user_modal <- function(filename, show_modal = TRUE) {
  # check if valid path was provided and look up file in 'www' otherwise
  if (!file.exists(filename)) {
    srp <- shiny::resourcePaths()["www"]
    if (is.na(srp)) stop("No 'shiny::resourcePaths()['www']' defined")
    file_in <- list.files(path = srp, pattern = paste0(filename, ".[Rr][Mm][Dd]$"), recursive = TRUE, full.names = TRUE)
  } else {
    file_in <- filename
  }
  help_text <- NULL
  if (length(file_in)==1 && file.exists(file_in)) {
    message("[help_the_user_modal] Rendering Rmd file: ", file_in)
    help_text <- shiny::withMathJax(
      shiny::HTML(
        markdown::markdownToHTML(
          file = file_in,
          fragment.only = TRUE,
          extensions = c("tables","autolink","latex_math")
        )
      )
    )
    if (show_modal) {
      shiny::showModal(
        shiny::modalDialog(
          help_text,
          footer = NULL,
          size = "m",
          easyClose = TRUE,
          title = NULL
        )
      )
    }
  } else {
    message("[help_the_user_modal] cant find help file: ", filename)
  }
  return(help_text)
}