#' Help Window: opens a modal with in memory rendering of simple Rmd files
#'
#' @param filename name of the file as string (if necessary, containing also path)
#'
#' @return returns the help text as HTML (currently produces errors when used)
#'
#' @keywords internal
#'
#' @importFrom markdown markdownToHTML
#'
help_the_user_modal <- function(filename) {
  #browser()
  file_in <- list.files(path = shiny::resourcePaths()["www"], pattern = paste0(filename, ".[Rr][Mm][Dd]$"), recursive = TRUE, full.names = TRUE)
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
    shiny::showModal(
      shiny::modalDialog(
        help_text,
        footer = NULL,
        size = "m",
        easyClose = TRUE,
        title = NULL
      )
    )
  } else {
    message("[help_the_user_modal] cant find help file: ", filename)
  }
  return(help_text)
}