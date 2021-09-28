#' Help Window: opens a modal with respective Help text for users
#'
#' @param filename name of the file as string (if necessary, containing also path)
#' @param format whether the file is html or rmd (default)
#' @param modal shall the modal window be opened (TRUE is default)
#'
#' @return returns the help text as HTML (currently produces errors when used)
#' @export
#'
#'
help_the_user = function(filename, format = "rmd", modal=TRUE) {

  help_text = NULL

  if (format == "html") {
    file <- rmarkdown::render(
      fnc_get_local_file(
        x = paste0(filename, ".Rmd"),
        copy_to_tempdir = FALSE # don't TRUE, or it can't find dependent RMDs
      ),
      output_format = "html_document",
      output_file = paste0(filename,".html"),
      quiet = TRUE
    )
    help_text <- shiny::withMathJax(shiny::includeCSS(path = file))
  } else if (format == "rmd")  {
    help_text <- shiny::withMathJax(
      shiny::includeMarkdown(
        fnc_get_local_file(x = paste0(filename, ".Rmd"), copy_to_tempdir = FALSE)
      )
    )
  } else if (format == "test") {
    file <- rmarkdown::render(
      input = fnc_get_local_file(
        x = paste0(filename, ".Rmd"),
        copy_to_tempdir = FALSE # don't TRUE, or it can't find dependent RMDs
      ),
      output_format = "html_document",
      output_file = paste0(filename,".html"),
      quiet = TRUE
    )
    #browser()
    help_text <- shiny::withMathJax(shiny::includeHTML(file))
  } else {
    warning("format does not exist")
  }

  if (modal==TRUE) {
    shiny::showModal(
      shiny::modalDialog(
        help_text,
        footer = shiny::tagList(shiny::modalButton("Ok")),
        size = "m",
        easyClose = TRUE,
        title = paste("Help", filename, sep=" - ")
      )
    )
  }

  return(help_text)

}