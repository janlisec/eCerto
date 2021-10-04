#' Help Window: opens a modal with respective Help text for users
#'
#' @param filename name of the file as string (if necessary, containing also path)
#' @param format whether the file is html or rmd (default)
#' @param modal shall the modal window be opened (TRUE is default)
#'
#' @return returns the help text as HTML (currently produces errors when used)
#' @export
#'
#' @importFrom markdown markdownToHTML
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
    message(fnc_get_local_file(x = paste0(filename, ".Rmd"), copy_to_tempdir = FALSE))
    test <- markdown::markdownToHTML(
      file = fnc_get_local_file(x = paste0(filename, ".Rmd"), copy_to_tempdir = FALSE),
      fragment.only = TRUE
    )
    help_text <- shiny::withMathJax(shiny::HTML(test))
    # shiny::includeMarkdown unfortunately  fails when on shinyio
  } else if (format == "rmd_with_link") {
    # Note! ifame's can only be served with html files from the www subdirectory
    # to use this option and avoid includeHTML (because it causes side effects to the navbar)
    # we need (!!) to keep the Rmd files in the www subfolder although a creation
    # using tempfile() would be beneficial
    #tmp_file <- tempfile(pattern = "ecerto_help_", fileext = ".html")
    tmp_file <- paste0(filename, ".html")
    file <- rmarkdown::render(
      input = fnc_get_local_file(
        x = paste0(filename, ".Rmd"),
        copy_to_tempdir = FALSE # don't TRUE, or it can't find dependent RMDs
      ),
      output_format = "html_document",
      output_file = tmp_file,
      quiet = TRUE
    )
    #tmp_file <- "certification_laboratoryStatistics.html"
    #tmp_file <- file
    #file.exists(tmp_file)
    #help_text <- includeHTML(tmp_file)
    help_text <- shiny::tags$iframe(src = tmp_file, width="100%", height="400", scrolling="yes", seamless="seamless", frameBorder="0")
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