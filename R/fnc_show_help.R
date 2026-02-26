#' @title show_help.
#' @description Help Window: opens a modal with in memory rendering of simple Rmd files.
#' @param filename name of the file as string (if necessary, containing also path)
#' @param show_modal Will show the returned help_text in a modal. Can be suppressed by setting to FALSE for testing purposes.
#' @return Returns the help text as HTML (currently produces errors when used)
#' @examples
#' foo <- eCerto:::show_help(
#'   #filename = system.file("app/www/rmd/start_gethelp.Rmd", package = "eCerto"),
#'   filename = system.file("app/www/rmd/stability_plot.Rmd", package = "eCerto"),
#'   #filename = system.file("app/www/rmd/help_start.Rmd", package = "eCerto"),
#'   show_modal = FALSE
#' )
#' str(foo)
#' shiny::shinyApp(ui = bslib::page_fluid(shiny::div(style = "width: 800px;", foo)), server = function(input, output) {})
#' @noRd
#' @keywords internal
show_help <- function(filename, show_modal = TRUE) {
  # check if valid path was provided and look up file in 'www' otherwise
  if (!file.exists(filename)) {
    srp <- shiny::resourcePaths()["www"]
    if (is.na(srp)) stop("No 'shiny::resourcePaths()['www']' defined")
    file_in <- list.files(path = srp, pattern = paste0(filename, ".[Rr][Mm][Dd]$"), recursive = TRUE, full.names = TRUE)
  } else {
    file_in <- filename
  }
  help_text <- NULL
  if (length(file_in) == 1 && file.exists(file_in)) {
    e_msg(paste("Rendering Rmd file:", file_in))
    tmp_html <- tempfile(fileext = ".html")
    tmp_rmd <- fs::path(fs::path_dir(file_in), "tmp.rmd")
    on.exit(unlink(x= c(tmp_html, tmp_rmd)))
    # patch the input file to include an empty title to prevent rendering warnings
    # when title is missing in the Rmd file
    writeLines(text = c(
      '---',
      'title: "&nsbp;"',
      '---',
      readLines(file_in, encoding = "UTF-8")),
      con = tmp_rmd
    )
    rmarkdown::render(
      input = tmp_rmd,
      output_file = tmp_html,
      output_format = rmarkdown::html_fragment(),
      quiet = TRUE
    )
    help_text <- shiny::withMathJax(
      shiny::HTML(
        paste(readLines(tmp_html, encoding = "UTF-8"), collapse = "\n")
      )
    )
    if (show_modal) {
      shiny::showModal(
        shiny::modalDialog(
          help_text,
          footer = NULL,
          size = "l",
          easyClose = TRUE,
          title = NULL
        )
      )
    }
  } else {
    e_msg(paste("cant find help file:", filename))
  }
  return(help_text)
}
