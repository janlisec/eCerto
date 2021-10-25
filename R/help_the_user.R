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
help_the_user = function(filename, format = c("rmd", "html", "rmd_with_link")[1], modal=TRUE) {

  # get input Rmd file  # don't `copy_to_tempdir`, or it can't find dependent RMDs
  #browser()
  file_in <- fnc_get_local_file(x = paste0(filename, ".Rmd"), copy_to_tempdir = FALSE)
  # show message if file is not available
  if (!file.exists(file_in)) {
    message("[help_the_user] cant find help file: ", file_in)
  } else {
    file_out <- gsub(".Rmd$",".html", file_in)
    message("[help_the_user] using this file: ", file_out)
    # render Rmd to HTML if required
    if (format %in% c("html", "rmd_with_link")) {
      if (!file.exists(file_out)) {
        message("I am here: ", getwd())
        message("This is dir: ", paste(dir(),collapse=", "))
        #browser()
        #shiny::onStop(unlink(file.path(file_out), force = TRUE))
        #path_in <- fs::path_dir(file_in)
        #setwd(substr(path_in, 1, nchar(path_in)-4))
        if (dirname(file_out)!="www") {
          setwd(substr(dirname(file_out),1,ncol(dirname(file_out))-4))
          message("New wd: ", getwd())
        }
        tmp <- rmarkdown::render(
          input = file_in,
          output_format = "html_document",
          output_file = basename(file_out),
          output_dir = dirname(file_out),
          quiet = TRUE
        )
        message("[help_the_user] did write this new html file: ", tmp)
      }
    }
    #browser()
    help_text <- switch(
      format,
      "html" = shiny::withMathJax(shiny::includeCSS(path = file_out)),
      # shiny::includeMarkdown unfortunately fails when on shinyio
      "rmd" = shiny::withMathJax(shiny::HTML(markdown::markdownToHTML(file = file_in, fragment.only = TRUE))),
      # Note! ifame's can only be served with html files from the www subdirectory
      # to use this option and avoid includeHTML (because it causes side effects to the navbar)
      # we need (!!) to keep the Rmd files in the www subfolder although a creation
      # using tempfile() would be beneficial
      "rmd_with_link" = shiny::tags$iframe(src = file_out, width="100%", height="400", scrolling="yes", seamless="seamless", frameBorder="0"),
      NULL
    )
    # serve the HTML to a modal
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
  }
  return(help_text)
}