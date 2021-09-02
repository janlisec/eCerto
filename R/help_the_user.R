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
  if(format == "html"){
    file = paste0(filename,".html")
    rmarkdown::render(
      fnc_get_local_file(
        x = paste0(filename, ".Rmd"),
        copy_to_tempdir = FALSE # don't TRUE, or it can't find dependent RMDs
      ),
      quiet = TRUE
    )
    help_text = shiny::withMathJax(shiny::includeCSS(
      ecerto::fnc_get_local_file(file, copy_to_tempdir = FALSE)
    ))
  } else if(format == "rmd")  {
    file = paste0(filename, ".Rmd")
    help_text = shiny::withMathJax(shiny::includeMarkdown(
      #rmarkdown::render(input = system.file("rmd", "uncertainty.Rmd", package = "ecerto"))
      ecerto::fnc_get_local_file(file, copy_to_tempdir = FALSE)
    ))
  } else {
    warning("format does not exist")
  }

if(modal==TRUE) {
  shiny::showModal(
    shiny::modalDialog(
      #@FK: Der in issue # 66 beschribene Fehler tritt nur auf, wenn man modal_html übergibt, bei HTML("bla") funktioniert alles
      #HTML("bla"),
      help_text,
      footer = shiny::tagList(shiny::modalButton("Ok")),
      size = "m",
      easyClose = TRUE,
      title = "Help" # TODO title
    )
  )
}
return(help_text)

}