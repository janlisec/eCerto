#'@title deploy_to_shinyio.
#'
#'@description
#'\code{deploy_to_shinyio} will deploy_to_shinyio.
#'
#'@param path_app Which path to use for the App.
#'@param path_pkg Which path to use for the App.
#'@param appName Either shinyapp.io name to deploy to or NULL to omit deployment step.
#'
#'@details
#'use `rsconnect::showLogs(appName = "test")` to check for errors
#'
#'@examples
#'\dontrun{ecerto:::deploy_to_shinyio(appName=NULL)}
#'
#'@return
#'NULL.
#'
#'@importFrom rsconnect deployApp
#'@keywords internal
#'
deploy_to_shinyio <- function(path_app = "C:/Users/jlisec/Rpackages/ShinyApps/eCerto", path_pkg = "C:/Users/jlisec/Rpackages/Rpackage_eCerto/ecerto", appName = c("test","eCerto")[1]) {

  # ensure that the current version is installed locally
  # ToDo: would be good but leads to error
  #devtools::install(pkg = path_pkg, quick = TRUE)

  # get folders required for the online App
  owd <- getwd()
  setwd(path_pkg)
  www_files <- dir(path = "inst/app/www", full.names = TRUE, recursive = TRUE)
  R_files <- dir(path = "R", full.names = TRUE, recursive = TRUE)

  # copy 'R' without app.R and (ToDo) removing roxygen2 header
  if (!file.exists(paste(path_app,"R",sep="/"))) file.create(paste(path_app,"R",sep="/"))
  file.remove(dir(paste(path_app,"R",sep="/"), full.names = TRUE))
  for (fl in R_files) {
    if (!basename(fl) %in% c("test_helper.R","deploy_to_shinyio")) file.copy(from = fl, to = paste(path_app,"R",sep="/"), overwrite = TRUE)
  }

  # copy 'www'
  if (!file.exists(paste(path_app,"www",sep="/"))) file.create(paste(path_app,"www",sep="/"))
  file.remove(dir(paste(path_app,"www",sep="/"), full.names = TRUE))
  for (fl in www_files) {
    file.copy(from = fl, to = paste(path_app,"www",sep="/"), overwrite = TRUE)
  }

  # copy 'CRM001.RData' as example data
  fl <- list.files(pattern = "CRM001.RData", full.names = TRUE, recursive = TRUE)
  file.copy(from = fl, to = paste(path_app,"www",sep="/"), overwrite = TRUE)

  # prepare 'global.R'
  #fl <- list.files(pattern = "DESCRIPTION", full.names = TRUE, recursive = TRUE)
  cat(file = paste(path_app,"global.R",sep="/"))
  for (pkg in c("shiny","shinyjs","shinyalert","DT","openxlsx","outliers","moments","tinytex","rmarkdown","markdown","htmltools","purrr","tidyxl","plyr","R6","agricolae","dgof","rsconnect","magick","rsvg")) {
    cat(paste0("library('", pkg, "')"), file = paste(path_app,"global.R",sep="/"), append=TRUE, sep = "\n")
  }

  # prepare 'app.R'
  DESC <- readLines("DESCRIPTION")
  status_line <- paste0("ver ", gsub("Version: ", "", DESC[grep("Version", DESC)]),
                        " (", gsub("Date: ", "", DESC[grep("Date", DESC)]), ")",
                        " jan.lisec@bam.de")
  rm(DESC)
  cat('source("global.R")', file = paste(path_app,"app.R",sep="/"), append=FALSE, sep = "\n")
  cat("# start shinyApp using package function", file = paste(path_app, "app.R", sep="/"), append=TRUE, sep = "\n")
  cat(paste0("ecerto('status_line'='", status_line, "')"), file = paste(path_app, "app.R", sep="/"), append=TRUE, sep = "\n")

  # now you should be ready to deploy
  if (!is.null(appName)) {
    setwd(path_app)
    rsconnect::deployApp(appName = appName, forceUpdate = TRUE)
  }

  #reset wd
  setwd(owd)

  invisible(NULL)
}