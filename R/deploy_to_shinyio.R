#'@title deploy_to_shinyio.
#'
#'@description
#'\code{deploy_to_shinyio} will deploy_to_shinyio.
#'
#'@param path_app Which path to use for the App.
#'@param path_pkg Which path to use for the App.
#'@param appName Which shinyapp.io name is to be used.
#'
#'@details
#'tbd.
#'
#'@examples
#'#ecerto:::deploy_to_shinyio()
#'
#'@return
#'NULL.
#'
#'@importFrom rsconnect deployApp
#'@keyword internal
#'
deploy_to_shinyio <- function(path_app = "C:/Users/jlisec/Rpackages/ShinyApps/eCerto", path_pkg = "C:/Users/jlisec/Rpackages/Rpackage_eCerto/ecerto", appName = c("TriboMat","eCerto")[2]) {

  # ensure that the current version is installed locally
  # ToDo: would be good but leads to error
  #devtools::install(pkg = "C:/Users/jlisec/Rpackages/Rpackage_eCerto/ecerto", quick = TRUE)

  # get folders required for the online App
  owd <- getwd()
  setwd(path_pkg)
  www_files <- dir(path = "inst/app/www", full.names = TRUE, recursive = TRUE)
  R_files <- dir(path = "R", full.names = TRUE, recursive = TRUE)

  # copy 'www'
  if (!file.exists(paste(path_app,"www",sep="/"))) file.create(paste(path_app,"www",sep="/"))
  file.remove(dir(paste(path_app,"www",sep="/"), full.names = TRUE))
  for (fl in www_files) {
    file.copy(from = fl, to = paste(path_app,"www",sep="/"), overwrite = TRUE)
  }

  # copy 'CRM001.RData' as example data
  fl <- list.files(pattern = "CRM001.RData", full.names = TRUE, recursive = TRUE)
  file.copy(from = fl, to = paste(path_app,"www",sep="/"), overwrite = TRUE)

  # copy 'R' without app.R and (ToDo) removing roxygen2 header
  if (!file.exists(paste(path_app,"R",sep="/"))) file.create(paste(path_app,"R",sep="/"))
  file.remove(dir(paste(path_app,"R",sep="/"), full.names = TRUE))
  for (fl in R_files) {
    file.copy(from = fl, to = paste(path_app,"R",sep="/"), overwrite = TRUE)
  }

  # prepare 'global.R'
  #fl <- list.files(pattern = "DESCRIPTION", full.names = TRUE, recursive = TRUE)
  cat(file = paste(path_app,"global.R",sep="/"))
  for (pkg in c("shiny","shinyjs","shinyalert","DT","openxlsx","outliers","moments","tinytex","rmarkdown","markdown","htmltools","purrr","tidyxl","plyr","R6","agricolae","dgof","rsconnect","magick","rsvg")) {
    cat(paste0("library('", pkg, "')"), file = paste(path_app,"global.R",sep="/"), append=TRUE, sep = "\n")
  }
  #cat("sapply(dir(path_app = 'R', full.names = TRUE, pattern = 'R$'), source)", file = paste(path_app,"global.R",sep="/"), append=TRUE, sep = "\n")
  cat("shiny::addResourcePath(prefix = 'ecerto', directoryPath = 'www')", file = paste(path_app,"global.R",sep="/"), append=TRUE, sep = "\n")

  # prepare 'app.R'
  cat("shiny::shinyApp(ui = app_ui(), server = app_server, onStart = function(){source('global.R')})", file = paste(path_app,"app.R",sep="/"), append=FALSE, sep = "\n")

  # now you should be ready to deploy
  setwd(path_app)
  rsconnect::deployApp(appName = appName, forceUpdate = TRUE)

  #reset wd
  setwd(owd)

  invisible(NULL)
}