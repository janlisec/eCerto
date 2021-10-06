#'@title deploy_to_shinyio.
#'
#'@description
#'\code{deploy_to_shinyio} will deploy_to_shinyio.
#'
#'@param path Which path to use for the App.
#'@param appName Which shinyapp.io name is to be used.
#'
#'@details
#'tbd.
#'
#'@examples
#'#'#ecerto:::deploy_to_shinyio()
#'
#'@return
#'NULL.
#'
#'@importFrom rsconnect deployApp
#'
deploy_to_shinyio <- function(path = "C:/Users/jlisec/Rpackages/ShinyApps/eCerto", appName = c("TriboMat","eCerto")[2]) {

  owd <- getwd()
  e_path <- system.file(package = "ecerto")
  setwd(e_path)
  www_files <- dir(path = "inst/app/www", full.names = TRUE, recursive = TRUE)
  R_files <- dir(path = "R", full.names = TRUE, recursive = TRUE)

  # copy 'www'
  if (!file.exists(paste(path,"www",sep="/"))) file.create(paste(path,"www",sep="/"))
  for (fl in www_files) {
    file.copy(from = fl, to = paste(path,"www",sep="/"), overwrite = TRUE)
  }

  # copy 'CRM001.RData' as example data
  fl <- list.files(pattern = "CRM001.RData", full.names = TRUE, recursive = TRUE)
  file.copy(from = fl, to = paste(path,"www",sep="/"), overwrite = TRUE)

  # copy 'R' without app.R and removing roxygen2 header
  if (!file.exists(paste(path,"R",sep="/"))) file.create(paste(path,"R",sep="/"))
  for (fl in R_files) {
    file.copy(from = fl, to = paste(path,"R",sep="/"), overwrite = TRUE)
  }

  # prepare 'global.R'
  #fl <- list.files(pattern = "DESCRIPTION", full.names = TRUE, recursive = TRUE)
  cat(file = paste(path,"global.R",sep="/"))
  for (i in c("shiny","shinyjs","shinyalert","DT","openxlsx","outliers","moments","tinytex","rmarkdown","markdown","htmltools","purrr","tidyxl","plyr","R6","agricolae","dgof","rsconnect")) {
    cat(paste0("library('",i,"')"), file = paste(path,"global.R",sep="/"), append=TRUE, sep = "\n")
  }
  #cat("sapply(dir(path = 'R', full.names = TRUE, pattern = 'R$'), source)", file = paste(path,"global.R",sep="/"), append=TRUE, sep = "\n")
  cat("shiny::addResourcePath(prefix = 'ecerto', directoryPath = 'www')", file = paste(path,"global.R",sep="/"), append=TRUE, sep = "\n")

  # prepare 'app.R'
  cat("shiny::shinyApp(ui = app_ui(), server = app_server, onStart = function(){source('global.R')})", file = paste(path,"app.R",sep="/"), append=FALSE, sep = "\n")

  # now you should be ready to deploy
  setwd(path)
  rsconnect::deployApp(appName = appName)

  #reset wd
  setwd(owd)

  invisible(NULL)
}