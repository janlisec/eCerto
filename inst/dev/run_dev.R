# https://rtask.thinkr.fr/our-shiny-template-to-design-a-prod-ready-app/#Testing_your_application


.rs.api.documentSaveAll() # closes and saves all open files
suppressWarnings(lapply(paste('package:', names(sessionInfo()$otherPkgs), sep = ""), detach, character.only = TRUE, unload = TRUE))# detach all  packages
rm(list = ls(all.names = TRUE)) # clean environment
devtools::document('.') # create NAMESPACE and man
devtools::load_all('.') # load package
# options(shiny.reactlog=TRUE)
options(app.prod = FALSE) # TRUE = production mode, FALSE = development mode
shiny::runApp('inst/app') # run the main app
