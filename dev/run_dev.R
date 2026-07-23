# Set options here
#options(golem.app.prod = TRUE) # TRUE = production mode, FALSE = development mode
Sys.setenv("GOLEM_CONFIG_ACTIVE"="dev")

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# dont render help page Rmd to HTML in dev mode
options(eCerto.renderHelp = TRUE)

# Run the application
# options(shiny.testmode = FALSE) # to get the help documentation rendered ensure that shiny.testmode = FALSE
run_app(appDir = "C:/Users/jlisec/Documents/Rpackages/Rpackage_eCerto/eCerto")
