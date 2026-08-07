# Set options here
#options(golem.app.prod = TRUE) # TRUE = production mode, FALSE = development mode
Sys.setenv("GOLEM_CONFIG_ACTIVE"="dev")

# Detach all loaded packages and clean your environment
golem::detach_all_attached()

# Document and reload your package
golem::document_and_reload()

# don't render help page Rmd to HTML in dev mode
#options(eCerto.renderHelp = FALSE)
options(eCerto.renderHelp = TRUE)

# Run the application
run_app(appDir = "C:/Users/jlisec/Documents/Rpackages/Rpackage_eCerto/eCerto")
