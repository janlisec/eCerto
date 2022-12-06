# Launch the ShinyApp (Do not remove this comment)
# Or use the blue button on top of this file

# To deploy:
# rsconnect::deployApp(appDir = "C:/Users/jlisec/Rpackages/Rpackage_eCerto/eCerto", appName = c("test","eCerto")[1], forceUpdate = TRUE)

# To check for errors use:
# rsconnect::showLogs(appName = "test", account = "jali")

# to change the package name for testing use:
# golem::set_golem_name(name = c("eCerto", "eCertoTEST")[1])

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
eCerto::run_app() # add parameters here (if any)
