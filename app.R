# Launch the ShinyApp (Do not remove this comment)
# To deploy:
# rsconnect::deployApp(appDir = "C:/Users/jlisec/Rpackages/Rpackage_eCerto/eCerto", appName = c("test","eCerto")[1], forceUpdate = TRUE)
# To check for errors use:
# rsconnect::showLogs(appName = "test", account = "jali")
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
eCerto::run_app() # add parameters here (if any)
