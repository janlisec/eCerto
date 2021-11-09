# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp(appName = c("test","eCerto")[1])
# To check for errors use: rsconnect::showLogs(appName = "test")
# Or use the blue button on top of this file

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
eCerto::run_app() # add parameters here (if any)
