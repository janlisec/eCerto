# Launch the ShinyApp (Do not remove this comment)
# Or use the blue button on top of this file

# To deploy to shinyapps.io:
# rsconnect::deployApp(appDir = "C:/Users/jlisec/Documents/Rpackages/Rpackage_eCerto/eCerto", appName = c("test","eCerto")[1], forceUpdate = TRUE)

# To check for errors in shinyapps.io:
# rsconnect::showLogs(appName = "test", account = "jali")

# to switch between CRAN and BAM-Server version upon compile modify
# the option 'bam_server' in 'golem-config.yml'

# to check the test code coverage use:
# x <- covr::package_coverage(function_exclusions = "page_*")
# covr::zero_coverage(x = x)
# covr::function_coverage(fun = eCerto:::app_server)
# covr::function_coverage(fun = eCerto:::styleTabC1, code = source("tests/testthat/test-fnc_styleTabC1.R"))
# covr::function_coverage(fun = eCerto:::CertValPlot, code = source("tests/testthat/test-fnc_CertValPlot.R"))

pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
eCerto::run_app() # add parameters here (if any)
