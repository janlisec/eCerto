library(testthat)
library(eCerto)

## run all tests similar to Ctrl+Shift+T in RStudio
# testthat::test_check(package = "eCerto", reporter=c("minimal", "location"))

## check for the code covered by test functions
# x <- covr::package_coverage(quiet = FALSE, function_exclusions = "page_*")
# covr::zero_coverage(x = x)
# covr::function_coverage(fun = eCerto:::app_server)
# covr::function_coverage(fun = eCerto:::list2rv, code = source("tests/testthat/test-list2rv.R"))

## tests with snaps fail upon "RCHECK" while being successful in "test_check"
## setting R_TESTS avoids this problem
Sys.setenv(R_TESTS="")

## run all tests
# testthat::test_dir(path = "./tests/testthat")
testthat::test_dir(path = "./testthat")

## to run only test for a specific file use
# testthat::test_file(path = "tests/testthat/test-fnc_CertValPlot.R")
# testthat::test_file(path = "tests/testthat/test-statistic_helper.R")
# testthat::test_file(path = "tests/testthat/test-run_app.R")
