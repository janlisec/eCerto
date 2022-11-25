library(testthat)
library(eCerto)

# run all tests similar to Ctrl+Shift+T in RStudio
# testthat::test_check(package = "ecerto", reporter=c("minimal", "location"))

# check for the code covered by test functions
# covr::package_coverage()

# tests with snaps fail upon check while being successful in test
# setting R_TESTS avoids this problem
Sys.setenv(R_TESTS="")

# run all tests
testthat::test_dir(path = "./testthat")

# to run only test for a specific file use
# testthat::test_file(path = "tests/testthat//test-fnc_CertValPlot.R")
