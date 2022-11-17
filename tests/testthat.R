library(testthat)
library(eCerto)

# run all tests similar to Ctrl+Shift+T in RStudio
# testthat::test_check(package = "ecerto", reporter=c("minimal", "location"))

# check for the code covered by test functions
# covr::package_coverage()

testthat::test_dir(path = "./testthat")
