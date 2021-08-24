library(testthat)
library(ecerto)

# testthat::test_check(package = "ecerto",reporter=c("minimal", "location"))

testthat::test_dir(path = "./testthat")
