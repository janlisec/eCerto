testthat::test_that(
  desc = "get_local_file works",
  code = {
    # check that function stops if input is not character vector of length = 1
    testthat::expect_error(eCerto:::get_local_file(x = NULL))
    testthat::expect_error(eCerto:::get_local_file(x = c("", "")))
    # check that function returns warning if 'www' does not exist as resourcePath
    if ("www" %in% names(shiny::resourcePaths())) shiny::removeResourcePath("www")
    testthat::expect_warning(eCerto:::get_local_file(x=""))
    # check that function returns NA if 'www' does not exist as resourcePath
    suppressWarnings(x <- eCerto:::get_local_file(x = ""))
    testthat::expect_equal(x, NA)
    # check that function returns warning if 'file' does not exist as resourcePath
    x <- as.character(fs::file_temp())
    shiny::addResourcePath(prefix="www", directoryPath = dirname(x))
    testthat::expect_warning(eCerto:::get_local_file(x = basename(x)))
    # check that function returns appropriate full path if 'www' does exist and file is present
    shiny::addResourcePath(prefix="www", directoryPath = dirname(x))
    cat(file = x, append = F)
    testthat::expect_equal(x, eCerto:::get_local_file(x = basename(x)))
    # check that function returns warning if 'file' specification is ambiguous at resourcePath
    x <- as.character(fs::file_temp())
    cat(file = x, append = F)
    testthat::expect_warning(eCerto:::get_local_file(x = "file"))
  }
)
