testthat::test_that(
  desc = "read_zenodo works",
  code = {
    suppressMessages({
      x <- eCerto:::read_zenodo(id = "8380870")
    })
    testthat::expect_true(is.list(x))
    testthat::expect_length(x, 6)
    testthat::expect_true("Certification" %in% names(x))
    testthat::expect_warning(eCerto:::read_zenodo(id = "a"))
  }
)
