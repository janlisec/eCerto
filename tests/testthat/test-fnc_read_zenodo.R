testthat::test_that(
  desc = "read_zenodo works",
  code = {
    testthat::skip_on_cran() # 64-debian_gcc failed on CRAN; can not test the problem
    testthat::skip_if_offline()
    x <- eCerto:::read_zenodo(id = "8380870")
    if (!is.null(x)) {
      testthat::expect_true(is.list(x))
      testthat::expect_length(x, 6)
      testthat::expect_true("Certification" %in% names(x))
    }
    testthat::expect_warning(eCerto:::read_zenodo(id = "no_valid_id"))
  }
)
