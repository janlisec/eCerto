testthat::test_that(
  desc = "read_lts_input works",
  code = {
    fl <- system.file("extdata", "eCerto_LTS_example_input.xlsx", package = "eCerto")
    out <- eCerto:::read_lts_input(file = fl)
    testthat::expect_true(is.list(out))
    testthat::expect_equal(length(out), 2L)
    testthat::expect_equal(names(out[[1]]), c("def", "val"))
    out <- eCerto:::read_lts_input(file = fl, simplify = TRUE)
    testthat::expect_true(is.data.frame(out))
  }
)
