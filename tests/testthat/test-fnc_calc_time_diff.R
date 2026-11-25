testthat::test_that(
  desc = "calc_time_diff works",
  code = {
    x <- c("2022-02-01", "2022-02-03", "2022-03-01", "2024-02-01")
    testthat::expect_identical(eCerto::calc_time_diff(x = x), c(0, 0, 1, 24))
    testthat::expect_equal(eCerto::calc_time_diff(x = x, exact = TRUE), c(0.00000000, 0.06570842, 0.91991786, 23.98357290), tolerance = 10^-6)
    testthat::expect_identical(eCerto::calc_time_diff(x = x, type = "day"), c(0L, 2L, 28L, 730L))
    testthat::expect_identical(eCerto::calc_time_diff(x = x, type = "year"), c(0L, 0L, 0L, 2L))
    testthat::expect_identical(eCerto::calc_time_diff(x = x, type = "year", d_start = "2021-12-31"), c(1L, 1L, 1L, 3L))
    testthat::expect_identical(eCerto::calc_time_diff(x = 1:3, type = "day", origin = Sys.Date()), c(0L, 1L, 2L))
  }
)
