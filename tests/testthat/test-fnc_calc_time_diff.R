testthat::test_that(
  desc = "calc_time_diff works",
  code = {
    x <- c("2022-02-01", "2022-02-03", "2022-03-01", "2024-02-01")
    testthat::expect_equal(eCerto::calc_time_diff(x=x), c(0,0,1,24))
    testthat::expect_equal(eCerto::calc_time_diff(x=x, type="day"), c(0,2,28,730))
    testthat::expect_equal(eCerto::calc_time_diff(x=x, type="year"), c(0,0,0,2))
    testthat::expect_equal(eCerto::calc_time_diff(x=x, type="year", d_start="2021-12-31"), c(1,1,1,3))
    testthat::expect_equal(eCerto::calc_time_diff(x=1:3, type="day", origin=Sys.Date()), c(0,1,2))
  }
)