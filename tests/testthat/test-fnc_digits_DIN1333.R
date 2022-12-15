testthat::test_that(
  desc = "round_DIN1333 works",
  code = {
    x <- c(0.011, 0.021, 0.0299999, 0.03, 0.031, 0.000299, 29.01, 3.01, 200, 300)
    testthat::expect_equal(eCerto:::digits_DIN1333(x), c(3, 3, 3, 2, 2, 5, 0, 0, -1, -2))
  }
)
