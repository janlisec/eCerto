testthat::test_that(
  desc = "round_DIN1333 works",
  code = {
    x <- c(-2.5,2.5,3.5)
    eCerto:::round_DIN1333(x)
    testthat::expect_equal(eCerto:::round_DIN1333(x), c(-3, 3, 4))
  }
)
