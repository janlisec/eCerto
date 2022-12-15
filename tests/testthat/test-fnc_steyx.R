testthat::test_that(
  desc = "round_DIN1333 works",
  code = {
    # perfect correlation
    testthat::expect_equal(eCerto::steyx(x=1:3, y=2:4), 0)
    # reasonable test
    testthat::expect_equal(eCerto::steyx(x=1:3, y=c(2,3.1,3.9)), 0.122474487)
    # catch errors for wrong inputs
    testthat::expect_error(eCerto::steyx(x=1:3))
    testthat::expect_error(eCerto::steyx(y=1:3))
    testthat::expect_error(eCerto::steyx(x=1:3, y=2))
    testthat::expect_error(eCerto::steyx(x=1:5, y=c(NA,NA,NA,2,2)))
  }
)
