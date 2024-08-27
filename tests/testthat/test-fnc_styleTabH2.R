testthat::test_that(
  desc = "styleTabH2 works",
  code = {
    x <- data.frame("Item" = LETTERS[1:3], "mean" = 1:3, "sd" = rnorm(3), "n" = 4:6)
    tmp <- eCerto:::styleTabH2(x = x)
    testthat::expect_true(inherits(tmp, "datatables"))

    tmp <- eCerto:::styleTabH2(x = x, precision = 2)
    testthat::expect_true(inherits(tmp, "datatables"))
    testthat::expect_true(all(tmp$x$data[,"mean"] == c("1.00", "2.00", "3.00")))
  }
)
