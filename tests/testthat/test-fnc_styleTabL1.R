testthat::test_that(
  desc = "styleTabL1 works",
  code = {
    x <- data.frame("Value"=1, "Date"=Sys.Date(), "File"="XX.smp")
    testthat::expect_true(inherits(eCerto:::styleTabL1(x = x), "datatables"))
  }
)
