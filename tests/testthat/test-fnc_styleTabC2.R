testthat::test_that(
  desc = "styleTabC2 works",
  code = {
    x <- eCerto:::test_Certification_Excel()
    suppressWarnings({ x <- eCerto:::prepTabC2(dat = x) })
    x <- eCerto:::styleTabC2(x = x, n = 2)
    testthat::expect_true(inherits(x, "datatables"))
  }
)
