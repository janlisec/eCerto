testthat::test_that(
  desc = "fnc_preTabC2 works",
  code = {
    x <- eCerto:::test_Certification_Excel()
    # Anscombe-Test returns a warning for this test data
    suppressWarnings( {out <- eCerto:::prepTabC2(dat = x)} )
    testthat::expect_true(is.data.frame(out))
    testthat::expect_equal(out[,"Median"], 1.4936)
  }
)
