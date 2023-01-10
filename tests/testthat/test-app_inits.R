# testthat::test_file(path = "tests/testthat//test-app_inits.R")
testthat::test_that(
  desc = "app_inits work as expected",
  code = {
    out <- eCerto:::init_apm()
    testthat::expect_true(is.list(out))
    x <- data.frame(
      "ID"=1:2,
      "analyte"=c("A1","A2"),
      "Lab"=1:2,
      "S_flt"=c(T,F),
      "L_flt"=c(T,F)
    )
    out <- eCerto:::init_apm(x=x)
    testthat::expect_equal(out[["A1"]][["sample_filter"]], 1)
    testthat::expect_equal(out[["A1"]][["lab_filter"]], "1")
  }
)
