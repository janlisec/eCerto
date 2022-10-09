testthat::test_that("Fig.S3 works", {
  # prepare test data
  x <- data.frame(
    "dummy_name" = c("0", "4"),
    "1/K" = 1/(c(0, 4)+273.15),
    "log(-k_eff)" = c(1, 2),
    "CI_upper" = c(2, 3),
    "CI_lower" = c(0, 1),
    "log(k)_calc" = c(1, 2),
    check.names = FALSE
  )
  # check if function is running properly (returning NULL)
  testthat::expect_null(eCerto:::prepFigS3(tab = x))
})