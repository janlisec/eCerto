testthat::test_that("Fig.S2 works", {
  # load test data
  x <- eCerto:::test_Stability_Arrhenius(3)
  # check for warning in case that 'Value' is not standardized
  testthat::expect_warning(eCerto:::prepFigS2(tmp = x))
  # standardized 'Value' and check if function is running properly (returning NULL)
  x$Value <- x$Value/mean(x$Value[x$time==0])
  testthat::expect_null(eCerto:::prepFigS2(tmp = x))
  # check if all parameters can be set
  testthat::expect_null(eCerto:::prepFigS2(tmp = x, show_reference_point = FALSE))
  testthat::expect_null(eCerto:::prepFigS2(tmp = x, plot_nominal_scale = FALSE))
  testthat::expect_null(eCerto:::prepFigS2(tmp = x, plot_in_month = FALSE))
  testthat::expect_null(eCerto:::prepFigS2(tmp = x, plot_ln_relative = FALSE))
})