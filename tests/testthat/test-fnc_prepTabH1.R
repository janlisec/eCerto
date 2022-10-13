testthat::test_that("prepTabH1 works", {
  x <- eCerto:::test_homog()$data
  suppressMessages({x <- eCerto:::prepTabH1(x = x)})
  testthat::expect_equal(ncol(x), 10)
  testthat::expect_equal(colnames(x), c('analyte', 'H_type', 'mean', 'n', 'N', 'M_between', 'M_within', 'P', 's_bb', 's_bb_min'))
  for (i in 3:10) testthat::expect_true(is.numeric(x[,i]))
})
