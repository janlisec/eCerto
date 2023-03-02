testthat::test_that("styleTabC1 works", {
  suppressMessages({
    rv <- eCerto:::test_rv(type = "SR3")
    x <- shiny::isolate(eCerto:::prepTabC1(dat = rv$c_fltData(), lab_means = rv$c_lab_means()))
    xs <- eCerto:::styleTabC1(x = x)
  })
  testthat::expect_true(inherits(xs, "datatables"))
})
