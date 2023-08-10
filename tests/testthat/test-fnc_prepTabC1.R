testthat::test_that(
  desc = "fnc_prepTabC1 works as expected",
  code = {
    rv <- eCerto:::test_rv(type = "SR3")
    shiny::isolate(dat <- rv$c_fltData())
    shiny::isolate(lab_means <- rv$c_lab_means(data = dat))
    # table values are dependent on parameter 'fmt'
    testthat::expect_equal(eCerto:::prepTabC1(dat = dat, lab_means = lab_means)[1,"Dixon"], "n.s.")
    testthat::expect_equal(eCerto:::prepTabC1(dat = dat, lab_means = lab_means, fmt = "cval")[1,"Dixon"], 0.002778, tolerance = 6)
    # setting 'excl_labs = TRUE' will remove depending on column 'L_flt'
    dat[dat[,"Lab"]=="L13","L_flt"] <- TRUE
    shiny::isolate(lab_means <- rv$c_lab_means(data = dat))
    testthat::expect_equal(nrow(eCerto:::prepTabC1(dat = dat, lab_means = lab_means, excl_labs = TRUE)), 9)
  }
)
