testthat::test_that(
  desc = "check_fmt_Vdata works",
  code = {
    inp <- system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx")
    testthat::expect_equal(eCerto:::check_fmt_Vdata(file = inp), "Agilent")
  }
)
