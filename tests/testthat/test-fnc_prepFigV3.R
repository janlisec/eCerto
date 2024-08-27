testthat::test_that(
  desc = "prepFigV3 works",
  code = {
    inp <- system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx")
    tab <- eCerto:::read_Vdata(file = inp)
    x <- eCerto:::flt_Vdata(x = tab, l = c(2,4), a = "PFBA")
    pdf(NULL)
    vdiffr::expect_doppelganger(
      title = "FigV3 Standard",
      fig = function() eCerto:::prepFigV3(x = x, cex = 0.8)
    )
  }
)
