testthat::test_that(
  desc = "prepFigV2 works",
  code = {
    skip_if_not_linux_ci()

    inp <- system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx")
    tab <- eCerto:::read_Vdata(file = inp)
    pdf(NULL)
    vdiffr::expect_doppelganger(
      title = "FigV2 Standard",
      fig = function() eCerto:::prepFigV2(tab = tab, a = "PFOA", alpha = 0.01, cex = 1)
    )
  }
)
