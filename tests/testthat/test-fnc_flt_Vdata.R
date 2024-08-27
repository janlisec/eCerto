testthat::test_that(
  desc = "flt_Vdata works",
  code = {
    inp <- system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx")
    tab <- eCerto:::read_Vdata(file = inp)
    testthat::expect_true(is.data.frame(tab))
    testthat::expect_length(unique(eCerto:::flt_Vdata(x = tab, l = c("2","4"), a = c("PFOA", "PFBA"))[,"Level"]), 3)
    testthat::expect_true(all(eCerto:::flt_Vdata(x = tab, l = c(2,5), a = "PFBA", rng = FALSE)[,"Analyte"] %in% c("PFOA","PFBA")))
    testthat::expect_length(unique(eCerto:::flt_Vdata(x = tab, l = c(2,5), a = "PFBA", rng = FALSE)[,"Level"]), 2)
    testthat::expect_true(all(eCerto:::flt_Vdata(x = tab, l = c(2,5), a = "PFBA", rng = FALSE)[,"Analyte"]=="PFBA"))
  }
)
