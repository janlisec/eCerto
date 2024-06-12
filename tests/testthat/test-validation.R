testthat::test_that(
  desc = "Validation module functions work as expected",
  code = {

    # check if example data file exists
    inp <- system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx")
    testthat::expect_true(file.exists(inp))

    # check Excel file import using example data
    tab <- eCerto:::read_Vdata(file = inp)
    testthat::expect_true(is.data.frame(tab))
    testthat::expect_true(all(c("Analyte", "Level") %in% colnames(tab)))
    testthat::expect_true(all(sapply(tab[,c("Analyte", "Level")], is.factor)))

    # check if statistics on import data works
    out <- plyr::ldply(levels(tab[,"Analyte"]), function(a) {
        eCerto:::prepTabV1(tab = tab, a = a)
    })
    testthat::expect_true(is.data.frame(out))
    testthat::expect_true(nrow(out)==4)
    testthat::expect_true(all(out[,"Analyte"]==c("PFBA","4-2 FTSA","PFHxA","PFOA")))

    # check styling of Tab.V1
    out_styled <- eCerto:::style_tabV1(df = out, selected = NULL)
    testthat::expect_true(inherits(out_styled, "datatables"))

    # check generation of Fig.V1
    ab <- eCerto:::prepDataV1(tab = tab)
    testthat::expect_true(length(ab)==8)
    ab <- eCerto:::prepDataV1(tab = tab, fmt = "norm")
    testthat::expect_true(all(names(attributes(ab))==c("names","Analyte","Level","Concentration")))
    ab <- eCerto:::prepDataV1(tab = tab, a = "PFOA", l = c("2", "7"), fmt = "rel_norm")
    pdf(NULL)
    vdiffr::expect_doppelganger(
      title = "Fig_V1",
      fig = function() eCerto:::prepFigV1(ab = ab)
    )

    # check generation of Fig.V2 (Linearity details)
    pdf(NULL)
    vdiffr::expect_doppelganger(
      title = "Fig_V2",
      fig = function() eCerto:::prepFigV2(tab = tab, a = "PFOA", alpha = 0.01, cex = 1)
    )

    # check generation of Fig.V3 (Working range details)
    x <- eCerto:::flt_Vdata(x = tab, l = c(2,4), a = "PFBA")
    pdf(NULL)
    vdiffr::expect_doppelganger(
      title = "Fig_V3",
      fig = function() eCerto:::prepFigV3(x = x, cex = 0.8)
    )

  }
)