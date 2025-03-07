testthat::test_that(
  desc = "styleTabH1 works",
  code = {
    suppressMessages({
      x <- eCerto:::prepTabH1(x = eCerto:::test_homog()$data)
      xs <- eCerto:::styleTabH1(x = x)
      mt <- data.frame("analyte"="Fe")
      xs2 <- eCerto:::styleTabH1(x = x, mt = mt)
      prec <- unlist(list("Fe"=2))
      xs3 <- eCerto:::styleTabH1(x = x, prec = prec)
    })
    testthat::expect_true(all(c("style_analyte", "style_s_bb", "style_s_bb_min") %in% colnames(xs)))
    testthat::expect_true(all(xs[,"style_analyte"]=="red"))
    testthat::expect_true(all(nchar(xs[,"mean"])==6))
    testthat::expect_false(all(xs2[,"style_analyte"]=="red"))
    testthat::expect_false(all(nchar(xs3[,"mean"])==6))
  }
)
