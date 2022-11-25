testthat::test_that("plot_lts_data works", {
  s <- eCerto:::test_Stability_Excel()
  apm <- list("Mn"=list("confirmed"=TRUE))
  x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm=apm)
  out <- eCerto:::plot_lts_data(x=x_prep)
  mt <- data.frame("analyte"="Mn", "cert_val"=1, "U_abs"=1, "sd"=1, "unit"="unit")
  x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm=apm, U_Def="U", mt=mt)
  vdiffr::expect_doppelganger(
    title = "FigS1 type1",
    fig = function() eCerto:::plot_lts_data(x=x_prep, type = 1)
  )
  vdiffr::expect_doppelganger(
    title = "FigS1 type2",
    fig = function() eCerto:::plot_lts_data(x=x_prep, type = 2)
  )

})
