testthat::test_that("plot_lts_data works", {
  s <- eCerto:::test_Stability_Excel()
  apm <- list("Mn"=list("confirmed"=TRUE))
  x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm=apm)
  out <- eCerto:::plot_lts_data(x=x_prep)
  testthat::expect_true(is.numeric(out))
  testthat::expect_equal(names(out), "2184-02-22")
  testthat::expect_equal(unname(out), 2076)
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
  x_prep[["def"]][,"KW"] <- "test_text"
  vdiffr::expect_doppelganger(
    title = "FigS1 type1 with KW",
    fig = function() eCerto:::plot_lts_data(x=x_prep, type = 1)
  )
})
