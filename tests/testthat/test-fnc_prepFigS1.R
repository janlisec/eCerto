testthat::test_that("Fig.S1 works", {
  # load test data
  s <- eCerto:::test_Stability_Excel()
  apm <- list("Mn"=list("confirmed"=TRUE))
  x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm = apm)
  # returns list of length=2
  testthat::expect_length(x_prep, 2)
  testthat::expect_true(all(x_prep[["val"]][,1]=="Mn"))
  testthat::expect_equal(x_prep[["def"]][1,"U"], 1.39661008)

  mt <- data.frame("analyte"="Mn", "cert_val"=1, "U_abs"=1, "sd"=1, "unit"="unit")
  x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm = apm, U_Def = "U", mt = mt)
  testthat::expect_equal(x_prep[["def"]][1,"U"], 1)
})