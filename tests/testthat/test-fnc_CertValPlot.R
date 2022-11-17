testthat::test_that("CertValPlot works on expected data frame format", {
  data <- data.frame("ID"=1:20, "value"=rnorm(20), "analyte"="X", "Lab"=gl(2,10), "L_flt"=FALSE)
  testthat::expect_null(eCerto:::CertValPlot(data=data))
  testthat::expect_null(eCerto:::CertValPlot(data=data, annotate_id=TRUE))
  testthat::expect_null(eCerto:::CertValPlot(data=data, annotate_id=TRUE, filename_labels = TRUE))
})

testthat::test_that("CertValPlot produces an expected figure", {
  set.seed(0)
  data <- data.frame("ID"=1:20, "value"=rnorm(20), "analyte"="X", "Lab"=gl(2,10), "L_flt"=FALSE)
  #testthat::expect_snapshot(eCerto:::CertValPlot(data=data))
  vdiffr::expect_doppelganger(
    title = "CertValPlot Standard",
    fig = eCerto:::CertValPlot(data=data)
  )
  vdiffr::expect_doppelganger(
    title = "CertValPlot Annotated",
    fig = eCerto:::CertValPlot(data=data, annotate_id=TRUE, filename_labels = TRUE)
  )
})