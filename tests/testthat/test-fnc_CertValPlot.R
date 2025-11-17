testthat::test_that("CertValPlot works on expected data frame format", {
  data <- data.frame("ID"=1:20, "value"=rnorm(20), "analyte"="X", "Lab"=gl(2,10), "L_flt"=FALSE)
  # avoid creating a Rplots.pdf in testthat folder
  pdf(NULL)
  testthat::expect_null(eCerto:::CertValPlot(data=data))
  testthat::expect_null(eCerto:::CertValPlot(data=data, annotate_id=TRUE))
  testthat::expect_null(eCerto:::CertValPlot(data=data, annotate_id=TRUE, filename_labels = TRUE))
})

testthat::test_that("CertValPlot produces an expected figure", {
  skip_if_not_linux_ci()

  set.seed(0)
  data <- data.frame("ID"=1:20, "value"=rnorm(20), "analyte"="X", "Lab"=gl(2,10), "L_flt"=FALSE)
  # avoid creating a Rplots.pdf in testthat folder
  pdf(NULL)
  vdiffr::expect_doppelganger(
    title = "CertValPlot Standard",
    fig = function() eCerto:::CertValPlot(data=data)
  )
  data$File <- rep(c("Name_File_1","F2"), each=10)
  vdiffr::expect_doppelganger(
    title = "CertValPlot Annotated",
    fig = function() eCerto:::CertValPlot(data=data, annotate_id=TRUE, filename_labels = TRUE)
  )
})