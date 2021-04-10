test_that("App works", {
  # Don't run these tests on the CRAN build servers
  # testthat::skip_on_cran()
  
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  appdir = system.file(package = "ecerto","app")
  shinytest::expect_pass(
    shinytest::testApp(
      appDir = appdir,
      testnames = c(
        "Excel_Upload_Errortest",
        "UploadTest",
        "AnalyteChangeTest",
        "transferTest")
    )
  )
})

