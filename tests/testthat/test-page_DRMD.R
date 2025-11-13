testthat::test_that(
  desc = "Module page_DRMDServer works",
  code = {
  fl <- system.file("extdata", "drmd", "BAM-M375a.xml", package = "eCerto")
  shiny::testServer(
    app = eCerto:::page_DRMDServer, args = list("test_data" = fl), expr = {

      session$flushReact()

      testthat::expect_true(is.list(D_data()))
      testthat::expect_true(all(colnames(D$tab_D1) %in% c("path", "idx", "value")))

      # passing of arguments from inputs to pars list works
      session$setInputs(tab_D1_rows_selected = 3)
      testthat::expect_equal(D$tab_D1_i, 3)

  })
})