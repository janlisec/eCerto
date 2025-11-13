testthat::test_that(
  desc = "Module m_longtermstabilityServer works",
  code = {
  fl <- system.file("extdata", "eCerto_LTS_example_input.xlsx", package = "eCerto")
  shiny::testServer(
    app = eCerto:::m_longtermstabilityServer, args = list("test_data" = fl), expr = {

      session$flushReact()

      testthat::expect_true(is.list(lts$data))
      testthat::expect_true(all(names(lts$data[[1]]) %in% c("def", "val")))

      # passing of arguments from inputs to pars list works
      session$setInputs("LTS_sel_KW" = "KW2")
      testthat::expect_equal(i(), 2)

  })
})