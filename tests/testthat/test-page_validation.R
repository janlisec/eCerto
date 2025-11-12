testthat::test_that(
  desc = "Module page_validationServer works",
  code = {
  fl <- system.file("extdata", "eCerto_Testdata_VModule.xlsx", package = "eCerto")
  td <- eCerto:::read_Vdata(file = fl, fmt = eCerto:::check_fmt_Vdata(fl))
  shiny::testServer(
    app = eCerto:::page_validationServer, args = list("test_data" = td), expr = {

      session$flushReact()

      # V_pars list is created
      testthat::expect_true(all(c("opt_figV1_anal", "inp_file_name", "txt_trueness") %in% names(V_pars)))

      # passing of arguments from inputs to pars list works
      session$setInputs(txt_trueness = "A")
      testthat::expect_equal(input$txt_trueness, "A")
      testthat::expect_equal(V_pars$txt_trueness, "A")

  })
})