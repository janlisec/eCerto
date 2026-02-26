testthat::test_that(
  desc = "page_ValidationServer works on package testdata",
  code = {
    # set up eCerto R6 object containing data from an arrhenius style experiment
    fl <- system.file("extdata", "eCerto_Testdata_VModule.xlsx", package = "eCerto")
    td <- eCerto:::read_Vdata(file = fl, fmt = eCerto:::check_fmt_Vdata(fl))

    # provide this test data to the server function and test if the expected outputs are generated
    shiny::testServer(
      app = eCerto:::page_validationServer,
      args = list(
        id = "test",
        test_data = td
      ),
      expr = {
        # check data upload and processing
        session$setInputs(
          opt_V2_vals = "Area_Analyte"
        )
        V_pars$opt_figV1_level <- "1"
        current_analyte$name <- "PFBA"
        x <- V2_dat()
        testthat::expect_true(is.list(x))
        testthat::expect_true(length(x)==1L)
        testthat::expect_true(all(c("Level", "Analyte") %in% colnames(x[[1]])))

        V_pars$opt_tabV1_useLevels <- FALSE
        x <- tab_V1()
        testthat::expect_true(is.data.frame(x))
        testthat::expect_true(nrow(x)==4L)

      }
    )
  })
