testthat::test_that(
  desc = "app can be started in initial state",
  code = {
    # Don't run these tests on the CRAN build servers
    testthat::skip_on_cran()

    # remove resource path 'www' to get consistent snapshots
    if ("www" %in% names(shiny::resourcePaths())) shiny::removeResourcePath("www")

    # run this test app in a headless browser using shinytest2
    if (is.null(getOption("shiny.testmode"))) {
      options(shiny.testmode = TRUE)
      on.exit(options(shiny.testmode = NULL))
    }
    app <- shinytest2::AppDriver$new(eCerto::run_app(), name = "run_app")

    # get initial app values
    init_vals <- app$get_values()

    # check if modules/components are named such that function 'to_startPage' still works
    testthat::expect_true("Start-excelfile-moduleSelect" %in% names(init_vals$input))
browser()
    # Check that test data load button is still present and with consistent name
    testthat::expect_true("Start-Rdatain-load_test_data" %in% names(init_vals$input))

    # check if empty R6 object was initialized
    testthat::expect_true(identical(init_vals$export$`rv`$c_analytes(), list()))

    # check if loading test data works
    app$click(input = "Start-Rdatain-load_test_data")
    test <- app$get_values(export = "rv")$export$rv
    testthat::expect_equal(unname(test$c_analytes()), c("X", "Y", "Z"))

    }
)
