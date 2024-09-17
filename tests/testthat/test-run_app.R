testthat::test_that(
  desc = "app can be started in initial state",
  code = {
    # Don't run these tests on the CRAN build servers
    testthat::skip_on_cran()
    #testthat::skip_on_covr()
    #testthat::skip_on_ci()

    # remove resource path 'www' to get consistent snapshots
    #if ("www" %in% names(shiny::resourcePaths())) shiny::removeResourcePath("www")

    # $$ test does not work using RCHECK but only when called via testthat::test_file(path = "tests/testthat/test-run_app.R")

    #app <- shinytest2::AppDriver$new(eCerto::run_app(options = list("test.mode" = TRUE)), name = "run_app", load_timeout = 45*1000, timeout = 12*1000, seed = 1234, width = 1920, height = 1080, view = TRUE)
    #
    # # get initial app values
    # init_vals <- app$get_values()
    #
    # # check if modules/components are named such that function 'to_startPage' still works
    # testthat::expect_true("Start-excelfile-moduleSelect" %in% names(init_vals$input))
    #
    # # Check that test data load button is still present and with consistent name
    # testthat::expect_true("Start-Rdatain-load_test_data" %in% names(init_vals$input))
    #
    # # check if empty R6 object was initialized
    # testthat::expect_true(identical(init_vals$export$`rv`$c_analytes(), list()))
    #
    # # check if loading test data works
    # app$click(input = "Start-Rdatain-load_test_data")
    # test <- app$get_values(export = "rv")$export$rv
    # testthat::expect_equal(unname(test$c_analytes()), c("X", "Y", "Z"))

  }
)
