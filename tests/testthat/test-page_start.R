# testthat::test_file(path = "tests/testthat/test-page_start.R")
testthat::test_that(
  desc = "page_start works",
  code = {
    # Don't run these tests on the CRAN build servers
    testthat::skip_on_cran()
    testthat::skip_on_covr()
    testthat::skip_on_ci()

    # remove resource path 'www' to get consistent snapshots
    if ("www" %in% names(shiny::resourcePaths())) shiny::removeResourcePath("www")

    # put a small test app together calling the module
    rv <- eCerto::eCerto$new(eCerto:::init_rv()) # initiate persistent variables
    test_app <- shiny::shinyApp(
     ui = shiny::fluidPage(
       eCerto:::page_startUI(id = "test")
     ),
     server = function(input, output, session) {
       eCerto:::page_startServer(id = "test", rv = rv)
     },
     options = list("test.mode" = TRUE)
    )

    # run this test app in a headless browser using shinytest2
    app <- shinytest2::AppDriver$new(test_app, name = "test_app_page_start")

    # clicking on load test data led to tests to fail
    app$click("test-Rdatain-load_test_data")

    # check if module did start by comparing with previously recorded snapshot
    app$expect_values()

    # covr does not work with shinytest2 currently according to this reported issue:
    # https://github.com/rstudio/shinytest2/issues/268
    # terminate Shiny to allow covr to calculate the code coverage
    # p <- app$.__enclos_env__$private$shinyProcess
    # p$interrupt()
    # p$wait()
  }
)
