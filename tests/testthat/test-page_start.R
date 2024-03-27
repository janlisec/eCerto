# testthat::test_file(path = "tests/testthat/test-page_start.R")
testthat::test_that(
  desc = "page_start works",
  code = {
    # Don't run these tests on the CRAN build servers
    testthat::skip_on_cran()

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
     }
    )

    # run this test app in a headless browser using shinytest2
    app <- shinytest2::AppDriver$new(test_app, name = "page_start")

    app$click("test-Rdatain-load_test_data")

    # check if module did start by comparing with previously recorded snapshot
    app$expect_values(export = "test")

    # terminate Shiny to allow covr to calculate the code coverage
    # p <- app$.__enclos_env__$private$shinyProcess
    # p$interrupt()
    # p$wait()
  }
)
