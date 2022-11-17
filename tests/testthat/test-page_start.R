testthat::test_that("page_start works", {
  # Don't run these tests on the CRAN build servers
  testthat::skip_on_cran()

  # put a small test app together calling the module
  test_app <- shiny::shinyApp(
   ui = shiny::fluidPage(
     eCerto:::page_startUI(id = "test")
   ),
   server = function(input, output, session) {
     rv <- eCerto::eCerto$new(eCerto::init_rv()) # initiate persistent variables
     eCerto:::page_startServer(id = "test", rv = rv)
   }
  )

  # run this test app in a headless browser using shinytest2
  app <- shinytest2::AppDriver$new(test_app, name = "page_start works")

  # check if module did start by comparing with previously recorded snapshot
  app$expect_values()

})
