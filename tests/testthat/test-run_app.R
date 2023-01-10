# Don't run these tests on the CRAN build servers
testthat::skip_on_cran()

# remove resource path 'www' to get consistent snapshots
if ("www" %in% names(shiny::resourcePaths())) shiny::removeResourcePath("www")

# run this test app in a headless browser using shinytest2
app <- shinytest2::AppDriver$new(eCerto::run_app(), name = "run_app")

# get initial app values
init_vals <- app$get_values()

testthat::test_that(
  desc = "modules/components are named consistently such that function 'to_startPage' still works",
  code = {

    # check if modules/components are named such that function 'to_startPage' still works
    testthat::expect_true("Start-excelfile-moduleSelect" %in% names(init_vals$input))

  }
)

testthat::test_that(
  desc = "R6 object is initialized empty and filled with test data upon user click",
  code = {

    # check if empty R6 object was initialized
    testthat::expect_true(identical(init_vals$export$`rv`$c_analytes(), list()))

    # check if loading test data works
    app$click(input = "Start-load_test_data")
    test <- app$get_values(export = "rv")$export$rv
    testthat::expect_equal(unname(test$c_analytes()), c("X", "Y", "Z"))

  }
)