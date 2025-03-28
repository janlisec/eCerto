testthat::test_that("'show_help' renders Rmd files correctly", {
  testthat::skip_on_cran()
  fn <- system.file("app/www/rmd/start_gethelp.Rmd", package = "eCerto")
  testthat::expect_true(file.exists(fn))
  suppressMessages({
    # returns a tag.list
    out <- eCerto:::show_help(filename = fn, show_modal = FALSE)
    testthat::expect_true(inherits(out, "shiny.tag.list"))
    # returns error as it can not open a modal
    testthat::expect_error(eCerto:::show_help(filename = fn, show_modal = TRUE))
  })
  # returns error if no valid file
  if ("www" %in% names(shiny::resourcePaths())) shiny::removeResourcePath("www")
  testthat::expect_error(eCerto:::show_help(filename = "bla", show_modal = FALSE))
})
