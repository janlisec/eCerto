testthat::test_that("help_the_user renders Rmd files correctly", {
  fn <- system.file("app/www/rmd/start_gethelp.Rmd", package = "eCerto")
  testthat::expect_true(file.exists(fn))
  suppressMessages({
    # returns a tag.list
    out <- eCerto:::help_the_user_modal(filename = fn, show_modal = FALSE)
    testthat::expect_true(inherits(out, "shiny.tag.list"))
    # returns error as it can not open a modal
    testthat::expect_error(eCerto:::help_the_user_modal(filename = fn, show_modal = TRUE))
  })
  # returns error if no valid file
  if ("www" %in% names(resourcePaths())) removeResourcePath("www")
  testthat::expect_error(eCerto:::help_the_user_modal(filename = "bla", show_modal = FALSE))
})
