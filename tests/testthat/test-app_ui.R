testthat::test_that(
  desc = "app ui",
  code = {
    # avoid rendering of Help page in this test
    old_val <- getOption("eCerto.renderHelp")
    options(eCerto.renderHelp = FALSE)
    ui <- eCerto:::app_ui
    golem::expect_shinytaglist(ui())
    # Check that formals have not been removed
    fmls <- formals(ui)
    for (i in c("request")) {
      testthat::expect_true(i %in% names(fmls))
    }
    options(eCerto.renderHelp = old_val)
  }
)

testthat::test_that(
  desc = "app server",
  code = {
    server <- eCerto:::app_server
    testthat::expect_type(server, "closure")
    # Check that formals have not been removed
    fmls <- formals(server)
    for (i in c("input", "output", "session")) {
      testthat::expect_true(i %in% names(fmls))
    }
  }
)
