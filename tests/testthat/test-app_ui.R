testthat::test_that(
  desc = "app ui",
  code = {
    ui <- eCerto:::app_ui
    golem::expect_shinytaglist(ui())
    # Check that formals have not been removed
    fmls <- formals(ui)
    for (i in c("request")) {
      testthat::expect_true(i %in% names(fmls))
    }
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
