testthat::test_that(
  desc = "Init of materialtable after Certifications uploaded",
  code = {
    testthat::local_edition(3)
    suppressMessages({
      rv <- eCerto:::test_rv() # load test data
      shiny::testServer(
        app = eCerto:::m_materialtabelleServer,
        args = list(rv = rv),
        expr = {
          mt <- eCerto::getValue(rv, c("General", "materialtabelle"))
          testthat::expect_identical(unname(rv$c_analytes()), c("Si", "Fe", "Cu"))
          testthat::expect_identical(nrow(mt), 3L)
          testthat::expect_identical(colnames(mt), c("analyte", "mean", "cert_val", "sd", "n", "u_char", "u_com", "k", "U", "U_abs", "unit"))
          testthat::expect_equal(unname(cert_sd()), 0.004331029, tolerance = 10^-6)
          testthat::expect_equal(unname(cert_mean()), 0.0484375, tolerance = 10^-6)
        }
      )
    })
  }
)
