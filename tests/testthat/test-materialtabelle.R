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
          mt <- eCerto::getValue(rv, c("General","materialtabelle"))
          testthat::expect_equal(unname(rv$c_analytes()), c("Si","Fe","Cu"))
          testthat::expect_equal(nrow(mt), 3)
          testthat::expect_equal(colnames(mt), c("analyte", "mean", "cert_val", "sd", "n", "u_char", "u_com", "k", "U", "U_abs", "unit"))
          testthat::expect_equal(unname(cert_sd()), 0.004331029)
          testthat::expect_equal(unname(cert_mean()), 0.0484375)
        }
      )
    })
  }
)
