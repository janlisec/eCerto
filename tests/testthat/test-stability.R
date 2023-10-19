testthat::test_that(
  desc = "Stability$s_vals correctly initiated",
  code = {
    rv_test <- eCerto::eCerto$new(eCerto:::init_rv())
    suppressMessages(
      shiny::testServer(
        app = eCerto:::page_StabilityServer,
        args = list(
          rv = rv_test
        ),
        expr =  {
          eCerto::setValue(rv, c("Stability","data"), eCerto:::test_Stability_Excel() )
          session$setInputs("s_shelf_life" = 60)
          session$setInputs("slope_of_means" = TRUE)
          session$flushReact()
          s_vals_cmp_month <- structure(
            list(
              analyte = structure(1:2, levels = c("Mn", "Si"), class = "factor"),
              mon_diff = c(88.3, 88.3),
              slope = c(-0.000673493626114624, -1.95873570319798e-05),
              SE_slope = c(0.00367471067431989, 4.73816331290521e-06),
              t_cert = c(60, 60),
              u_stab = c(0.220482640, 0.0002842898),
              P = c(0.855321094344113, 0.000135937030629598)),
            class = "data.frame", row.names = c(NA, -2L)
          )
          testthat::expect_equal(eCerto::getValue(rv,c("Stability","s_vals")), s_vals_cmp_month)
        }
      )
    )
  }
)

testthat::test_that(
  desc = "Correct error if no Certification has been uploaded yet",
  code = {
    rv_test <- eCerto::eCerto$new(eCerto:::init_rv())
    suppressMessages(
      shiny::testServer(
        app = eCerto:::page_StabilityServer,
        args = list(
          rv = rv_test
        ),
        expr =  {
          eCerto::setValue(rv_test, c("Stability","data"), eCerto:::test_Stability_Excel() )
          testthat::expect_error(output$s_transfer_ubb, "Please upload certification data to transfer Uncertainty values")
        }
      )
    )
  }
)

testthat::test_that(
  desc = "Correct Warnings if no C data or no U column are available",
  code = {
    rv_test <- eCerto::eCerto$new(eCerto:::init_rv())
    suppressMessages(
      shiny::testServer(
        app = eCerto:::page_StabilityServer,
        args = list(
          rv = rv_test
        ),
        expr =  {
          eCerto::setValue(rv_test, c("Stability","data"), eCerto:::test_Stability_Excel() )
          session$flushReact()
          testthat::expect_error(output$s_transfer_ubb, "Please upload certification data to transfer Uncertainty values")
          eCerto::setValue(rv, c("General","materialtabelle"), eCerto:::init_materialtabelle("Si"))
          session$flushReact()
          testthat::expect_error(output$s_transfer_ubb, "Please specify a U column in material table to transfer Uncertainty values")
        }
      )
    )
  }
)
