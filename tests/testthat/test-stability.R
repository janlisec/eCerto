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
              mean = c(97.4266711538462, 0.233713461538462),
              u_stab = c(0.00226306244325058, 0.00121640318406532),
              P = c(0.855321094344113, 0.000135937030629598)),
            class = "data.frame", row.names = c(NA, -2L)
          )
          testthat::expect_equal(eCerto::getValue(rv,c("Stability","s_vals")), s_vals_cmp_month)
        }
      )
    )
  }
)
