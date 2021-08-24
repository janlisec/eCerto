test_that(
  desc = "Stability$s_vals correctly initiated",
  code = {
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    suppressMessages(
    shiny::testServer(
      app = m_StabilityServer,
      args = list(
        rv = rv_test
      ),
      expr =  {
        ecerto::setValue(rv_test, c("Stability","data"), ecerto:::test_Stability_Excel() )
        ecerto::setValue(rv_test, c("Stability","uploadsource"), "Excel")
        session$flushReact()
        # testthat::expect_equal(input$certification_view,NULL)
        s_vals_cmp = structure(
          list(
            analyte = structure(1:2, .Label = c("Mn", "Si"), class = "factor"),
            mon_diff = c(88, 88),
            slope = c(-2.2127100652636e-05, -6.43527130414118e-07),
            SE_slope = c(0.000120729714146033, 1.55668609869576e-07),
            U_Stab = c(-2.67139853667325e-09, -1.00176973804923e-13)),
          class = "data.frame", row.names = c(NA,-2L)
        )
        #browser()
        expect_equal(getValue(rv,c("Stability","s_vals")),s_vals_cmp)
      }
      )
    )
  })

# Correct Warning if no U column is available in materialtabelle
# Correct output of module after Transfer-Button
