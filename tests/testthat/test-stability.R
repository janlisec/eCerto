testthat::test_that(
  desc = "Stability$s_vals correctly initiated",
  code = {
    rv_test <- eCerto::reactiveClass$new(eCerto::init_rv())
    suppressMessages(
      shiny::testServer(
        app = eCerto::page_StabilityServer,
        args = list(
          rv = rv_test
        ),
        expr =  {
          eCerto::setValue(rv_test, c("Stability","data"), eCerto:::test_Stability_Excel() )
          eCerto::setValue(rv_test, c("Stability","uploadsource"), "Excel")
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
          testthat::expect_equal(getValue(rv,c("Stability","s_vals")),s_vals_cmp)
        }
      )
    )
  }
)

testthat::test_that(
  desc = "Correct error if no Certification has been uploaded yet",
  code = {
    rv_test <- eCerto::reactiveClass$new(eCerto::init_rv())
    suppressMessages(
      shiny::testServer(
        app = eCerto::page_StabilityServer,
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
    rv_test <- eCerto::reactiveClass$new(eCerto::init_rv())
    suppressMessages(
      shiny::testServer(
        app = eCerto::page_StabilityServer,
        args = list(
          rv = rv_test
        ),
        expr =  {
          eCerto::setValue(rv_test, c("Stability","data"), eCerto:::test_Stability_Excel() )
          session$flushReact()
          testthat::expect_error(output$s_transfer_ubb, "Please upload certification data to transfer Uncertainty values")
          eCerto::setValue(rv, c("General","materialtabelle"), eCerto::init_materialTabelle("Si"))
          session$flushReact()
          testthat::expect_error(output$s_transfer_ubb, "Please specify a U column in material table to transfer Uncertainty values")
        }
      )
    )
  }
)
