test_that(
  desc = "Stability$s_vals correctly initiated",
  code = {
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    suppressMessages(
      shiny::testServer(
        app = ecerto::page_StabilityServer,
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
          expect_equal(getValue(rv,c("Stability","s_vals")),s_vals_cmp)
        }
      )
    )
  })

test_that(
  desc = "Correct error if no Certification has been uploaded yet",
  code = {
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    datreturn_test = ecerto::reactiveClass$new(init_datreturn())
    suppressMessages(
      shiny::testServer(
        app = ecerto::page_StabilityServer,
        args = list(
          rv = rv_test,
          datreturn = datreturn_test
        ),
        expr =  {
          ecerto::setValue(rv_test, c("Stability","data"), ecerto:::test_Stability_Excel() )
          ecerto::setValue(rv_test, c("Stability","uploadsource"), "Excel")
          expect_error(output$s_transfer_ubb,"Please upload certification data to transfer Uncertainty values")
        }
      )
    )
  })

test_that(
  desc = "Correct Warning if no U column is available in materialtabelle",
  code = {
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    datreturn_test = ecerto::reactiveClass$new(init_datreturn())
    mater_table = structure(
      list(
        analyte = c("Si", "Fe", "Cu", "Mn", "Mg", "Cr", "Ni"),
        mean = c(0.0494, NA, 4.4072, 0.8092, NA, 0.0546, NA),
        cert_val = c(0.0494, 1, 13.2216, 0.8092, 1, 0.0546, 1),
        sd = c(0.0034, NA, 0.0551, 0.0022, NA, 8e-04, NA),
        n = c(3L, NA, 3L, 3L, NA, 2L, NA),
        char = c(0.0397366582033346, NA, 0.00721818838091042, 0.00156966212582449, NA, 0.0103605389184842, NA),
        com = c(0.0397366582033346, 0.015535927030583, 0.00721818838091042, 0.00156966212582449, 0.015535927030583, 5.00001073406515, 0),
        k = c(2, 2, 2, 2, 2, 2, 2),
        U = c(0.0794733164066691, 0.031071854061166, 0.0144363767618208, 0.00313932425164898, 0.031071854061166, 10.0000214681303, 0)
      ),
      row.names = c("1", "2", "3", "4", "5", "6", "7"),
      col_code = structure(list(ID = NULL, Name = NULL), row.names = NULL, class = "data.frame"), class = "data.frame"
    )
    shiny::isolate({setValue(datreturn_test,"mater_table",mater_table)})
    suppressMessages(
      shiny::testServer(
        app = ecerto::page_StabilityServer,
        args = list(
          rv = rv_test,
          datreturn = datreturn_test
        ),
        expr =  {
          ecerto::setValue(rv_test, c("Stability","data"), ecerto:::test_Stability_Excel() )
          ecerto::setValue(rv_test, c("Stability","uploadsource"), "Excel")
          expect_error(output$s_transfer_ubb,"Please specify a U column in material table to transfer Uncertainty values")
        }
      )
    )
  })

# Correct output of module after Transfer-Button
