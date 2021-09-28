test_that(
  desc = "Boxplot-View toggled saved in rv",
  code = {
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    datreturn <- ecerto::reactiveClass$new(ecerto::init_datreturn()) # initiate runtime variables
    suppressMessages(
      shiny::testServer(
        app = m_CertificationServer,
        args = list(
          rv = rv_test,
          datreturn = datreturn
        ),
        expr =  {
          ecerto::setValue(rv_test, c("Certification","data"), ecerto:::test_Certification_Excel())
          ecerto::setValue(rv, c("Certification","uploadsource"), "Excel")
          session$flushReact()
          # nothing is selected at start
          testthat::expect_equal(input$certification_view, NULL)
          # all expected selections can be made
          session$setInputs(certification_view=c("dataview", "stats", "CertValPlot"))
          testthat::expect_equal(input$certification_view,c("dataview","stats","CertValPlot"))
          # the rv object is updated accordingly
          testthat::expect_equal(
            ecerto::getValue(rv,c("Certification_processing","CertValPlot","show")),
            TRUE
          )
        }
      )
    )
  }
)

# test_that(
#   desc = "Deselecting of last lab filter is working",
#   code = {
#     # initiate runtime variables
#     rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
#     datreturn <- ecerto::reactiveClass$new(ecerto::init_datreturn())
#     # suppressMessages(
#       shiny::testServer(
#         app = m_CertificationServer,
#         args = list(
#           rv = rv_test,
#           datreturn = datreturn
#         ),
#         expr = {
#           ecerto::setValue(rv_test, c("Certification","data"), ecerto:::test_Certification_Excel())
#           ecerto::setValue(rv, c("Certification","uploadsource"), "Excel")
#           session$setInputs(flt_labs = "L2")
#           testthat::expect_equal(input$flt_labs, "L2")
#           session$flushReact()
#           browser()
#           getValue(rv,c("General","apm"))
#           print(apm())
#           # testthat::expect_equal(
#           #   ecerto::getValue(rv,c("Certification_processing","CertValPlot","show")),
#           #   TRUE
#           # )
#         }
#       )
#     # )
#   }
# )

test_that(
  desc = "RData Upload for apm saved in variable",
  code = {
    # initiate runtime variables
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    datreturn <- ecerto::reactiveClass$new(ecerto::init_datreturn())
    apm_test <- ecerto::init_apm(ecerto:::test_Certification_Excel())
    suppressMessages(
      shiny::testServer(
        app = m_CertificationServer,
        args = list(
          rv = rv_test,
          datreturn = datreturn
        ),
        expr =  {
          ecerto::setValue(rv_test, c("General","apm"), apm_test)
          ecerto::setValue(rv, c("Certification","uploadsource"), "RData")
          session$flushReact()
          expect_equal(apm(), getValue(rv_test, c("General","apm")))
        }
      )
    )
  }
)
