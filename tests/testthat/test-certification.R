testthat::test_that(
  desc = "Boxplot-View toggled saved in rv",
  code = {
    rv <- eCerto:::test_rv()
    suppressMessages(
      shiny::testServer(
        app = page_CertificationServer,
        args = list(
          rv = rv
        ),
        expr = {
          session$flushReact()
          # nothing is selected at start
          testthat::expect_equal(input$certification_view, NULL)
          # all expected selections can be made
          session$setInputs(certification_view=c("dataview", "stats", "CertValPlot"))
          testthat::expect_equal(input$certification_view,c("dataview","stats","CertValPlot"))
          # the rv object is updated if Fig_width parameter is modified
          session$setInputs(Fig01_width=450)
          testthat::expect_equal(eCerto::getValue(rv, c("Certification_processing","CertValPlot","Fig01_width")), 450)
        }
      )
    )
  }
)

# testthat::test_that(
#   desc = "Deselecting of last lab filter is working",
#   code = {
#     # initiate runtime variables
#     rv_test <- eCerto::reactiveClass$new(eCerto::init_rv())
#     datreturn <- eCerto::reactiveClass$new(eCerto::init_datreturn())
#     # suppressMessages(
#       shiny::testServer(
#         app = page_CertificationServer,
#         args = list(
#           rv = rv_test,
#           datreturn = datreturn
#         ),
#         expr = {
#           eCerto::setValue(rv_test, c("Certification","data"), eCerto:::test_Certification_Excel())
#           eCerto::setValue(rv, c("Certification","uploadsource"), "Excel")
#           session$setInputs(flt_labs = "L2")
#           testthat::expect_equal(input$flt_labs, "L2")
#           session$flushReact()
#           browser()
#           getValue(rv,c("General","apm"))
#           print(apm())
#           # testthat::expect_equal(
#           #   eCerto::getValue(rv,c("Certification_processing","CertValPlot","show")),
#           #   TRUE
#           # )
#         }
#       )
#     # )
#   }
# )

testthat::test_that(
  desc = "RData Upload for apm saved in variable",
  code = {
    # initiate runtime variables
    rv_test <- eCerto::reactiveClass$new(eCerto::init_rv())
    apm_test <- eCerto::init_apm(eCerto:::test_Certification_Excel())
    suppressMessages(
      shiny::testServer(
        app = page_CertificationServer,
        args = list(
          rv = rv_test
        ),
        expr =  {
          eCerto::setValue(rv_test, c("General","apm"), apm_test)
          session$flushReact()
          testthat::expect_equal(apm(), getValue(rv_test, c("General","apm")))
        }
      )
    )
  }
)
