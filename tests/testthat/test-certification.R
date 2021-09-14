test_that(
  desc = "Boxplot-View toggled saved in rv",
  code = {
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    # shiny::isolate({ecerto::setValue(rv_test, c("Certification","data"), ecerto:::test_Certification_Excel()) })
    # shiny::isolate({ecerto::setValue(rv_test, c("Certification","uploadsource"), "Excel") })
    datreturn <- ecerto::reactiveClass$new(ecerto::init_datreturn()) # initiate runtime variables
    suppressMessages(
      shiny::testServer(
        app = m_CertificationServer,
        args = list(
          rv = rv_test,
          # apm.input = reactiveVal(),
          datreturn = datreturn
        ),
        expr =  {
          ecerto::setValue(rv_test, c("Certification","data"), ecerto:::test_Certification_Excel())
          ecerto::setValue(rv, c("Certification","uploadsource"), "Excel")
          session$flushReact()
          # testthat::expect_equal(input$certification_view,NULL)
          session$setInputs(certification_view=c("boxplot","stats"))
          # testthat::expect_equal(input$certification_view,c("boxplot","stats"))
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
#     rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
#     # shiny::isolate({ecerto::setValue(rv_test, c("Certification","data"), ecerto:::test_Certification_Excel()) })
#     # shiny::isolate({ecerto::setValue(rv_test, c("Certification","uploadsource"), "Excel") })
#     datreturn <- ecerto::reactiveClass$new(ecerto::init_datreturn()) # initiate runtime variables
#     # suppressMessages(
#       shiny::testServer(
#         app = m_CertificationServer,
#         args = list(
#           rv = rv_test,
#           datreturn = datreturn
#         ),
#         expr =  {
#           ecerto::setValue(rv_test, c("Certification","data"), ecerto:::test_Certification_Excel())
#           ecerto::setValue(rv, c("Certification","uploadsource"), "Excel")
#           session$flushReact()
#           # testthat::expect_equal(input$certification_view,NULL)
#           session$setInputs(flt_labs = "L2", selected_tab("Si"))
#           session$flushReact()
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
    test_apm = analyte_parameter_list(ecerto:::test_Certification_Excel())
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
          ecerto::setValue(rv_test, c("General","apm"), test_apm)
          ecerto::setValue(rv, c("Certification","uploadsource"), "RData")
          session$flushReact()
          expect_equal(apm(), getValue(rv_test, c("General","apm")))

        }
      )
    )
  }
)
