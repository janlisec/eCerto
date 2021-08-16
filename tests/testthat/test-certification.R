test_that(
  desc = "Boxplot-View toggled saved in rv", 
  code = {
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    # shiny::isolate({ecerto::setValue(rv_test, c("Certification","data"), ecerto:::test_ExcelUP()) })
    # shiny::isolate({ecerto::setValue(rv_test, c("Certification","uploadsource"), "Excel") })
    datreturn = ecerto::reactiveClass$new(init_datreturn()) # initiate runtime variables
    suppressMessages(
      shiny::testServer(
        app = m_CertificationServer,
        args = list(
          rv = rv_test,
          apm.input = reactiveVal(),
          datreturn = datreturn
        ),
        expr =  {
          ecerto::setValue(rv_test, c("Certification","data"), ecerto:::test_ExcelUP()) 
          ecerto::setValue(rv, c("Certification","uploadsource"), "Excel") 
          session$flushReact()
          # testthat::expect_equal(input$certification_view,NULL)
          session$setInputs(certification_view=c("boxplot","stats"))
          # testthat::expect_equal(input$certification_view,c("boxplot","stats"))
          testthat::expect_equal(
            ecerto::getValue(rv,c("Certification.processing","CertValPlot","show")),
            TRUE
          )
        }
      )
    )
  })
