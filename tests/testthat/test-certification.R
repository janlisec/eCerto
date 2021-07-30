test_that("Boxplot-View toggled saved in rv", 
  code = {
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    shiny::isolate({ecerto::setValue(rv_test, c("Certification","data"), ecerto:::test_ExcelUP()) })
    shiny::isolate({ecerto::setValue(rv_test, c("Certification","uploadsource"), uploadsource = "Excel") })
    datreturn = ecerto::reactiveClass$new(init_datreturn()) # initiate runtime variables
    # suppressMessages(
      shiny::testServer(app = m_CertificationServer,
                        args = list(
                          rv = rv_test,
                          apm.input = reactiveVal(),
                          datreturn = datreturn
                        ),
                        expr =  {
                          session$flushReact()
                          session$flushReact()
                          print(getValue(rv,"Certification"))
                          print(input$certification_view)
                        }
      )
    # )
})
