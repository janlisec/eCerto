test_that("Boxplot-View toggled saved in rv", 
  code = {
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    observe({setValue(rv_test, c("Certifications","data"), test_ExcelUP()) })
    observe({set_uploadsource(rv_test, "Certifications", uploadsource = "Excel") })
    datreturn = reactiveClass$new(init_datreturn()) # initiate runtime variables
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
                          print(getValue(rv,"Certifications"))
                          print(input$certification_view)
                        }
      )
    # )
})
