testthat::test_that(
  desc = "Reactivity in C module is working",
  code = {
    suppressMessages({
      rv <- eCerto:::test_rv()
      shiny::testServer(
        app = eCerto:::page_CertificationServer,
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
          #session$setInputs(Fig01_height=450)
          #testthat::expect_equal(eCerto::getValue(rv, c("Certification_processing","CertValPlot","Fig01_height")), 450)
        }
      )
    })
  }
)