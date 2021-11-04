# testthat::test_that(
#   desc = "Can generate H report",
#   code = {
#       app <- shinytest::ShinyDriver$new(
#         shiny::shinyApp(
#           ui = shiny::fluidPage(page_HomogeneityUI(id = "test")),
#           server = function(input, output, session) {
#             rv <- eCerto:::test_rv()
#             mt <- isolate(eCerto::getValue(rv, c("General","materialtabelle")))
#             attr(mt, "col_code") <- data.frame("ID"="U","Name"="U")
#             isolate(eCerto::setValue(rv, c("General","materialtabelle"), mt))
#             isolate(eCerto::setValue(rv, "Homogeneity", eCerto:::test_homog()))
#             page_HomogeneityServer(id = "test", rv = rv)
#           }
#         ), loadTimeout = 1e+05
#       )
#       #app$listWidgets()
#       #app$getValue("test-h_sel_analyt")
#       #app$getValue("test-h_statement2")
#       app$click(name = "test-h_Report", iotype = "output")
#   }
# )

testthat::test_that(
  desc = "H analytes being found in C data",
  code = {
    rv <- eCerto:::test_rv()
    mt <- isolate(eCerto::getValue(rv, c("General","materialtabelle")))
    attr(mt, "col_code") <- data.frame("ID"="U","Name"="U")
    isolate(eCerto::setValue(rv, c("General","materialtabelle"), mt))
    isolate(eCerto::setValue(rv, "Homogeneity", eCerto:::test_homog()))
    shiny::testServer(
      app = eCerto::page_HomogeneityServer,
      args = list(rv = rv),
      expr =  {
        session$setInputs(h_sel_analyt = "Fe.axial")
        session$flushReact()
        testthat::expect_equal(
          as.character(h_vals_print()[,"In_Cert_Module"]),
          rep(c("Yes","No"), each=2)
        )
      }
    )
  }
)

testthat::test_that(
  desc = "All objects for report are present",
  code = {
    rv <- eCerto:::test_rv()
    mt <- isolate(eCerto::getValue(rv, c("General","materialtabelle")))
    attr(mt, "col_code") <- data.frame("ID"="U","Name"="U")
    isolate(eCerto::setValue(rv, c("General","materialtabelle"), mt))
    isolate(eCerto::setValue(rv, "Homogeneity", eCerto:::test_homog()))
    shiny::testServer(
      app = eCerto::page_HomogeneityServer,
      args = list(rv = rv),
      expr =  {
        testthat::expect_true(all(c("data","h_vals") %in% names(getValue(rv, "Homogeneity"))))
        # set input value to trigger calculation of 'h_vals'
        session$setInputs(h_sel_analyt = "Fe.axial")
        session$flushReact()
        testthat::expect_true(all(sapply(c("data","h_vals"), function(x) {!is.null(getValue(rv, "Homogeneity")[[x]])})))
      }
    )
  }
)
