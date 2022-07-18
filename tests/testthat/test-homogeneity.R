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
  desc = "H analytes being found or not in C data",
  code = {
    suppressMessages({
      x <- eCerto:::prepTabH1(x = eCerto:::test_homog()$data)
      o1 <- eCerto:::styleTabH1(x = x)
      testthat::expect_equal(
        as.character(o1[,"style_analyte"]),
        rep("red", each=4)
      )
      mt <- data.frame("analyte"="Fe")
      o2 <- eCerto:::styleTabH1(x = x, mt = mt)
      testthat::expect_equal(
        as.character(o2[,"style_analyte"]),
        rep(c("black","red"), each=2)
      )
      apm <- list("Fe"=list("precision"=2))
      o3 <- eCerto:::styleTabH1(x = x, apm = apm)
      testthat::expect_equal(
        o3[,"mean"],
        c("0.29","0.29","0.2905","0.2935")
      )
    })
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
