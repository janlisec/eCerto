testthat::test_that(
  desc = "m_arrheniusServer works on package testdata",
  code = {
    # set up eCerto R6 object containing data from an arrhenius style experiment
    rv <- eCerto:::test_rv(type = "SR3")
    shiny::isolate(eCerto::setValue(rv, c("Stability", "data"), eCerto:::test_Stability_Arrhenius()))
    mt <- data.frame("analyte"="a1", "cert_val"=10, "U_abs"=1)
    shiny::isolate(eCerto::setValue(rv, c("General", "materialtabelle"), mt))

    # provide this test data to the server function and test if the expected outputs are generated
    shiny::testServer(
      app = eCerto:::m_arrheniusServer,
      args = list(
        id = "test",
        rv = rv
      ),
      expr = {
        session$setInputs(
          analyte = "a1",
          rbtn_storage = "rt"
        )
        testthat::expect_equal(an(), "a1")
        x <- tab1exp()
        testthat::expect_true(is.data.frame(x))
        testthat::expect_equal(nrow(x),3)
        testthat::expect_equal(ncol(x), 9)

        if (FALSE) {
          # this part should test for a missing value in input data but reactivity is not working
          tmp <- eCerto:::test_Stability_Arrhenius()
          tmp[1,"Value"] <- NA
          eCerto::setValue(rv, c("Stability", "data"), tmp)
          session$flushReact()
          head(eCerto::getValue(rv, c("Stability", "data")))
          head(df())
        }
        session$setInputs(
          rbtn_storage = "mt"
        )
        session$setInputs(
          num_coef = 1,
          user_temp = 23
        )
        testthat::expect_true(grepl("At the specified temperature of 23", output$user_month$html))
      }
    )
})
