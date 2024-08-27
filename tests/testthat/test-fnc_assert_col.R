testthat::test_that(
  desc = "assert_col works",
  code = {

    x <- data.frame(
      "analyte" = c("A", "B"),
      "tmp" = rep(0L, 2),
      "unit" = c("x", "y")
    )

    # test intended behaviour
    testthat::expect_true(is.factor(eCerto::assert_col(df = x, name = "analyte", pos = 1, type = "factor")[,1]))
    testthat::expect_true(colnames(eCerto::assert_col(df = x, name = "Analyte", pos = 3, type = "character"))[3]=="Analyte")
    testthat::expect_true(colnames(eCerto::assert_col(df = x, name = " Analyte", pos = 2, type = "factor"))[2]==" Analyte")
    testthat::expect_true(is.factor(eCerto::assert_col(df = x, name = " Analyte", pos = 2, type = "factor")[,2]))
    testthat::expect_true(ncol(eCerto::assert_col(df = x, name = "Analyte", pos = 2, type = "factor", fuzzy_name = FALSE))==4)
    testthat::expect_true(all(eCerto::assert_col(df = x, name = "test", type = "factor", default_value = "test")[,"test"]=="test"))

    # this will lead to NAs in column unit because the conversion does not lead to an error
    # hence the default value is not used
    testthat::expect_warning(eCerto::assert_col(df = x, name = "unit", type = "numeric", default_value = 10))

    # this will lead to the specified default data (today date) in column unit
    # because the conversion attempt does lead to an error (the error message will be attached)
    testthat::expect_true(class(eCerto::assert_col(df = x, name = "unit", type = "Date")[,"unit"])=="Date")
    testthat::expect_true(as.character(eCerto::assert_col(df = data.frame("test" = "2022-03-31"), name = "test", type = "Date")[,"test"])=="2022-03-31")

  }
)