# testthat::test_file(path = "tests/testthat//test-app_utils.R")
testthat::test_that(
  desc = "xlsxSheetNames returns sheet names of excel files",
  code = {
    # expect warning for non xlsx files
    testthat::expect_warning(eCerto:::xlsxSheetNames(tempfile()))
    # Create sample data and write to temp xlsx file
    dfs <- list(
      "S1" = data.frame("A"=1:3),
      "S2" = data.frame("A"=1:3, "B"=2:4)
    )
    path_xlsx <- tempfile(fileext = ".XlSX")
    openxlsx::write.xlsx(dfs, path_xlsx)
    # check that correct sheet names are returned
    testthat::expect_equal(eCerto:::xlsxSheetNames(path_xlsx), c("S1", "S2"))
    # Create a second sample data file with identical sheet names
    path_xlsx2 <- tempfile(fileext = ".XlSX")
    openxlsx::write.xlsx(dfs, path_xlsx2)
    # check that correct sheet names are returned if multiple files contain identical names
    testthat::expect_equal(eCerto:::xlsxSheetNames(c(path_xlsx, path_xlsx2)), c("S1", "S2"))
    # Create a second sample data file with differing sheet names
    names(dfs)[2] <- "S_Err"
    openxlsx::write.xlsx(dfs, path_xlsx2)
    # check if sheet names are different between multiple files
    testthat::expect_warning(eCerto:::xlsxSheetNames(c(path_xlsx, path_xlsx2)))
  }
)

testthat::test_that(
  desc = "getValue: empty key should return reactive thing",
  code = {
    lz <- list(a1 = list(b1 = "Streusalz", b2 = "Andreas Scheuer"), a2 = "Wurst")
    lz_e <- eCerto::eCerto$new(do.call(shiny::reactiveValues, lz))
    k <- c("a1", "b1") # keys
    testthat::expect_equal(class(eCerto::getValue(lz_e, NULL)), "reactivevalues")
    testthat::expect_equal(eCerto::getValue(lz, "a2"), "Wurst")
    testthat::expect_error(eCerto::getValue("x"))
    testthat::expect_error(eCerto::setValue("x"))
  }
)

testthat::test_that(
  desc = "listNames accepts R6 objects",
  code = {
    rv <- eCerto$new(eCerto:::init_rv()) # initiate persistent variables
    nms = shiny::isolate(eCerto:::listNames(l = rv))
    testthat::expect_gte(length(nms), 2)
    testthat::expect_equal(class(nms), "character")
  }
)

testthat::test_that(
  desc = "show_view returns CertValPlot as visible panel",
  code = {
    rv <- eCerto$new(eCerto:::init_rv()) # initiate persistent variables
    shiny::isolate({setValue(rv, c("Certification_processing","CertValPlot","show"),TRUE) })
    testthat::expect_equal(eCerto:::show_view(rv), "CertValPlot")
  }
)

testthat::test_that(
  desc = "welcome_screen: returns a TagList",
  code = {
    testthat::expect_true(inherits(eCerto:::welcome_screen(id = "test"), "shiny.tag.list"))
  }
)

testthat::test_that(
  desc = "HTML2markdown works",
  code = {
    x <- c("x<sub>i</sub>", "This is <i>formatted</i> <b>HTM<sup>L</sup></b>")
    testthat::expect_true(is.character(eCerto:::HTML2markdown(x)))
  }
)
