testthat::test_that(
  desc = "loading functions decline other filetypes as excel",
  code = {
    # Create sample data and write to temp csv file
    df <- tibble::tibble(x = 1, y = 2)
    path_csv <- tempfile()
    write.csv(df, path_csv, row.names = FALSE)
    # check that error messages are created on selection
    fpath <- paste0(path_csv,"test.csv")
    testthat::expect_error(eCerto::load_sheetnames(fpath))
    testthat::expect_error(eCerto::load_excelfiles(fpath, 1))
  }
)

testthat::test_that(
  desc = "getValue: empty key should return reactive thing",
  code = {
    lz = list(a1=list(b1 = "Streusalz", b2 = "Andreas Scheuer"), a2 = "Wurst")
    lz = eCerto::eCerto$new(do.call(shiny::reactiveValues, lz))
    k = c("a1","b1") # keys
    testthat::expect_equal(class(eCerto::getValue(lz, NULL)), "reactivevalues")
  }
)

testthat::test_that(
  desc = "listNames accepts R6 objects",
  code = {
    rv <- eCerto$new(init_rv()) # initiate persistent variables
    nms = shiny::isolate(listNames(l = rv))
    testthat::expect_gte(length(nms), 2)
    testthat::expect_equal(class(nms), "character")
  }
)

testthat::test_that(
  desc = "show_view returns CertValPlot as visible panel",
  code = {
    rv <- eCerto$new(init_rv()) # initiate persistent variables
    shiny::isolate({setValue(rv, c("Certification_processing","CertValPlot","show"),TRUE) })
    testthat::expect_equal(show_view(rv), "CertValPlot")
  }
)