test_that("loading functions decline other filetypes as excel", {
  # Create sample data
  df <- tibble::tibble(x = 1, y = 2)
  path_csv <- tempfile()
  write.csv(df, path_csv, row.names = FALSE)

  fpath = paste0(path_csv,"test.csv")
  expect_error(ecerto::load_sheetnames(fpath))
  expect_error(ecerto::load_excelfiles(fpath, 1))
})

# get/set -----------------------------------------------------------------

testthat::test_that("getValue: empty key should return reactive thing",{
  lz = list(a1=list(b1 = "Streusalz",b2 = "Andreas Scheuer"), a2 = "Wurst")
  lz = ecerto::reactiveClass$new(do.call(shiny::reactiveValues,lz))
  k = c("a1","b1") # keys
  expect_equal(class(ecerto::getValue(lz,NULL)),"reactivevalues")
})
