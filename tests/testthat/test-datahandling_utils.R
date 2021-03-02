test_that("loading functions decline other filetypes as excel", {
  # Create sample data
  df <- tibble::tibble(x = 1, y = 2)
  path_csv <- tempfile()
  write.csv(df, path_csv, row.names = FALSE)

  fpath = paste0(path_csv,"test.csv")
  expect_error(load_sheetnames(fpath))
  expect_error(load_excelfiles(fpath, 1))
})