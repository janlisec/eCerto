testthat::test_that("styleTabD works", {
  suppressMessages({
    fl <- system.file("extdata", "drmd", "BAM-M375a.xml", package = "eCerto")
    lst <- eCerto:::read_drmd_xml(fl)
    tab <- eCerto:::flatten_list_to_df(lst)
  })

  # tab D2
  out_dt <- eCerto:::styleTabD2(df = tab, selected = NULL, interact_ele = FALSE)
  testthat::expect_true(inherits(out_dt, "datatables"))

  # tab D3
  out_dt <- eCerto:::styleTabD3(df = tab)
  testthat::expect_true(inherits(out_dt, "datatables"))
})
