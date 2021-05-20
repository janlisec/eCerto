

# Test 1 ---------------------------------------------------------------

dr = init_datreturn()

test_that("Button not able to click before init of materialtable",code = {
  testServer(.TransferHomogeneityServer,
             args = list(datreturn=dr), {
               print(input$h_transfer_ubb_button)
               # expect_message(
               #   session$flushReact(), "add File column")
               # # has File been added correctly after Upload
               # expect_true("File" %in% colnames(rv$tab_flt[[1]]))
               # 
               # # set rows and columns selection
               # suppressMessages(
               #   session$setInputs(uitab_cells_selected = cells_selected)
               # )
               # session$flushReact()
               # expect_snapshot(rv$tab_flt)
               # expect_equal(rv$end_col,6)
             }
  )
})