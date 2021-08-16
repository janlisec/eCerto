local_edition(3)





# Certification Test ------------------------------------------------------



test_that("Successful Upload test",code = {
  excelformat_test = shiny::reactiveVal("Certification")
  check = shiny::reactiveVal(TRUE)
  xlsx_test = list(
    datapath = c(
      system.file(package = "ecerto", "extdata","Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"),
      system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"),
      system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
    ),
    name = c(
      "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx",
      "Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx",
      "Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
  )
  shiny::testServer(
    app = m_ExcelUploadControl_Server,
    args = list(excelformat=excelformat_test, check = check),
    expr =  {
      session$setInputs(excel_file = xlsx_test, sheet_number = 1) # without row and column selection unfortunately
      session$setInputs(go = "click")
      print(out())
      # expect_true("File" %in% colnames(out()[[1]]))
      
    }
  )
})



# Homogeneity Upload -------------------------------------------------------------



test_that("Successful Homogeneity Upload test",code = {
  xlsx_test2 = list(
    datapath = system.file(package = "ecerto", "extdata","Homog_test.xlsx"),
    name = "Homog_test.xlsx"
  )
  excelformat_test = shiny::reactiveVal("Homogeneity")
  dat_test <- shiny::reactiveVal(FALSE)
  
  shiny::testServer(app = m_ExcelUploadControl_Server,
    args = list(excelformat=excelformat_test, check = dat_test),
    expr =  {
      # suppressMessages(
        session$setInputs(excel_file = xlsx_test2, sheet_number = 1) # without row and column selection
      # )

      # expect_snapshot(rv_xlsx_range_select$tab_flt)
      session$setInputs(go = "click")
      # expect_message(ex(), "go clicked")
      expect_snapshot(session$returned())
    }
  )
})

