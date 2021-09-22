local_edition(3)

# Certification Test ------------------------------------------------------
test_that("Certification: File-column appended",code = {
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
  suppressMessages(
    shiny::testServer(
      app = m_ExcelUpload_Server,
      args = list(excelformat=excelformat_test, check = check),
      expr =  {
        session$setInputs(excel_file = xlsx_test, sheet_number = 1)
        # without row and column selection unfortunately
        rv_xlsx_range_select$tab = ecerto:::crop_dataframes(
          dfs = rv_xlsx_range_select$tab,
          rows = 8:16,
          cols = 1:5
        )
        session$setInputs(go = "click")
        expect_equal("File" %in% names(out$data), TRUE)
      }
    )
  )
})

# Homogeneity Upload -------------------------------------------------------------

test_that(desc = "Successful Homogeneity Upload test", code = {
  xlsx_test2 = list(
    datapath = system.file(package = "ecerto", "extdata","Homog_test.xlsx"),
    name = "Homog_test.xlsx"
  )
  excelformat_test = shiny::reactiveVal("Homogeneity")
  dat_test <- shiny::reactiveVal(FALSE)
  suppressMessages(
    shiny::testServer(
      app = m_ExcelUpload_Server,
      args = list(excelformat=excelformat_test, check = dat_test),
      expr =  {
        session$setInputs(excel_file = xlsx_test2, sheet_number = 1) # without row and column selection
        session$setInputs(go = "click")
        expect_equal(is.null(out$data),FALSE)
      }
    )
  )
}
)

test_that(desc = "Successful Stability Upload test", code = {
  xlsx_test2 = list(
    datapath = system.file(package = "ecerto", "extdata","Stability_Testdata.xlsx"),
    name = "Stability_Testdata.xlsx"
  )
  excelformat_test = shiny::reactiveVal("Stability")
  dat_test <- shiny::reactiveVal(FALSE)
  suppressMessages(
    shiny::testServer(
      app = m_ExcelUpload_Server,
      args = list(excelformat=excelformat_test, check = dat_test),
      expr =  {
        session$setInputs(excel_file = xlsx_test2, sheet_number = 1) # without row and column selection
        session$setInputs(go = "click")
        expect_equal(out$data,ecerto:::test_Stability_Excel())
      }
    )
  )
})