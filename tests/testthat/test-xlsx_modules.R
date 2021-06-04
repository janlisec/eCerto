local_edition(3)
xlsx_test = list(
  datapath = c(
    system.file(package = "ecerto", "extdata","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx"),
    system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"),
    system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
  ),
  name = c(
    "Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx",
    "Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx",
    "Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
)

dat_test <- shiny::reactiveVal(FALSE)


# Certification Test ------------------------------------------------------


excelformat_test = shiny::reactiveVal("Certifications")
# # test_that("Successful Upload test",code = {
# shiny::testServer(
#   app = m_ExcelUploadControl_Server,
#   args = list(excelformat=excelformat_test, check = dat_test),
#   expr =  {
#     session$setInputs(excel_file = xlsx_test, sheet_number = 1) # without row and column selection unfortunately
#     # expect_snapshot(out$tab)
#     print(reactiveValuesToList(out))
#
#   }
# )
# # })



# Homogeneity -------------------------------------------------------------

xlsx_test2 = list(
  datapath = system.file(package = "ecerto", "extdata","Homog_test.xlsx"),
  name = "Homog_test.xlsx"
)
excelformat_test = shiny::reactiveVal("Homogeneity")

test_that("Successful Upload test",code = {
  shiny::testServer(app = m_ExcelUploadControl_Server,
    args = list(excelformat=excelformat_test, check = dat_test),
    expr =  {
      # suppressMessages(
        session$setInputs(excel_file = xlsx_test2, sheet_number = 1) # without row and column selection
      # )

      expect_snapshot(rv_xlsx_range_select$tab_flt)
      session$setInputs(go = "click")
      # expect_message(ex(), "go clicked")
      expect_snapshot(session$returned())
    }
  )
})

