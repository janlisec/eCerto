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

dat_test = reactiveVal(NULL)


# Certification Test ------------------------------------------------------


# excelformat_test = reactiveVal("Certifications")
# # test_that("Successful Upload test",code = {
# shiny::testServer(
#   app = .ExcelUploadControllServer, 
#   args = list(excelformat=excelformat_test, dat = dat_test),
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
excelformat_test = reactiveVal("Homogeneity")

test_that("Successful Upload test",code = {
  shiny::testServer(app = .ExcelUploadControllServer, 
    args = list(excelformat=excelformat_test, dat = dat_test),
    expr =  {
      suppressMessages(
        session$setInputs(excel_file = xlsx_test2, sheet_number = 1) # without row and column selection
      )
      expect_snapshot(out$tab_flt)

      # print(session$returned())
      # expect_false(exists("ex()")) # ex() does not exist yet
      # cat("ex() exists?: ", ex(), "\n")
      session$setInputs(go = "click")
      # expect_true(exists("ex()"))
      expect_message(ex(), "go clicked")
      # cat("ex() exists?: ", need(ex(),"exists"), "\n")
      expect_snapshot(session$returned())
    }
  )
})

