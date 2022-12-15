testthat::local_edition(3)

# Certification Test ------------------------------------------------------
testthat::test_that(
  desc = "Certification: File-column appended",
  code = {
    excelformat_test <- shiny::reactiveVal("Certification")
    xlsx_test <- list(
      datapath = c(
        system.file(package = "eCerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"),
        system.file(package = "eCerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"),
        system.file(package = "eCerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
      ),
      name = c(
        "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx",
        "Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx",
        "Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
    )
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_ExcelUpload_Server,
        args = list(exl_fmt=excelformat_test),
        expr =  {
          session$setInputs(excel_file = xlsx_test, sheet_number = 1)
          # without row and column selection unfortunately
          rv_xlsx_range_select$tab <- lapply(rv_xlsx_range_select$tab, function(x) { x[8:16, 1:5, drop=FALSE]})
          session$setInputs(go = "click")
          testthat::expect_equal("File" %in% names(out$data), TRUE)
        }
      )
    )
  }
)

# Homogeneity Upload -------------------------------------------------------------
testthat::test_that(
  desc = "Successful Homogeneity Upload test",
  code = {
    xlsx_test2 = list(
      datapath = system.file(package = "eCerto", "extdata","Homog_test.xlsx"),
      name = "Homog_test.xlsx"
    )
    excelformat_test = shiny::reactiveVal("Homogeneity")
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_ExcelUpload_Server,
        args = list(exl_fmt=excelformat_test),
        expr =  {
          session$setInputs(excel_file = xlsx_test2, sheet_number = 1) # without row and column selection
          session$setInputs(go = "click")
          testthat::expect_equal(is.null(out$data),FALSE)
        }
      )
    )
  }
)

testthat::test_that(
  desc = "Successful Stability Upload test",
  code = {
    xlsx_test2 <- list(
      datapath = system.file(package = "eCerto", "extdata","Stability_Testdata.xlsx"),
      name = "Stability_Testdata.xlsx"
    )
    excelformat_test <- shiny::reactiveVal("Stability")
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_ExcelUpload_Server,
        args = list(exl_fmt=excelformat_test),
        expr =  {
          session$setInputs(excel_file = xlsx_test2, sheet_number = 1) # without row and column selection
          session$setInputs(go = "click")
          testthat::expect_equal(out$data, eCerto:::test_Stability_Excel())
        }
      )
    )
  }
)