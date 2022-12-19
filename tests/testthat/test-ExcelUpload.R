testthat::local_edition(3)

# Certification Test ------------------------------------------------------
testthat::test_that(
  desc = "Certification: File-column appended",
  code = {
    exl_fmt_test <- shiny::reactiveVal("Certification")
    xlsx_test <- list(
      datapath = c(
        system.file(package = "eCerto", "extdata", "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"),
        system.file(package = "eCerto", "extdata", "Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"),
        system.file(package = "eCerto", "extdata", "Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
      ),
      name = c(
        "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx",
        "Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx",
        "Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx"
      )
    )
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_ExcelUpload_Server,
        args = list(exl_fmt = exl_fmt_test),
        expr = {
          session$setInputs(excel_file = xlsx_test, sheet_number = 1, file_number = 1, file_name = "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx")
          # without testing row and column selection unfortunately
          rv_xlsx_range_select$tab <- lapply(rv_xlsx_range_select$tab, function(x) {
            x[8:16, 1:5, drop = FALSE]
          })
          session$setInputs(btn_load = "click")
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
    exl_fmt_test <- shiny::reactiveVal("Homogeneity")
    xlsx_test <- list(
      datapath = system.file(package = "eCerto", "extdata", "Homog_test.xlsx"),
      name = "Homog_test.xlsx"
    )
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_ExcelUpload_Server,
        args = list(exl_fmt = exl_fmt_test),
        expr = {
          session$setInputs(excel_file = xlsx_test, sheet_number = 1, file_number = 1, file_name = "Homog_test.xlsx")
          testthat::expect_true(exists("rv_xlsx_range_select"))
          session$setInputs(btn_load = "click")
          testthat::expect_true(exists("out"))
          testthat::expect_true(!is.null(out$data))
        }
      )
    )
  }
)

# Stability Upload -------------------------------------------------------------
testthat::test_that(
  desc = "Successful Stability Upload test",
  code = {
    exl_fmt_test <- shiny::reactiveVal("Stability")
    xlsx_test <- list(
      datapath = system.file(package = "eCerto", "extdata", "Stability_Testdata.xlsx"),
      name = "Stability_Testdata.xlsx"
    )
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_ExcelUpload_Server,
        args = list(exl_fmt = exl_fmt_test),
        expr = {
          session$setInputs(excel_file = xlsx_test, sheet_number = 1, file_number = 1, file_name = "Stability_Testdata.xlsx")
          session$setInputs(btn_load = "click")
          testthat::expect_equal(out$data, eCerto:::test_Stability_Excel())
        }
      )
    )
  }
)