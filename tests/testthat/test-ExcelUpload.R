testthat::local_edition(3)
# testthat::test_file(path = "tests/testthat/test-ExcelUpload.R")

# Certification Test ------------------------------------------------------
testthat::test_that(
  desc = "Certification: File-column appended",
  code = {
    rv <- eCerto:::test_rv()
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
        args = list(rv = rv),
        expr = {
          suppressWarnings({
            session$setInputs(
              moduleSelect = "Certification",
              excel_file = xlsx_test,
              sheet_number = 1,
              file_number = 1,
              file_name = "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"
            )
          })
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
    rv <- eCerto::eCerto$new(eCerto:::init_rv())
    #rv <- eCerto:::test_rv()
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_ExcelUpload_Server,
        args = list(rv = rv),
        expr = {
          testthat::expect_true(exists("rv_xlsx_range_select"))
          testthat::expect_null(rv_xlsx_range_select$tab)
          inp <- system.file(package = "eCerto", "extdata", "Homog_test.xlsx")
          # in GitHub actions the file can't be found
          if (file.exists(inp)) {
            suppressWarnings({
              # set required inputs
              session$setInputs(
                moduleSelect = "Homogeneity",
                excel_file = list(
                  datapath = inp,
                  name = "Homog_test.xlsx"
                ),
                sheet_number = 1,
                file_number = 1,
                file_name = "Homog_test.xlsx"
              )
              # flush reactivity and click button
              session$flushReact()
              session$setInputs(btn_load = "click")
            })
            testthat::expect_false(is.null(rv_xlsx_range_select$tab))
            testthat::expect_true(exists("out"))
            testthat::expect_true(!is.null(out$data))
          }
        }
      )
    )
  }
)

# Stability Upload -------------------------------------------------------------
testthat::test_that(
  desc = "Successful Stability Upload test",
  code = {
    rv <- eCerto:::test_rv()
    xlsx_test <- list(
      datapath = system.file(package = "eCerto", "extdata", "Stability_Testdata.xlsx"),
      name = "Stability_Testdata.xlsx"
    )
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_ExcelUpload_Server,
        args = list(rv = rv),
        expr = {
          suppressWarnings({
            session$setInputs(moduleSelect = "Stability", excel_file = xlsx_test, sheet_number = 1, file_number = 1, file_name = "Stability_Testdata.xlsx")
            session$flushReact()
            session$setInputs(btn_load = "click")
          })
          # $$JL$$ upload module was changed to respect analyte order from Excel file and to incorporate column time
          # original test data have to be modified to reflect this change
          comp <- eCerto:::test_Stability_Excel()
          comp[,"analyte"] <- factor(comp[,"analyte"], levels=c("Si","Mn"))
          comp[,"time"] <- as.numeric(comp[,"Date"]-min(comp[,"Date"]))
          testthat::expect_equal(out$data, comp)
        }
      )
    )
  }
)