# Test 1: changed output of dput()
testthat::test_that(
  desc = "Successful Certifications Upload test",
  code = {
    testthat::local_edition(3)
    fn1 = eCerto:::test_mod_xlsx_range()
    sheetNo <- shiny::reactiveVal(1)
    range_selected <- matrix(c(1,1,1,2), ncol = 2, byrow = FALSE, dimnames = list(NULL, c("row", "col")))
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_xlsx_range_select_Server,
        args = list(current_file_input = fn1, sheet = sheetNo),
        {
          suppressMessages(session$flushReact())
          # set rows and columns selection
          #suppressMessages(session$setInputs(uitab_cells_selected = cells_selected))
          session$setInputs(uitab_range_selected = range_selected)
          testthat::expect_equal(tab_param$rng, "A1:B1")
          testthat::expect_equal(length(tab_param$tab), 3)  # contains three lists
          # [JL] this test needed to be rewritten because range selection was changed
          # to use the AutoFill pluin of DataTables instead of cell selection
          testthat::expect_equal(tab_param$end_col,2)
        }
      )
    )
  }
)

# Test 2: Upload RData even though Excel was expected ------------------------------------------------------------------
testthat::test_that(
  desc = "Throws error because RData was uploaded, but Excel was expected",
  code = {
    fn2 <- shiny::reactiveVal(structure(list(
      name = c(
        "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx",
        "Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx",
        "SR3_Fe_v26chs.RData"),
      size = c(27926L, 27617L, 9944L),
      type = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
               "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
               ""),
      datapath = c(
        system.file(package = "eCerto", "extdata", "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"),
        system.file(package = "eCerto", "extdata", "Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"),
        system.file(package = "eCerto", "extdata", "SR3_Fe_v26chs.RData"))
    ),
    row.names = c(NA,-3L),
    class = "data.frame"
    ))
    sheetNo <- shiny::reactiveVal(1)
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_xlsx_range_select_Server,
        args = list(current_file_input = fn2,sheet = sheetNo), {
          testthat::expect_warning(
            testthat::expect_error(tab(),"uploaded Excel files contain an empty one"),
            "Invalid file; Please upload a .xlsx file")
        }
      )
    )
  }
)


# Test 3: One empty Excel -----------------------------------------------------
testthat::test_that(
  desc = "Throws error when one file is uploaded which is Empty Excel",
  code = {
    fn3 <- shiny::reactiveVal(structure(
      list(
        name = c("EmptyExcel.xlsx","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx"),
        size = 10780L,
        type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        datapath = c(
          system.file(package = "eCerto", "extdata", "EmptyExcel.xlsx"),
          system.file(package = "eCerto", "extdata", "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx")
        )
      ),
      class = "data.frame",
      row.names = c(NA,-1L)
    ))
    sheetNo <- shiny::reactiveVal(1)
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_xlsx_range_select_Server,
        args = list(current_file_input = fn3, sheet = sheetNo), {
          testthat::expect_error(tab(), "uploaded Excel files contain an empty one")
        }
      )
    )
  }
)

# Test 4: Only one Certification Error -----------------------------------------
testthat::test_that(
  desc = "Throws error correctly when only one Certifications get uploaded",
  code = {
    fn1 = eCerto:::test_mod_xlsx_range()
    fn1_2 = shiny::reactiveVal(as.list(sapply(isolate(fn1()), "[[", 1)))
    sheetNo <- shiny::reactiveVal(1)
    suppressMessages(
      shiny::testServer(
        eCerto:::m_xlsx_range_select_Server,
        args = list(current_file_input = fn1_2,sheet=sheetNo), {
          testthat::expect_error(tab(),"less than 2 laboratory files uploaded. Upload more!")
        }
      )
    )
  }
)


# Test 5: no reaction expected after only one cell is selected -----------------
testthat::test_that(
  desc = "Range is correctly calculated for AutoFill selection",
  code = {
    fn1 <- eCerto:::test_mod_xlsx_range()
    sheetNo <- shiny::reactiveVal(1)
    range_selected <- matrix(c(1,1,1,2), ncol = 2, byrow = FALSE, dimnames = list(NULL, c("row", "col")))
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_xlsx_range_select_Server,
        args = list(current_file_input = fn1, sheet = sheetNo), {
          suppressMessages(session$flushReact())
          session$setInputs(uitab_range_selected = range_selected)
          testthat::expect_equal(tab_param$rng, "A1:B1")
        }
      )
    )
  }
)
