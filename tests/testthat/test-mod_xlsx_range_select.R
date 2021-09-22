# Test 1.0 changed output of dput()
test_that(
  desc = "Successful Certifications Upload test",
  code = {
    testthat::local_edition(3)
    fn1 = ecerto:::test_mod_xlsx_range()
    sheetNo <- shiny::reactiveVal(1)
    cells_selected <- matrix(c(7,1,16,6), ncol = 2, byrow = TRUE)
    suppressMessages(
      shiny::testServer(
        app = ecerto::m_xlsx_range_select_Server,
        args = list(current_file_input = fn1, sheet = sheetNo),
        {
          suppressMessages(session$flushReact())
          # set rows and columns selection
          suppressMessages(session$setInputs(uitab_cells_selected = cells_selected))
          session$flushReact()
          expect_equal(length(tab_param$tab), 3)  # contains three lists
          expect_equal(tab_param$end_col,6)
        }
      )
    )
  }
)




# Test 2: Upload RData even though Excel was expected ------------------------------------------------------------------

test_that(
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
        system.file(package = "ecerto", "extdata", "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"),
        system.file(package = "ecerto", "extdata", "Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"),
        system.file(package = "ecerto", "extdata", "SR3_Fe_v26chs.RData"))
    ),
    row.names = c(NA,-3L),
    class = "data.frame"
    ))
    sheetNo <- shiny::reactiveVal(1)
    suppressMessages(
      shiny::testServer(
        app = ecerto::m_xlsx_range_select_Server,
        args = list(current_file_input = fn2,sheet = sheetNo), {
          #browser()
          expect_warning(
            expect_error(tab(),"uploaded Excel files contain an empty one"),
            "Invalid file; Please upload a .xlsx file")
        }
      )
    )
  }
)


# Test 3: One empty Excel -----------------------------------------------------
test_that(
  desc = "Throws error when one file is uploaded which is Empty Excel",
  code = {
    fn3 <- shiny::reactiveVal(structure(
      list(
        name = c("EmptyExcel.xlsx","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx"),
        size = 10780L,
        type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        datapath = c(
          system.file(package = "ecerto", "extdata", "EmptyExcel.xlsx"),
          system.file(package = "ecerto", "extdata", "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx")
        )
      ),
      class = "data.frame",
      row.names = c(NA,-1L)
    ))
    sheetNo <- shiny::reactiveVal(1)

    suppressMessages(
      shiny::testServer(
        app = m_xlsx_range_select_Server,
        args = list(current_file_input = fn3, sheet = sheetNo), {
          expect_error(tab(), "uploaded Excel files contain an empty one")
        }
      )
    )
  }
)

# Test: Only one Certification Error ------------------------------------------------------------------

test_that("Throws error correctly when only one Certifications get uploaded",code = {
  fn1 = ecerto:::test_mod_xlsx_range()
  fn1_2 = shiny::reactiveVal(as.list(sapply(isolate(fn1()), "[[", 1)))
  sheetNo <- shiny::reactiveVal(1)
  suppressMessages(
    shiny::testServer(
      m_xlsx_range_select_Server,
      args = list(current_file_input = fn1_2,sheet=sheetNo), {
        expect_error(tab(),"less than 2 laboratory files uploaded. Upload more!")
      })
  )
})


# no reaction expected after only one cell is selected -------------------------------

test_that("no reaction after only one DataTable element is selected",
          code = {
            fn1 <- ecerto:::test_mod_xlsx_range()
            sheetNo <- shiny::reactiveVal(1)
            cells_selected <- matrix(c(7,1), ncol = 2, byrow = TRUE)
            suppressMessages(
              shiny::testServer(
                app = m_xlsx_range_select_Server,
                args = list(current_file_input = fn1, sheet = sheetNo), {
                  suppressMessages(session$flushReact())
                  # @Frederick: dieser Test musste modifiziert werden. Ich habe den Modul-Code so geändert, dass der User eine MessageBox bekommt, wenn er versucht eine dritte Zelle innerhalb der Range zu wählen. Die Bedingung das der Klick keine Aktion hervorruft ist aber nicht mehr gegeben.
                  # set rows and columns selection
                  # expect_silent(
                  #   object = session$setInputs(uitab_cells_selected = cells_selected)
                  # )
                  session$setInputs(uitab_cells_selected = cells_selected)
                  expect_equal(input$uitab_cells_selected, cells_selected)
                }
              )
            )
          })

