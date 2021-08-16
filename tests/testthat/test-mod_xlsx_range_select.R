# Test 1.0 changed output of dput()
test_that(
  desc = "Successful Certifications Upload test",
  code = {
    testthat::local_edition(3)
    fn1 = ecerto:::test_mod_xlsx_range()
    sheetNo <- shiny::reactiveVal(1)
    cells_selected <- matrix(c(7,1,16,6), ncol = 2, byrow = TRUE)

    shiny::testServer(
      app = ecerto::xlsx_range_select_Server,
      args = list(current_file_input = fn1, sheet = sheetNo),
      {
        suppressMessages(session$flushReact())
        # set rows and columns selection
        suppressMessages(session$setInputs(uitab_cells_selected = cells_selected))
        session$flushReact()
        expect_snapshot(tab_param$tab_flt)
        expect_equal(tab_param$end_col,6)
      }
    )
  }
)

# # Test 1.2 File column after cell selection
# test_that(
#   desc = "File column is appended for Certification after cell selection",
#   code = {
#     fn1 = ecerto:::test_mod_xlsx_range()
#     sheetNo <- shiny::reactiveVal(1)
#     cells_selected <- matrix(c(7,1,16,6), ncol = 2, byrow = TRUE)
#     shiny::testServer(
#       app = xlsx_range_select_Server,
#       args = list(current_file_input = fn1, sheet = sheetNo), {
#         suppressMessages(session$flushReact())
#         # set rows and columns selection
#         suppressMessages(
#           session$setInputs(uitab_cells_selected = cells_selected))
#         session$flushReact()
#         # has File been added correctly after cell selection
#         expect_true("File" %in% colnames(tab_param$tab_flt[[1]]))
#       }
#     )
#   }
# )


# Test 2: Upload RData even though Excel was expected ------------------------------------------------------------------

test_that("Throws error because RData was uploaded, but Excel was expected",code = {
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
      system.file(package = "ecerto", "extdata","Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"),
      system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"),
      system.file(package = "ecerto","extdata","SR3_Fe_v26chs.RData"))
  ),
  row.names = c(NA,-3L),
  class = "data.frame"
  ))
  sheetNo <- shiny::reactiveVal(1)

  shiny::testServer(
    app = ecerto::xlsx_range_select_Server,
    args =  list(current_file_input = fn2,sheet=sheetNo), {
      expect_warning(
        expect_error(tab(),"uploaded Excel contain an empty one"),
        "Invalid file; Please upload a .xlsx file")
    }
  )
})


# Test 3: One empty Excel -----------------------------------------------------



test_that("Throws error when one file is uploaded which is Empty Excel",code = {
  fn3 <- shiny::reactiveVal(structure(
    list(
      name = c("EmptyExcel.xlsx","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx"),
      size = 10780L,
      type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
      datapath = c(
        system.file(package = "ecerto", "extdata","EmptyExcel.xlsx"),
        system.file(package = "ecerto", "extdata","Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx")
      )
    ),
    class = "data.frame",
    row.names = c(NA,-1L)
  ))
  sheetNo <- shiny::reactiveVal(1)

  suppressMessages(
    shiny::testServer(
      app = xlsx_range_select_Server,
      args = list(current_file_input = fn3, sheet = sheetNo), {
        expect_error(tab(), "uploaded Excel contain an empty one")
      }
    )
  )
})

# Test: Only one Certification Error ------------------------------------------------------------------

test_that("Throws error correctly when only one Certifications get uploaded",code = {
  fn1 = ecerto:::test_mod_xlsx_range()
  fn1_2 = shiny::reactiveVal(as.list(sapply(isolate(fn1()), "[[", 1)))
  sheetNo <- shiny::reactiveVal(1)
  suppressMessages(
    shiny::testServer(
      xlsx_range_select_Server,
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
            shiny::testServer(
              app = xlsx_range_select_Server,
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
          })


# Homogeneity -------------------------------------------------------------

# test_that("File column is appended for Homogeneity",code = {
#   sheetNo <- shiny::reactiveVal(1)
#   fnHomog = shiny::reactiveVal(structure(
#     list(
#       name = "Homog_test.xlsx",
#       # size = c(27926L, 27617L, 9944L),
#       type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
#       datapath = system.file(package = "ecerto", "extdata", "Homog_test.xlsx"),
#       # row.names = c(NA,-3L),
#       class = "data.frame"
#     )
#   ))
#   shiny::testServer(
#     app = xlsx_range_select_Server,
#     args = list(current_file_input = fnHomog,sheet=sheetNo,excelformat=shiny::reactiveVal({"Homogeneity"})), {
#       suppressMessages(session$flushReact())
#       # has File been added correctly after Upload
#       expect_true("File" %in% colnames(tab_param$tab_flt[[1]]))
#       # set rows and columns selection
#       # suppressMessages(
#       #   session$setInputs(uitab_cells_selected = cells_selected))
#       # session$flushReact()
#       # # has File been added correctly after cell selection
#       # expect_true("File" %in% colnames(tab_param$tab_flt[[1]]))
#     }
#   )
# })
