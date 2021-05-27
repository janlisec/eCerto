
# Test 1 ---------------------------------------------------------------

# fn1: changed output of dput()
fn1 = shiny::reactiveVal(structure(list(
  name = c(
    "Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx",
    "Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx",
    "Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx"),
  size = c(27926L, 27617L, 27527L),
  type = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
  datapath = c(
    system.file(package = "ecerto", "extdata","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx"),
    system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"),
    system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx"))
),
row.names = c(NA,-3L),
class = "data.frame"
))
sheetNo = shiny::reactiveVal(1)
cells_selected = matrix(c(7,1,16,6),ncol = 2, byrow = TRUE)

test_that("Successful Certifications Upload test",code = {
  shiny::testServer(xlsx_range_select_Server,
             args = list(x = fn1,sheet=sheetNo), {
               suppressMessages(session$flushReact())
               # set rows and columns selection
               suppressMessages(
                 session$setInputs(uitab_cells_selected = cells_selected)
               )
               session$flushReact()
               expect_snapshot(rv$tab_flt)
               expect_equal(rv$end_col,6)
             }
  )
})


# Test 1.2 File column after cell selection -------------------------------


test_that("File column is appended for Certification after cell selection",code = {
  shiny::testServer(xlsx_range_select_Server,
             args = list(x = fn1,sheet=sheetNo), {
               suppressMessages(session$flushReact())
               # set rows and columns selection
               suppressMessages(
                 session$setInputs(uitab_cells_selected = cells_selected))
               session$flushReact()
               # has File been added correctly after cell selection
               expect_true("File" %in% colnames(rv$tab_flt[[1]]))
             }
  )
})

# Test 2: Upload RData even though Excel was expected ------------------------------------------------------------------

fn2 = shiny::reactiveVal(structure(list(
  name = c(
    "Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx",
    "Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx",
    "SR3_Fe_v26chs.RData"),
  size = c(27926L, 27617L, 9944L),
  type = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
           ""),
  datapath = c(
    system.file(package = "ecerto", "extdata","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx"),
    system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"),
    system.file(package = "ecerto","extdata","SR3_Fe_v26chs.RData"))
),
row.names = c(NA,-3L),
class = "data.frame"
))

test_that("Throws error because RData was uploaded but Excel was expected",code = {
  shiny::testServer(xlsx_range_select_Server,args =  list(x = fn2,sheet=sheetNo), {
    expect_error(tab(), "Please upload Excel only")
  }
  )
})


# Test 3: One empty Excel -----------------------------------------------------

fn3 = shiny::reactiveVal(structure(
  list(
    name = c("EmptyExcel.xlsx","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx"),
    size = 10780L,
    type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    datapath = c(
      system.file(package = "ecerto", "extdata","EmptyExcel.xlsx"),
      system.file(package = "ecerto", "extdata","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx")
    )
  ),
  class = "data.frame",
  row.names = c(NA,-1L)
))

test_that("Throws error when one file is uploaded which is Empty Excel",code = {
  suppressMessages(
    shiny::testServer(xlsx_range_select_Server, args = list(x = fn3,sheet=sheetNo), {

    expect_error(tab(), "Excel file must not be empty")
    # expect_warning(tab(), "No data found on worksheet.")
    })
  )
})

# Test: Only one Certification Error ------------------------------------------------------------------

fn1_2 = shiny::reactiveVal(as.list(sapply(isolate(fn1()), "[[", 1)))

test_that("Throws error correctly when only one Certifications get uploaded",code = {
  suppressMessages(shiny::testServer(
      xlsx_range_select_Server,
      args = list(x = fn1_2,sheet=sheetNo), {
        expect_error(tab(),"less than 2 laboratory files uploaded. Upload more!")
      })
     )
})


# no reaction after only one thing selected -------------------------------

cells_selected = matrix(c(7,1),ncol = 2, byrow = TRUE)

test_that("no reaction after only one DataTable element is selected",code = {
  shiny::testServer(xlsx_range_select_Server,
             args = list(x = fn1,sheet=sheetNo), {
               suppressMessages(session$flushReact())

               # # set rows and columns selection
               expect_silent(
                 object = session$setInputs(uitab_cells_selected = cells_selected)
               )
             }
  )
})


# Homogeneity -------------------------------------------------------------



fnHomog = shiny::reactiveVal(structure(
  list(
    name = "Homog_test.xlsx",
    # size = c(27926L, 27617L, 9944L),
    type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    datapath = system.file(package = "ecerto", "extdata", "Homog_test.xlsx"),
    # row.names = c(NA,-3L),
    class = "data.frame"
  )
))

test_that("File column is appended for Homogeneity",code = {
  shiny::testServer(xlsx_range_select_Server,
             args = list(x = fnHomog,sheet=sheetNo,excelformat=shiny::reactiveVal({"Homogeneity"})), {
               suppressMessages(session$flushReact())
  # has File been added correctly after Upload
  expect_true("File" %in% colnames(rv$tab_flt[[1]]))
  # set rows and columns selection
  # suppressMessages(
  #   session$setInputs(uitab_cells_selected = cells_selected))
  # session$flushReact()
  # # has File been added correctly after cell selection
  # expect_true("File" %in% colnames(rv$tab_flt[[1]]))
}
)
})