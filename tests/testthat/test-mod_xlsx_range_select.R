
# Test 1 ---------------------------------------------------------------

# changed output of dput()
fn1 = reactiveVal(structure(list(
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
sheetNo = reactiveVal(1)
cells_selected = matrix(c(7,1,16,6),ncol = 2, byrow = TRUE)

test_that("Successful Upload test",code = {
  testServer(xlsx_range_select_Server,
             args = list(x = fn1,sheet=sheetNo), {
               suppressMessages(
                 session$setInputs(uitab_cells_selected = cells_selected)
               )
               expect_snapshot(rv$tab_flt)
               expect_equal(rv$end_col,6)
             }
  )
})


# Test 2: Upload RData even though Excel was expected ------------------------------------------------------------------

fn2 = reactiveVal(structure(list(
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

test_that("Upload RData even though Excel was expected",code = {
  testServer(xlsx_range_select_Server,args =  list(x = fn2,sheet=sheetNo), {
               expect_error(tab(), "Please upload Excel only")
             }
  )
})


# Test 3: One empty Excel -----------------------------------------------------

fn3 = reactiveVal(structure(
  list(
    name = "EmptyExcel.xlsx",
    size = 10780L,
    type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    datapath = system.file(package = "ecerto", "extdata","EmptyExcel.xlsx")
  ),
  class = "data.frame",
  row.names = c(NA,-1L)
))

test_that("Throws error when one file is uploaded which is Empty Excel",code = {
  testServer(xlsx_range_select_Server, args = list(x = fn3,sheet=sheetNo), {
    
    expect_error(tab(), "Excel file must not be empty")
    # expect_warning(tab(), "No data found on worksheet.")
  })
})
