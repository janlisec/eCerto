rdat = list(
  name = "SR3_Fe_v26chs.RData",
  datapath = system.file(package = "ecerto","extdata","SR3_Fe_v26chs.RData")
)
rv_test = init_rv()

test_that("Successful RData (version 26) Upload",code = {
  testServer(.RDataImport_Server,
             args = list(rv = rv_test), {
               session$setInputs(in_file_ecerto_backup = rdat)
               expect_equal(rv$Certifications$user,"JL")
               expect_snapshot(reactiveValuesToList(rv))
             }
  )
})


# Error when Excel Upload -------------------------------------------------
excel = list(
  name = "Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx",
  datapath = system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
)
rv_test = ecerto::init_rv()

test_that("Throws error when Excel File is tried to upload",code = {
  testServer(.RDataImport_Server,
             args = list(rv = rv_test), {
               
               session$setInputs(in_file_ecerto_backup = excel)
               expect_error(rdata(),"Only RData allowed.")
             }
  )
})