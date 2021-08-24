test_that(
  desc = "Successful RData (version 26) Upload",
  code = {
    testthat::local_edition(3)
    rdat <- list(
      name = "SR3_Fe_v26chs.RData",
      datapath = system.file(package = "ecerto","extdata","SR3_Fe_v26chs.RData")
    )
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    suppressMessages(
    shiny::testServer(
      app = ecerto::m_RDataImport_Server,
      args = list(rv = rv_test),
      expr = {
               session$setInputs(in_file_ecerto_backup = rdat)
               #expect_snapshot(reactiveValuesToList(ecerto::getValue(rv)))

               expect_equal(sort(rv$names()),c("Certification", "Homogeneity", "Stability" ))
               expect_equal(getValue(rv, c("General","user")),"FK4")

               # because time_stamp changes every runtime, exclude it for testing as follows
               bbb <- getValue(rv,"General")
               bbb <- sapply(names(bbb)[!names(bbb) %in% "time_stamp"], function(x) {bbb[[x]]})
               #expect_snapshot(bbb()[!names(bbb) %in% "time_stamp"])
               expect_snapshot(bbb)
               expect_snapshot(shiny::reactiveValuesToList(getValue(rv,"Certification")))
               expect_snapshot(shiny::reactiveValuesToList(getValue(rv,"Homogeneity")))
               expect_snapshot(shiny::reactiveValuesToList(getValue(rv,"Stability")))
      }
    )
  )
})

test_that(
  desc = "Throws error when Excel File is tried to upload",
  code = {

    excel <- list(
      name = "Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx",
      datapath = system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
    )
    rv_test =  ecerto::reactiveClass$new(ecerto::init_rv())

    shiny::testServer(
      app = ecerto::m_RDataImport_Server,
      args = list(rv = rv_test),
      expr = {
        session$setInputs(in_file_ecerto_backup = excel)
        expect_error(rdata(),"Only RData allowed.")
      }
    )

  }
)


