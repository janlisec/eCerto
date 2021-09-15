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
        args = list(
          modules = shiny::reactiveVal(c("Certification","Stability","Homogeneity")),
          uploadsource = shiny::reactiveVal(NULL)
        ),
        expr = {
          session$setInputs(in_file_ecerto_backup = rdat)
          # session$flushReact()
          expect_equal(sort(ecerto::getValue(rvreturn(),"modules")),c("Certification", "Homogeneity", "Stability" ))
          expect_equal(ecerto::getValue(rvreturn(), c("General","user")),"FK4")
          # because time_stamp changes every runtime, exclude it for testing as follows
          general <- ecerto::getValue(rvreturn(),"General")
          expect_equal(is.null(general$apm), FALSE)
          expect_equal(is.null(general$dataformat_version), FALSE)
          expect_equal(is.null(getValue(rvreturn(),c("Certification","data"))), FALSE)
          expect_equal(is.null(getValue(rvreturn(),c("Homogeneity","data"))), FALSE)
          # expect_equal(is.null(getValue(rvreturn(),c("Stability","data"))), FALSE)
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
      args = list(
        modules = shiny::reactiveVal(c("Certification","Stability","Homogeneity")),
        uploadsource = shiny::reactiveVal(NULL)
      ),
      expr = {
        session$setInputs(in_file_ecerto_backup = excel)
        expect_error(rdata(),"Only RData allowed.")
      }
    )
    
  }
)

test_that(
  desc = "Gives note that something has been uploaded by Excel before",
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
        args = list(
          modules = shiny::reactiveVal(c("Certification","Stability","Homogeneity")),
          uploadsource = shiny::reactiveVal("Excel")
        ),
        expr = {
          
          
          expect_message(session$setInputs(in_file_ecerto_backup = rdat),"RDataImport: Found existing data. Overwrite?")
          expect_equal(rvreturn(), NULL)
          # expect_equal(sort(ecerto::getValue(rvreturn(),"modules")),c("Certification", "Homogeneity", "Stability" ))
          # expect_equal(ecerto::getValue(rvreturn(), c("General","user")),"FK4")
        }
      )
    )
  })

