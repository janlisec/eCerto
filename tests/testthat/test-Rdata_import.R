testthat::test_that(
  desc = "Successful RData (version 26) Upload",
  code = {
    testthat::local_edition(3)
    rdat <- list(
      name = "SR3_Fe_v26chs.RData",
      datapath = system.file(package = "eCerto","extdata","SR3_Fe_v26chs.RData")
    )
    suppressMessages(
      shiny::testServer(
        app = eCerto::m_RDataImport_Server,
        args = list(
          modules = shiny::reactiveVal(c("Certification","Stability","Homogeneity")),
          uploadsources = shiny::reactiveVal(NULL)
        ),
        expr = {
          session$setInputs(in_file_ecerto_backup = rdat)
          # session$flushReact()
          testthat::expect_equal(sort(eCerto::getValue(rvreturn(),"modules")),c("Certification", "Homogeneity", "Stability" ))
          testthat::expect_equal(eCerto::getValue(rvreturn(), c("General","user")),"FK4")
          # because time_stamp changes every runtime, exclude it for testing as follows
          general <- eCerto::getValue(rvreturn(),"General")
          testthat::expect_equal(is.null(general$apm), FALSE)
          testthat::expect_equal(is.null(general$dataformat_version), FALSE)
          testthat::expect_equal(is.null(getValue(rvreturn(),c("Certification","data"))), FALSE)
          testthat::expect_equal(is.null(getValue(rvreturn(),c("Homogeneity","data"))), FALSE)
          # expect_equal(is.null(getValue(rvreturn(),c("Stability","data"))), FALSE)
        }
      )
    )
  }
)

testthat::test_that(
  desc = "Throws error when Excel File is tried to upload",
  code = {
    excel <- list(
      name = "Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx",
      datapath = system.file(package = "eCerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
    )
    suppressMessages(
      shiny::testServer(
        app = eCerto::m_RDataImport_Server,
        args = list(
          modules = shiny::reactiveVal(c("Certification","Stability","Homogeneity")),
          uploadsources = shiny::reactiveVal(NULL)
        ),
        expr = {
          session$setInputs(in_file_ecerto_backup = excel)
          testthat::expect_error(rdata(),"Only RData allowed.")
        }
      )
    )
  }
)

testthat::test_that(
  desc = "Gives note that something has been uploaded by Excel before",
  code = {
    testthat::local_edition(3)
    rdat <- list(
      name = "SR3_Fe_v26chs.RData",
      datapath = system.file(package = "eCerto","extdata","SR3_Fe_v26chs.RData")
    )
    suppressMessages(
      shiny::testServer(
        app = eCerto::m_RDataImport_Server,
        args = list(
          modules = shiny::reactiveVal(c("Certification","Stability","Homogeneity")),
          uploadsources = shiny::reactiveVal(list("Certification" = "Excel","Homogeneity"=NULL,"stability"=NULL))
        ),
        expr = {
          testthat::expect_message(
            session$setInputs(in_file_ecerto_backup = rdat)
            ,"RDataImport: Found existing data. Overwrite?")
          testthat::expect_equal(rvreturn(), NULL)
        }
      )
    )
  }
)
