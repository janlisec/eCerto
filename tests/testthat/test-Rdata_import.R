testthat::test_that(
  desc = "Successful RData (version 26) Upload",
  code = {
    testthat::local_edition(3)
    rdat <- list(
      name = "SR3_Fe_v26chs.RData",
      datapath = system.file(package = "eCerto","extdata","SR3_Fe_v26chs.RData")
    )
    # setting the config file to 'default' ensures that messages are suppressed because
    # eCerto is set to silent
    Sys.setenv("R_CONFIG_ACTIVE" = "default")
    rv <- eCerto$new()
    shiny::testServer(
      app = eCerto:::m_RDataImport_Server,
      args = list(rv = rv),
      expr = {
        session$setInputs(in_file_ecerto_backup = rdat)
        session$flushReact()
        tmp_rv <- eCerto:::list2rv(x = rdata())
        testthat::expect_equal(sort(eCerto::getValue(tmp_rv,"modules")),c("Certification", "Homogeneity", "Stability"))
        testthat::expect_equal(eCerto::getValue(tmp_rv, c("General","user")),"JL")
        # because time_stamp changes every runtime, exclude it for testing as follows
        general <- eCerto::getValue(tmp_rv,"General")
        testthat::expect_equal(is.null(general$apm), FALSE)
        testthat::expect_equal(is.null(general$dataformat_version), FALSE)
        testthat::expect_equal(is.null(eCerto::getValue(tmp_rv,c("Certification","data"))), FALSE)
        testthat::expect_equal(is.null(eCerto::getValue(tmp_rv,c("Homogeneity","data"))), FALSE)
        testthat::expect_equal(is.null(eCerto::getValue(tmp_rv,c("Stability","data"))), FALSE)
      }
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
    rv <- eCerto$new()
    suppressMessages(
      shiny::testServer(
        app = eCerto:::m_RDataImport_Server,
        args = list(rv = rv),
        expr = {
          session$setInputs(in_file_ecerto_backup = excel)
          testthat::expect_error(rdata(), "File extension different from RData.")
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
      datapath = system.file(package = "eCerto", "extdata", "SR3_Fe_v26chs.RData")
    )
    suppressMessages({ rv <- eCerto$new(eCerto:::init_rv()) })
    # setting the config file to 'dev' ensures that messages are shown
    # if you use 'expect_message' and 'default' if not
    #Sys.setenv("R_CONFIG_ACTIVE" = "dev")
    Sys.setenv("R_CONFIG_ACTIVE" = "default")
    shiny::testServer(
      app = eCerto:::m_RDataImport_Server,
      args = list(rv = rv),
      expr = {
        testthat::expect_equal(unname(rv$e_present()), c(F,F,F))
        testthat::expect_true(all(nchar(div_check_present())==0))
        session$setInputs(in_file_ecerto_backup = rdat)
        session$flushReact()
        testthat::expect_equal(unname(rv$e_present()), c(T,T,T))
        testthat::expect_true(all(nchar(div_check_present())>0))
      }
    )
    # reset to 'default' or silent afterwards
    Sys.setenv("R_CONFIG_ACTIVE" = "default")
  }
)
