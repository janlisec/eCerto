testthat::test_that(
  desc = "Init of materialtable after Certifications uploaded",
  code = {
    testthat::local_edition(3)
    suppressMessages({
      rv <- eCerto:::test_rv() # load test data
      shiny::testServer(
        app = eCerto:::m_materialtabelleServer,
        args = list(rv = rv),
        expr = {
          #browser()
          gargoyle::init("update_c_analyte")
          #session$setInputs(pooling=FALSE) # needs to be set to trigger events ???
          mt <- getValue(rv, c("General","materialtabelle"))
          testthat::expect_equal(unname(rv$c_analytes()), c("Si","Fe","Cu"))
          testthat::expect_equal(nrow(mt), 3)
          testthat::expect_equal(colnames(mt), c("analyte", "mean", "cert_val", "sd", "n", "u_char", "u_com", "k", "U", "U_abs", "unit"))
          # set the current analyte to start calculation
          c_analyte(rv$c_analyte)
          testthat::expect_equal(cert_sd(), 0.004331029)
          testthat::expect_equal(cert_mean(), 0.0484375)
        }
      )
    })
  }
)

# # Test 2: another Analyte gets selected ------------------------------------
# testthat::test_that(
#   desc = "materialtable gets updated after another analyte gets selected",
#   code = {
#     # load/prepare test data
#     test_datreturn <- eCerto:::test_datreturn()
#     Fe <- structure(
#       list(
#         ID = c(2L, 9L, 16L, 23L, 30L, 37L),
#         Lab = structure(c(1L, 1L, 2L, 2L, 3L, 3L), .Label = c("L1", "L2", "L3"), class = "factor"),
#         analyte = structure(c(2L, 2L, 2L, 2L, 2L, 2L), .Label = c("Si", "Fe", "Cu", "Mn", "Mg", "Cr", "Ni"), class = "factor"),
#         replicate = structure(c(1L, 2L, 1L, 2L, 1L, 2L), .Label = c("1", "2"), class = "factor"),
#         value = c(0.0529, 0.0527, 0.049, 0.0563, 0.0495, 0.0489),
#         unit = c("0.05", "0.05", "0.05", "0.05", "0.05", "0.05"),
#         S_flt = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
#         L_flt = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
#       row.names = c(2L, 9L, 16L, 23L, 30L, 37L), class = "data.frame"
#     )
#     suppressMessages(
#       shiny::testServer(
#         app = eCerto::m_materialtabelleServer,
#         args = list(rdataUpload = shiny::reactive({NULL}), datreturn = test_datreturn),
#         expr = {
#           testthat::local_edition(3)
#           testthat::expect_null(mater_table()[mater_table()$analyte=="Fe","mean"])
#           session$setInputs(pooling=FALSE)
#           testthat::expect_equal(mater_table()[mater_table()$analyte=="Fe","mean"], as.numeric(NA))
#           datreturn$set("selectedAnalyteDataframe", Fe)
#           session$flushReact()
#           testthat::expect_equal(nrow(mater_table()), 11)
#           testthat::expect_equal(colnames(mater_table()), c("analyte", "mean", "cert_val", "sd", "n", "u_char", "u_com", "k", "U"))
#           testthat::expect_equal(cert_sd(), getValue(test_datreturn,"cert_sd"))
#           testthat::expect_equal(cert_mean(), getValue(test_datreturn,"cert_mean"))
#           testthat::expect_equal(mater_table(), getValue(test_datreturn,"mater_table"))
#           #expect_equal(mater_table()$U,c(0.079635,0.04475583,rep(0,9)),tolerance = 1e-5)
#           #expect_equal(mater_table()$char, c(0.039817,0.022378,rep(NA,9)), tolerance = 1e-5)
#           #expect_equal(mater_table()$com,c(0.03981726,0.02237792, rep(0,9)),tolerance = 1e-5)
#         }
#       )
#     )
#   }
# )
#
# # Test 3: Pooling on/off --------------------------------------------------
# testthat::test_that(
#   desc = "Pooling on/off switch can be set and changes 'n' in mat_tab",
#   code = {
#     testthat::local_edition(3)
#     test_datreturn <- eCerto:::test_datreturn()
#     suppressMessages(shiny::testServer(
#       app = eCerto::m_materialtabelleServer,
#       args = list(rdataUpload = shiny::reactive({NULL}), datreturn=test_datreturn),
#       expr = {
#         session$setInputs(pooling=FALSE)
#         testthat::expect_equal(mater_table()[1,"n"], 3L)
#         session$setInputs(pooling=TRUE)
#         testthat::expect_equal(mater_table()[1,"n"], 9L)
#         testthat::expect_equal(mater_table()$n[1], 9)
#         testthat::expect_equal(mater_table(), getValue(test_datreturn,"mater_table"))
#         # expect_equal(cert_sd(), 0.0032)
#         # expect_equal(mater_table()$char[1], 0.02163624, tolerance = 1e-5)
#         # expect_equal(mater_table()$com[1], 0.02163624, tolerance = 1e-5)
#         # expect_equal(mater_table()$U[1], 0.04327248, tolerance = 1e-5)
#       }
#     ))
#   }
# )
#
# # Test 4: Lab filter ---------------------
# testthat::test_that(
#   desc = "Setting Lab filter is changing 'n' in material table",
#   code = {
#     test_datreturn <- eCerto:::test_datreturn()
#     suppressMessages(
#       shiny::testServer(
#         eCerto::m_materialtabelleServer,
#         args = list(
#           rdataUpload = shiny::reactive({NULL}),
#           datreturn = test_datreturn
#         ), {
#           session$setInputs(pooling=FALSE)
#           session$flushReact()
#           testthat::expect_equal(tmp_mater_table()[tmp_mater_table()$analyte=="Si","n"], 3L)
#           tmp <- selectedAnalyteDataframe()
#           tmp[tmp$Lab=="L1",]$L_flt <- TRUE
#           setValue(datreturn, "selectedAnalyteDataframe", tmp)
#           session$flushReact()
#           testthat::expect_equal(tmp_mater_table()[tmp_mater_table()$analyte=="Si","n"], 2L)
#         }
#       )
#     )
#   }
# )