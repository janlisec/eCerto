# Test 1: Certifications Uploaded, but Homogeneity hasn't yet ---------------------
test_that(
  desc = "Init of materialtable after Certifications uploaded",
  code = {
    testthat::local_edition(3)
    test_datreturn <- ecerto:::test_datreturn() # load test data
    suppressMessages(
      shiny::testServer(
        app = ecerto::m_materialtabelleServer,
        args = list(rdataUpload = shiny::reactive({NULL}), datreturn = test_datreturn),
        expr = {
          session$setInputs(pooling=FALSE) # needs to be set to trigger events ???
          expect_equal(availableAnalytes(), c("Si","Fe","Cu","Mn","Mg","Cr","Ni","Zn","Ti","Sc","Sn"))
          expect_equal(nrow(mater_table()), 11)
          expect_equal(colnames(mater_table()), c("analyte", "mean", "cert_val", "sd", "n", "char", "com", "k", "U"))
          expect_equal(cert_sd(), 0.0034)
          expect_equal(cert_mean(), 0.0493)
          expect_equal(mater_table()$U,c(0.079635,rep(0,10)),tolerance = 1e-5)
          expect_equal(mater_table()$char, c(0.03981726,rep(NA,10)), tolerance = 1e-5)
          expect_equal(mater_table()$com,c(0.03981726 ,rep(0,10)),tolerance = 1e-5)
        }
      )
    )
  }
)


# Test 2: another Analyte gets selected ------------------------------------
test_that(
  desc = "materialtable gets updated after another analyte gets selected",
  code = {
    test_datreturn <- ecerto:::test_datreturn() # load test data
    #Fe2 <- ecerto:::test_certification()[["data"]]
    #Fe2 <- Fe[Fe[,"ID"] %in% c(2,9,16,23,30,37),]
    Fe <- structure(
      list(
        ID = c(2L, 9L, 16L, 23L, 30L, 37L),
        Lab = structure(c(1L, 1L, 2L, 2L, 3L, 3L), .Label = c("L1", "L2", "L3"), class = "factor"),
        analyte = structure(c(2L, 2L, 2L, 2L, 2L, 2L), .Label = c("Si", "Fe", "Cu", "Mn", "Mg", "Cr", "Ni"), class = "factor"),
        replicate = structure(c(1L, 2L, 1L, 2L, 1L, 2L), .Label = c("1", "2"), class = "factor"),
        value = c(0.0529, 0.0527, 0.049, 0.0563, 0.0495, 0.0489),
        unit = c("0.05", "0.05", "0.05", "0.05", "0.05", "0.05"),
        S_flt = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
        L_flt = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
      row.names = c(2L, 9L, 16L, 23L, 30L, 37L), class = "data.frame"
    )
    #identical(Fe, Fe2)
    suppressMessages(
      shiny::testServer(
        app = ecerto::m_materialtabelleServer,
        args = list(rdataUpload = shiny::reactive({NULL}), datreturn = test_datreturn),
        expr = {
          testthat::local_edition(3)
          expect_null(mater_table()["mean","Fe"])
          session$setInputs(pooling=FALSE)
          datreturn$set("selectedAnalyteDataframe", Fe)
          session$flushReact()
          expect_equal(nrow(mater_table()), 11)
          expect_equal(colnames(mater_table()), c("analyte", "mean", "cert_val", "sd", "n", "char", "com", "k", "U"))
          expect_equal(cert_sd(), 0.0020)
          expect_equal(cert_mean(), 0.0516)
          expect_equal(mater_table()$U,c(0.079635,0.04475583,rep(0,9)),tolerance = 1e-5)
          expect_equal(mater_table()$char, c(0.039817,0.022378,rep(NA,9)), tolerance = 1e-5)
          expect_equal(mater_table()$com,c(0.03981726,0.02237792, rep(0,9)),tolerance = 1e-5)
        }
      )
    )
  })
# Test 3: Pooling on/off --------------------------------------------------
test_that(
  desc = "Pooling on/off switch can be set and changes 'n' in mat_tab",
  code = {
    testthat::local_edition(3)
    test_datreturn <- ecerto:::test_datreturn()
    suppressMessages(shiny::testServer(
      app = ecerto::m_materialtabelleServer,
      args = list(rdataUpload = shiny::reactive({NULL}), datreturn=test_datreturn),
      expr = {
        session$setInputs(pooling=FALSE)
        expect_equal(mater_table()[1,"n"], 3L)
        session$setInputs(pooling=TRUE)
        expect_equal(mater_table()[1,"n"], 9L)
        expect_equal(cert_sd(), 0.0032)
        expect_equal(mater_table()$n[1], 9)
        expect_equal(mater_table()$char[1], 0.02163624, tolerance = 1e-5)
        expect_equal(mater_table()$com[1], 0.02163624, tolerance = 1e-5)
        expect_equal(mater_table()$U[1], 0.04327248, tolerance = 1e-5)
      }
    ))
  }
)

# @FK --> Transfer funktioniert jetzt anders und "t_H" kann m.E. raus
# # Test 4: Homogeneity Transfer ---------------------
# test_that(
#   desc = "Transfer into column 'U3' successful",
#   code = {
#     test_datreturn <- ecerto:::test_datreturn()
#     transfer <- structure(list(U3 = c(0, 0.015535927030583, 0, 0, 0.015535927030583,0, 0, 0, 0, 0, 0)), row.names = c(NA, -11L), class = "data.frame")
#     com_check <- c(0.0398172599441121, 0.015535927030583, 0, 0, 0.015535927030583,0, 0, 0, 0, 0, 0)
#     U3_check <- c(0, 0.015535927030583, 0, 0, 0.015535927030583, 0, 0, 0, 0,0, 0)
#     U_check <- c(0.0796345198882242, 0.031071854061166, 0, 0, 0.031071854061166,0, 0, 0, 0, 0, 0)
#     suppressMessages(
#       shiny::testServer(
#         ecerto::m_materialtabelleServer,
#         args = list(rdataUpload = shiny::reactive({NULL}), datreturn=test_datreturn), {
#           session$setInputs(pooling=FALSE)
#           setValue(datreturn, "t_H", transfer)
#           session$flushReact()
#           expect_equal(mater_table()[,"com"], com_check)
#           expect_equal(mater_table()[,"U3"], U3_check)
#           expect_equal(mater_table()[,"U"], U_check)
#         })
#     )
# })

# Test 5: Homogeneity Transfer ---------------------
test_that("Lab filter",code = {
  datreturn1 = ecerto:::test_datreturn()
  suppressMessages(
    shiny::testServer(
      ecerto::m_materialtabelleServer,
      args = list(
        rdataUpload = shiny::reactive({NULL}),
        datreturn = datreturn1
      ), {
        session$setInputs(pooling=FALSE)
        session$flushReact()
        # print(tmp_mater_table())
        sAnData_tmp = sAnData()
        sAnData_tmp[sAnData_tmp$Lab=="L1",]$L_flt <- TRUE
        setValue(datreturn,"selectedAnalyteDataframe",sAnData_tmp)
        # session$setInputs(datreturn$selectedAnalyteDataframe=sAnData_tmp)
        session$flushReact()
        # print(dput(mater_table()[mater_table()$analyte=="Si",]))
        # data <- sAnData()[!sAnData()[, "L_flt"], ]
        # expected_mean = roundMT(mean(sapply(
        #   split(data[, "value"], as.character(data[, "Lab"])), mean, na.rm = T
        # )), 4)
        # expected_Si = structure(list(analyte = "Si", mean = 0.0484, cert_val = 0.0484,
        #                              sd = 0.0041, n = 2L, char = 0.0599, com = 0.0599,
        #                              k = 2, U = 0.1198), col_code = structure(list(
        #                                ID = character(0), Name = character(0)), row.names = integer(0), class = "data.frame"), row.names = 1L, class = "data.frame")
        # print(expected_mean)
        # actual_mean = tmp_mater_table()[tmp_mater_table()$analyte=="Si",]$mean
        # expect_equal(actual_mean,0.0512)
        actual_Si = tmp_mater_table()[tmp_mater_table()$analyte=="Si",]
        expected_Si = structure(list(analyte = "Si", mean = 0.0512, cert_val = 0.0512,
                                     sd = 1e-04, n = 2L, char = 0.0014, com = 0.0014, k = 2, U = 0.0028), col_code = structure(list(
                                       ID = character(0), Name = character(0)), row.names = integer(0), class = "data.frame"), row.names = 1L, class = "data.frame")
        expect_equal(actual_Si,expected_Si)
      })
  )
})
