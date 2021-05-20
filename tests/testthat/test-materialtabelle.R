# Test 1: Certifications Uploaded, but Homogeneity hasn't yet ---------------------

datreturn1 = do.call("reactiveValues",
  list(
    lab_statistics = structure(
      list(
        Lab = structure(1:3, .Label = c("L1", "L2", "L3"), class = "factor"), 
        mean = c(0.0453, 0.0513333333333333, 0.0511333333333333), 
        sd = c(0.00185202591774521, 0.00100664459136943,0.000351188458428424), 
        n = c(3L, 3L, 3L)), 
      class = "data.frame", row.names = c("L1","L2", "L3")
    ), 
    selectedAnalyteDataframe = structure(
      list(
        ID = c(1L,12L, 23L, 34L, 44L, 54L, 64L, 75L, 86L), 
        Lab = structure(c(1L,1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L), .Label = c("L1", "L2", "L3"), class = "factor"),
        analyte = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("Si",  "Fe", "Cu", "Mn", "Mg", "Cr", "Ni", "Zn", "Ti", "Sc", "Sn"), class = "factor"), 
        replicate = structure(c(1L, 2L, 3L,1L, 2L, 3L, 1L, 2L, 3L), .Label = c("1", "2", "3"), class = "factor"), value = c(0.0452, 0.0435, 0.0472, 0.0504, 0.0512, 0.0524,  0.0511, 0.0508, 0.0515),
        unit = c("0.05", "0.05", "0.05", "0.05", "0.05", "0.05", "0.05", "0.05", "0.05"), 
        S_flt = c(FALSE,FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
        L_flt = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE)), row.names = c(1L, 12L, 23L, 34L, 44L, 54L, 64L, 75L, 86L), class = "data.frame"), 
    mater_table = NULL, 
    h_vals = NULL,
    t_H = NULL
  )
)


test_that("Init of materialtable after Certifications uploaded",code = {
  suppressMessages(testServer(.materialtabelleServer, 
             args = list(rdataUpload = reactive({NULL}), datreturn=datreturn1), {
               session$setInputs(pooling=FALSE)
               # session$flushReact()
               aTest = c("Si","Fe","Cu","Mn","Mg","Cr","Ni","Zn","Ti","Sc","Sn")
               expect_equal(availableAnalytes(),aTest)
               # session$flushReact()
               # # c = init_materialTabelle(availableAnalytes())
               # # mater_table(c) # save materialtabelle
               expect_equal(nrow(mater_table()),11)
               m_cols = c("analyte", "mean", "F1", "F2", "F3", "cert_val", "sd", "n",
                          "char", "U2", "U3", "U4", "U5", "U6", "U7", "com", "k", "U")
               expect_equal(colnames(mater_table()),m_cols)
               expect_equal(cert_sd(),0.0034)
               expect_equal(cert_mean(),0.0493)
               expect_snapshot(mater_table())
               # session$setInputs(pooling=FALSE)
               # expect_snapshot(mater_table())
             }))
})


# Test 2: another Analyte gets selected ------------------------------------

Fe = structure(
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

test_that("materialtable gets updated after another analyte gets selected",code = {
  suppressMessages(
  testServer(.materialtabelleServer, 
             args = list(rdataUpload = reactive({NULL}), datreturn=datreturn1), {
               # session$flushReact()
               # expect_message('.',"materialTabelle - update initiated for: Si")
               expect_null(mater_table()["mean","Fe"])
               session$setInputs(pooling=FALSE)
               expect_snapshot(mater_table())
               datreturn$selectedAnalyteDataframe = Fe
               session$flushReact()
               expect_snapshot(mater_table())
             })
  )
})


# Test 3: Pooling on/off --------------------------------------------------

test_that("Pooling on/off",code = {
  
  suppressMessages(testServer(.materialtabelleServer, 
             args = list(rdataUpload = reactive({NULL}), datreturn=datreturn1), {
               session$setInputs(pooling=FALSE)
               expect_snapshot(mater_table())
               session$setInputs(pooling=TRUE)
               expect_snapshot(mater_table())
             }))
})


# Test 4: Homogeneity Transfer ---------------------





datreturn2 = list(
  lab_statistics = structure(
    list(
      Lab = structure(1:3, .Label = c("L1", "L2", "L3"), class = "factor"), 
      mean = c(0.0453, 0.0513333333333333, 0.0511333333333333), 
      sd = c(0.00185202591774521, 0.00100664459136943,0.000351188458428424), 
      n = c(3L, 3L, 3L)), 
    class = "data.frame", row.names = c("L1","L2", "L3")
  ), 
  selectedAnalyteDataframe = structure(
    list(
      ID = c(1L,12L, 23L, 34L, 44L, 54L, 64L, 75L, 86L), 
      Lab = structure(c(1L,1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L), .Label = c("L1", "L2", "L3"), class = "factor"),
      analyte = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c("Si",  "Fe", "Cu", "Mn", "Mg", "Cr", "Ni", "Zn", "Ti", "Sc", "Sn"), class = "factor"), 
      replicate = structure(c(1L, 2L, 3L,1L, 2L, 3L, 1L, 2L, 3L), .Label = c("1", "2", "3"), class = "factor"), value = c(0.0452, 0.0435, 0.0472, 0.0504, 0.0512, 0.0524,  0.0511, 0.0508, 0.0515),
      unit = c("0.05", "0.05", "0.05", "0.05", "0.05", "0.05", "0.05", "0.05", "0.05"), S_flt = c(FALSE,L_flt = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,  FALSE, FALSE)), row.names = c(1L, 12L, 23L, 34L, 44L, 54L, 64L, 75L, 86L), class = "data.frame"), 
    mater_table = NULL, 
    h_vals = structure(
      list(
        analyte = structure(c(1L, 1L, 2L, 2L), .Label = c("Fe", "Mg"   ), class = "factor"),
        H_type = structure(c(1L, 2L, 1L, 2L), .Label = c("axial", "radial"), class = "factor"),
        mean = c(0.290477358348616, 0.293019826162251, 0.290477358348616, 0.293492295767011),
        n = c(3, 3, 3, 3), N = c(8L, 7L, 8L, 7L), MSamong = c(0.000161686454979397, 3.41774621935363e-05, 0.000161686454979397, 4.73597993286006e-05),
        MSwithin = c(0.000172808526267238, 6.88779071226825e-05,0.000172808526267238, 7.14527186024594e-05),
        P = c(0.506515192644675, 0.800936689331988, 0.506515192644675, 0.680855084728865),
        s_bb = c(0, 0, 0, 0),
        s_bb_min = c(0.015535927030583, 0.0100532811338686, 0.015535927030583, 0.0102229805818161)), class = "data.frame", row.names = c(NA, -4L)),
    t_H = NULL)
)