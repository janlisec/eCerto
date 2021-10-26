testthat::test_that("Successfull Upload Homogeneity",code = {
  homog = shiny::reactiveVal({eCerto:::test_homog()})
  suppressMessages(
    shiny::testServer(
      app = eCerto::page_HomogeneityServer,
      args = list(
        homog = homog,
        cert = reactiveVal(NULL),
        datreturn = eCerto:::test_datreturn()
      ),
      expr =  {
        session$flushReact()
        testthat::expect_equal(
          session$returned(),
          structure(
            list(
              analyte = structure(c(1L, 1L, 2L, 2L), .Label = c("Fe", "Mg"), class = "factor"),
              H_type = structure(c(1L, 2L, 1L, 2L), .Label = c("axial", "radial"), class = "factor"),
              mean = c(0.290477358348616, 0.293019826162251, 0.290477358348616, 0.293492295767011),
              n = c(3, 3, 3, 3),
              N = c(8L, 7L, 8L, 7L),
              MSamong = c(0.0001616864549794, 3.41774621935367e-05, 0.0001616864549794, 4.73597993286004e-05 ),
              MSwithin = c(0.000172808526267237, 6.88779071226827e-05, 0.000172808526267237,7.14527186024597e-05),
              P = c(0.506515192644658, 0.800936689331986, 0.506515192644658, 0.680855084728869),
              s_bb = c(0, 0, 0, 0),
              s_bb_min = c(0.0155359270305829, 0.0100532811338686, 0.0155359270305829,  0.0102229805818161)),
            class = "data.frame", row.names = c(NA, -4L)))
        # print(session$returned())
      }
    )
  )
})

testthat::test_that("Successful Upload Homogeneity and Certification",code = {
  homog = shiny::reactiveVal({eCerto:::test_homog()})
  cert = shiny::reactiveVal({eCerto:::test_certification()})
  shiny::testServer(
    app = eCerto::page_HomogeneityServer,
    args = list(
      homog = homog,
      cert = cert,
      datreturn = eCerto:::test_datreturn()
    ),
    expr =  {
      session$setInputs(h_precision = 3)
      session$flushReact()
      testthat::expect_equal(
        as.character(h_vals_print()[,"In_Cert_Module"]),
        rep("Yes",4)
      )
    }
  )
})
