


# Test 1 ---------------------------------------------------------------

# dr = init_datreturn()
#
# test_that("Button not able to click before init of materialtable",code = {
#   shiny::testServer(.TransferHomogeneityServer,
#              args = list(datreturn=dr), {
#                print(input$h_transfer_ubb_button)
#                # expect_message(
#                #   session$flushReact(), "add File column")
#                # # has File been added correctly after Upload
#                # expect_true("File" %in% colnames(rv$tab_flt[[1]]))
#                #
#                # # set rows and columns selection
#                # suppressMessages(
#                #   session$setInputs(uitab_cells_selected = cells_selected)
#                # )
#                # session$flushReact()
#                # expect_snapshot(rv$tab_flt)
#                # expect_equal(rv$end_col,6)
#              }
#   )
# })

homogData  = structure(
  list(
    analyte = structure(
      c(1L, 1L, 2L, 2L),
      .Label = c("Fe",
                 "Mg"),
      class = "factor"
    ),
    H_type = structure(
      c(1L, 2L, 1L, 2L),
      .Label = c("axial", "radial"),
      class = "factor"
    ),
    mean = c(
      0.290477358348616,
      0.293019826162251,
      0.290477358348616,
      0.293492295767011
    ),
    n = c(3,
          3, 3, 3),
    N = c(8L, 7L, 8L, 7L),
    MSamong = c(
      0.000161686454979397,
      3.41774621935363e-05,
      0.000161686454979397,
      4.73597993286006e-05
    ),
    MSwithin = c(
      0.000172808526267238,
      6.88779071226825e-05,
      0.000172808526267238,
      7.14527186024594e-05
    ),
    P = c(
      0.506515192644675,
      0.800936689331988,
      0.506515192644675,
      0.680855084728865
    ),
    s_bb = c(0, 0, 0, 0),
    s_bb_min = c(
      0.015535927030583,
      0.0100532811338686,
      0.015535927030583,
      0.0102229805818161
    )
  ),
  class = "data.frame",
  row.names = c(NA,-4L)
)

matTab = structure(
  list(
    analyte = c(
      "Si",
      "Fe",
      "Cu",
      "Mn",
      "Mg",
      "Cr",
      "Ni",
      "Zn",
      "Ti",
      "Sc",
      "Sn"

    ),
    mean = c(0.0494, NA,NA, NA, NA, NA, NA, NA, NA, NA, NA),
    F1 = c(1, 1, 1,1, 1, 1, 1, 1, 1, 1, 1),
    F2 = c(1, 1, 1, 1, 1, 1, 1, 1,1, 1, 1),
    F3 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    cert_val = c(0.0494, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    sd = c(0.0034, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    n = c(3L, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    char = c(0.0397366582033346, NA, NA, NA, NA, NA, NA,
             NA, NA, NA, NA),
    U2 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0),
    U3 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    U4 = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    U5 = c(0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    U6 = c(0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0),
    U7 = c(0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0),
    com = c(0.0397366582033346, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0),
    k = c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
    U = c(0.0794733164066691, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0)
  ),
  row.names = c(NA,-11L),
  col_code = structure(
    list(
      ID = c("F1", "F2", "F3", "U2", "U3", "U4", "U5", "U6", "U7"),
      Name = c("F1", "F2", "F3", "U2", "U3", "U4", "U5", "U6",
               "U7")
    ),
    class = "data.frame",
    row.names = c(NA,-9L)
  ),
  class = "data.frame"
)

test_that("Transfer successful", code = {
  shiny::testServer(
    m_TransferHomogeneityServer,
    args = list(
      matTab_col_code = reactiveVal(attr(matTab, "col_code")),
      matTab_analytes = reactiveVal(as.character(matTab[, "analyte"])),
      homogData = reactiveVal(homogData)
    ),
    {
      expect_equal(levels(homogData()[, "H_type"]), c("axial", "radial"))
      # session$flushReact()
      session$setInputs(h_transfer_ubb = "U3", h_transfer_H_type =
                          "axial")
      
      expect_equal(input$h_transfer_ubb, "U3")
      #expect_message(
      session$setInputs(h_transfer_ubb_button = "click")
      # ,"TRANSFER BUTTON clicked")
      # session$flushReact()
      
      expect_equal(
        session$returned(),
        structure(list(U3 = c(0, 0.015535927030583, 0, 0, 0.015535927030583,
                              0, 0, 0, 0, 0, 0)), row.names = c(NA, -11L), class = "data.frame")
      )
      
      #session$returned()
    }
  )
})


# Error test --------------------------------------------------------------

test_that("Transfer not possible with values not set", code = {
  shiny::testServer(
    m_TransferHomogeneityServer,
    args = list(
      matTab_col_code = reactiveVal(attr(matTab, "col_code")),
      matTab_analytes = reactiveVal(as.character(matTab[, "analyte"])),
      homogData = reactiveVal(homogData)
    ),
    {
      expect_equal(levels(homogData()[, "H_type"]), c("axial", "radial"))
      session$setInputs(h_transfer_ubb = "U3")# , h_transfer_H_type = "axial"
      
      expect_equal(input$h_transfer_ubb, "U3")
      session$setInputs(h_transfer_ubb_button = "click")
      
      expect_error(return_reactive())
      # expect_equal(
      #   session$returned(),
      #   structure(list(U3 = c(0, 0.015535927030583, 0, 0, 0.015535927030583, 
      #                         0, 0, 0, 0, 0, 0)), row.names = c(NA, -11L), class = "data.frame")
      # )
      #session$returned()
    }
  )
})

