
#'@noRd
test_datreturn = function() {
  datreturnList = list(
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
  datreturn1 = do.call(shiny::reactiveValues, datreturnList)
  datreturn1 = reactiveClass$new(datreturn1)
  return(datreturn1)
}