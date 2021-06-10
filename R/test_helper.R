
#' @keywords internal
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

#' @keywords internal
test_mod_xlsx_range = function() {
  shiny::reactiveVal(structure(list(
    name = c(
      "Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx",
      "Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx",
      "Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx"),
    size = c(27926L, 27617L, 27527L),
    type = c("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
             "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"),
    datapath = c(
      system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"),
      system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"),
      system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
    )
  ),
  row.names = c(NA,-3L),
  class = "data.frame"
  ))
}

#' @keywords internal
test_homog = function() {
  list(data = structure(list(
    analyte = c(rep("Fe",45), rep("Mg",45)),
    H_type = c("radial", 
               "radial", "radial", "radial", "radial", "radial", "radial", "axial", 
               "axial", "axial", "axial", "axial", "axial", "axial", "axial", 
               "radial", "radial", "radial", "radial", "radial", "radial", "radial", 
               "axial", "axial", "axial", "axial", "axial", "axial", "axial", 
               "axial", "radial", "radial", "radial", "radial", "radial", "radial", 
               "radial", "axial", "axial", "axial", "axial", "axial", "axial", 
               "axial", "axial", "radial", "radial", "radial", "radial", "radial", 
               "radial", "radial", "axial", "axial", "axial", "axial", "axial", 
               "axial", "axial", "axial", "radial", "radial", "radial", "radial", 
               "radial", "radial", "radial", "axial", "axial", "axial", "axial", 
               "axial", "axial", "axial", "axial", "radial", "radial", "radial", 
               "radial", "radial", "radial", "radial", "axial", "axial", "axial", 
               "axial", "axial", "axial", "axial", "axial"), 
    Flasche = c(3, 
                36, 62, 78, 109, 144, 162, 200, 225, 239, 256, 295, 325, 351, 
                397, 3, 36, 62, 78, 109, 144, 162, 200, 225, 239, 256, 295, 325, 
                351, 397, 3, 36, 62, 78, 109, 144, 162, 200, 225, 239, 256, 295, 
                325, 351, 397, 3, 36, 62, 78, 109, 144, 162, 200, 225, 239, 256, 
                295, 325, 351, 397, 3, 36, 62, 78, 109, 144, 162, 200, 225, 239, 
                256, 295, 325, 351, 397, 3, 36, 62, 78, 109, 144, 162, 200, 225, 
                239, 256, 295, 325, 351, 397), 
    value = c(0.289769302010799, 0.296775267744762, 
              0.307595169154081, 0.300664250302984, 0.29811301754119, 0.301859235605158, 
              0.305164626561129, 0.293492422937789, 0.236992793889049, 0.299077096041326, 
              0.29164790303274, 0.298173200433747, 0.303313470041546, 0.308457017250942, 
              0.298846106640478, 0.293802481871515, 0.291392362971851, 0.291094800663056, 
              0.283585735787096, 0.298514721954372, 0.288477795851408, 0.294097538322355, 
              0.286553380479238, 0.294097538322355, 0.29656807690673, 0.290716021312024, 
              0.291126983269754, 0.288429945088968, 0.283975057155655, 0.296777621789265, 
              0.277709047077942, 0.288665439616136, 0.294864773778629, 0.292476575796996, 
              0.279400384647449, 0.291891197400404, 0.287502624747949, 0.289597686099481, 
              0.287421387249232, 0.29007503820822, 0.299880243325049, 0.283940874023323, 
              0.286604696487606, 0.290288015380048, 0.285404025002208, 0.289769302010799, 
              0.296775267744762, 0.307595169154081, 0.300664250302984, 0.29811301754119, 
              0.301859235605158, 0.305164626561129, 0.293492422937789, 0.236992793889049, 
              0.299077096041326, 0.29164790303274, 0.298173200433747, 0.303313470041546, 
              0.308457017250942, 0.298846106640478, 0.293802481871515, 0.291392362971851, 
              0.291094800663056, 0.283585735787096, 0.298514721954372, 0.288477795851408, 
              0.304019400022322, 0.286553380479238, 0.294097538322355, 0.29656807690673, 
              0.290716021312024, 0.291126983269754, 0.288429945088968, 0.283975057155655, 
              0.296777621789265, 0.277709047077942, 0.288665439616136, 0.294864773778629, 
              0.292476575796996, 0.279400384647449, 0.291891197400404, 0.287502624747949, 
              0.289597686099481, 0.287421387249232, 0.29007503820822, 0.299880243325049, 
              0.283940874023323, 0.286604696487606, 0.290288015380048, 0.285404025002208
    ), 
    unit = c("mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", 
             "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", 
             "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", 
             "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", 
             "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", 
             "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mM/L", "mg/mL", "mg/mL", 
             "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", 
             "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", 
             "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", 
             "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", 
             "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", 
             "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", "mg/mL", 
             "mg/mL"), 
    File = rep("Homog_test.xlsx", 90)), row.names = c(NA, 
                                                      90L), class = "data.frame"), uploadsource = "Excel", h_file = NULL, 
    h_vals = NULL, h_sel_analyt = NULL, h_precision = NULL, h_Fig_width = NULL)
}

test_certification = function() {
  list(data = structure(list(
    ID = 1:162, 
    Lab = c(rep("L1",54),rep("L2",54),rep("L3",54)), 
    analyte = structure(c(1L, 2L,  3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
                          1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 
                          8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 
                          6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 
                          4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 
                          2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 
                          9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 
                          7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 
                          5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 
                          3L, 4L, 5L, 6L, 7L, 8L, 9L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L
    ), .Label = c("Si", "Fe", "Cu", "Mn", "Mg", "Cr", "Ni", "Zn", 
                  "Ti"), class = "factor"), 
    replicate = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L,  3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 
                            4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 
                            6L, 6L, 6L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 
                            2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 
                            4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 
                            6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 
                            1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 
                            3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L, 
                            5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L), .Label = c("1", 
                                                                                                    "2", "3", "4", "5", "6"), class = "factor"), 
    value = c(0.0504, 0.049, 4.37, 0.805, 1.507, 0.0549, 0.0496, 0.1459, 0.0434, 0.0512, 
              0.0563, 4.385, 0.8089, 1.511, 0.0552, 0.0497, 0.1471, 0.0435, 
              0.0524, 0.0515, 4.34, 0.809, 1.506, 0.055, 0.0498, 0.1474, 0.0434, 
              0.052, 0.0505, 4.388, 0.813, 1.509, 0.0555, 0.0506, 0.1498, 0.0435, 
              0.052, 0.0505, 4.329, 0.809, 1.498, 0.0551, 0.0499, 0.1487, 0.0433, 
              0.051, 0.0505, 4.365, 0.812, 1.511, 0.0554, 0.0505, 0.1501, 0.0435, 
              0.0452, 0.0529, 4.4048, 0.8179, 1.5179, 0.05485, 0.0497, 0.1463, 
              0.043, 0.0435, 0.0527, 4.3802, 0.8084, 1.4988, 0.0547, 0.0493, 
              0.1444, 0.0425, 0.0472, 0.0482, 4.3907, 0.8112, 1.517, 0.0542, 
              0.0495, 0.1486, 0.0429, 0.0456, 0.0478, 4.391, 0.8116, 1.5048, 
              0.0544, 0.049, 0.148, 0.0431, 0.0464, 0.0488, 4.3798, 0.8098, 
              1.5085, 0.0552, 0.0498, 0.1466, 0.0426, 0.0452, 0.0466, 4.3924, 
              0.8112, 1.5102, 0.0542, 0.0496, 0.1478, 0.0428, 0.0511, 0.0495, 
              4.4483, 0.8054, 1.5007, 0.0539, 0.05, 0.1526, 0.0433, 0.0508, 
              0.0489, 4.4666, 0.8048, 1.4995, 0.0538, 0.0501, 0.1526, 0.0432, 
              0.0515, 0.05, 4.4623, 0.8063, 1.5008, 0.0538, 0.05, 0.1529, 0.0434, 
              0.0508, 0.0499, 4.4674, 0.8058, 1.5016, 0.054, 0.0504, 0.1548, 
              0.0433, 0.0513, 0.0498, 4.4826, 0.8082, 1.5115, 0.0542, 0.0506, 
              0.1557, 0.0435, 0.0521, 0.0496, 4.4857, 0.8082, 1.5107, 0.0542, 
              0.0504, 0.155, 0.0434), 
    unit = c("0.05", "0.05", "4.3", "0.8", 
             "1.5", "0.05", "0.05", "0.14", "0.05", "0.05", "0.05", "4.3", 
             "0.8", "1.5", "0.05", "0.05", "0.14", "0.05", "0.05", "0.05", 
             "4.3", "0.8", "1.5", "0.05", "0.05", "0.14", "0.05", "0.05", 
             "0.05", "4.3", "0.8", "1.5", "0.05", "0.05", "0.14", "0.05", 
             "0.05", "0.05", "4.3", "0.8", "1.5", "0.05", "0.05", "0.14", 
             "0.05", "0.05", "0.05", "4.3", "0.8", "1.5", "0.05", "0.05", 
             "0.14", "0.05", "0.05", "0.05", "4.3", "0.8", "1.5", "0.05", 
             "0.05", "0.14", "0.05", "0.05", "0.05", "4.3", "0.8", "1.5", 
             "0.05", "0.05", "0.14", "0.05", "0.05", "0.05", "4.3", "0.8", 
             "1.5", "0.05", "0.05", "0.14", "0.05", "0.05", "0.05", "4.3", 
             "0.8", "1.5", "0.05", "0.05", "0.14", "0.05", "0.05", "0.05", 
             "4.3", "0.8", "1.5", "0.05", "0.05", "0.14", "0.05", "0.05", 
             "0.05", "4.3", "0.8", "1.5", "0.05", "0.05", "0.14", "0.05", 
             "0.05", "0.05", "4.3", "0.8", "1.5", "0.05", "0.05", "0.14", 
             "0.05", "0.05", "0.05", "4.3", "0.8", "1.5", "0.05", "0.05", 
             "0.14", "0.05", "0.05", "0.05", "4.3", "0.8", "1.5", "0.05", 
             "0.05", "0.14", "0.05", "0.05", "0.05", "4.3", "0.8", "1.5", 
             "0.05", "0.05", "0.14", "0.05", "0.05", "0.05", "4.3", "0.8", 
             "1.5", "0.05", "0.05", "0.14", "0.05", "0.05", "0.05", "4.3", 
             "0.8", "1.5", "0.05", "0.05", "0.14", "0.05"), 
    S_flt = rep(FALSE,162), 
    L_flt = rep(FALSE,162)), 
    class = "data.frame", row.names = c(NA, 162L)), 
    input_files = NULL, uploadsource = "Excel", user = "FK", 
    study_id = "TEST", time_stamp = structure(0, class = "Date"), 
    dataformat_version = "2021-05-27", lab_means = NULL, cert_mean = NULL, 
    cert_sd = NULL, normality_statement = NULL, precision = NULL, 
    data_kompakt = NULL, CertValPlot = NULL, stats = NULL, boxplot = NULL, 
    opt = NULL, mstats = NULL, materialtabelle = NULL)
}
