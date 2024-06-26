#' @title An example set of data collected for a CRM.
#' @docType data
#' @format A list of length = 6 containing CRM test data.
#' @usage data(CRM001)
#' @source jan.lisec@@bam.de
"CRM001"

#' @title An example set of data collected for a LTS monitoring.
#' @docType data
#' @format A list of lists of length = 2 containing LTS test data.
#' @usage data(LTS001)
#' @source jan.lisec@@bam.de
"LTS001"

#' @title Dixon critical values table.
#' @docType data
#' @format A data frame containing Dixon critical values with n in rows and alpha in cols.
#' @usage data(cvals_Dixon)
#' @source <http://www.statistics4u.com/fundstat_eng/cc_outlier_tests_dixon.html>
"cvals_Dixon"

#' @title Grubbs2 critical values table.
#' @docType data
#' @format A data frame containing critical values for Double Grubbs test with n in rows and alpha in cols.
#' @usage data(cvals_Grubbs2)
#' @source `outliers` package and <https://link.springer.com/article/10.1007/s10182-011-0185-y>.
"cvals_Grubbs2"

#' @title test_Stability_Arrhenius.
#' @keywords internal
#' @noRd
test_Stability_Arrhenius <- function(seed = 4) {
  set.seed(seed)
  x <- data.frame(
    "analyte" = rep("a1", 39),
    "Date" = as.Date(rep(c("2022-01-01", "2022-01-15", "2022-02-01", "2022-04-01", "2023-01-01"), times = c(3, 9, 9, 9, 9))),
    "time" = c(0, 0, 0, rep(c(14, 30.42, 90, 365), each = 9)),
    "Value" = stats::rnorm(39, mean = 2),
    "Temp" = c(-20, -20, -20, rep(rep(c(4, 23, 60), each = 3), times = 4))
  )
  return(x)
}

#' @title test_rv.
#' @param type Select a specific data set.
#' @keywords internal
#' @noRd
test_rv <- function(type = c("generic", "SR3")) {
  type <- match.arg(type)
  rv <- eCerto$new(init_rv()) # initiate persistent variables
  testdata <- switch(type,
    "generic" = test_Certification_Excel(),
    "SR3" = {
      sr3 <- new.env()
      load(file = system.file(package = "eCerto", "extdata", "SR3_Fe_v26chs.RData"), envir = sr3)
      get0(x = "res", envir = sr3)$Certification$data_input
    }
  )
  apm <- init_apm(testdata)
  an <- sapply(apm, function(x) {
    x[["name"]]
  })
  mt <- init_materialtabelle(analytes = an)
  shiny::isolate({
    setValue(rv, c("Certification", "data"), testdata)
    setValue(rv, c("General", "apm"), apm)
    setValue(rv, c("General", "materialtabelle"), mt)
    rv$cur_an <- "Si"
    # ToDo: Example in m_report fails (analyte report acn not be exported to HTML)
    # this can be either solved in here by filling slots but should be better resolved
    # changing the param forwarded to the Rmd file to 'rv' and modifying the Rmd accordingly
    # setValue(
    #   rv, c("Certification_processing","stats"),
    #   prepTabC1(dat = rv$c_fltData(), lab_means = rv$c_lab_means(data = rv$c_fltData()))
    # )
    # setValue(
    #   rv, c("Certification_processing","mstats"),
    #   prepTabC2(dat = rv$c_fltData())
    # )
  })
  return(rv)
}

#' @title test_mod_xlsx_range.
#' @keywords internal
#' @noRd
test_mod_xlsx_range <- function() {
  fn <- paste0("Ergebnisblatt_BAM-M321_", c("Aleris_Koblenz", "Aleris_Duffel", "AMAG_Nasschemie"), "_m.xlsx")
  shiny::reactiveVal(
    structure(list(
      name = fn,
      size = c(27926L, 27617L, 27527L),
      type = rep("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", 3),
      datapath = sapply(fn, function(x) {
        system.file("extdata", x, package = "eCerto")
      }, USE.NAMES = FALSE)
    ), row.names = c(NA, -3L), class = "data.frame")
  )
}

#' @title test_homog.
#' @details The homogeneity test data are hard coded here using `structure` to
#'  potentially identify changes in the excel importer function. When tested,
#'  the excel importer will read the file "Homog_test.xlsx" from the folder
#'  `inst/extdata` and compare the result with the return value of this function.
#' @keywords internal
#' @noRd
test_homog <- function() {
  list(
    data = structure(list(
      analyte = c(rep("Fe", 45), rep("Mg", 45)),
      H_type = rep(rep(c("radial", "axial"), times = c(7, 8)), 6),
      Flasche = rep(c(3, 36, 62, 78, 109, 144, 162, 200, 225, 239, 256, 295, 325, 351, 397), 6),
      value = c(
        0.289769302010799, 0.296775267744762,
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
      unit = rep(c("mM/L", "mg/mL"), each = 45),
      File = rep("Homog_test.xlsx", times = 90)
    ), row.names = c(NA, 90L), class = "data.frame"),
    h_file = NULL, h_vals = NULL, h_sel_analyt = NULL, h_Fig_width = NULL
  )
}

#' @title test_Certification_Excel.
#' @keywords internal
#' @noRd
test_Certification_Excel <- function() {
  # After Upload of two Excel Files, what is saved in c(Certification,data) and Input to Certifications
  structure(
    list(
      ID = 1:24,
      Lab = rep(c("L1", "L2"), each = 12),
      analyte = structure(
        .Data = rep(c(1L, 2L, 3L), times = 8),
        .Label = c("Si", "Fe", "Cu"),
        class = "factor"
      ),
      replicate = structure(rep(rep(c(1L, 2L, 3L, 4L), each = 3), times = 2), .Label = c("1", "2", "3", "4"), class = "factor"),
      value = c(0.0504, 0.049, 4.37, 0.0512, 0.0563, 4.385, 0.0524, 0.0515, 4.34, 0.052, 0.0505, 4.388, 0.0452, 0.0529, 4.4048, 0.0435, 0.0527, 4.3802, 0.0472, 0.0482, 4.3907, 0.0456, 0.0478, 4.391),
      unit = rep(c("U", "kg/m2", "g/mL"), 8),
      S_flt = rep(FALSE, 24),
      L_flt = rep(FALSE, 24),
      File = rep(c("File1", "File2"), each = 12)
    ),
    class = "data.frame", row.names = c(NA, 24L)
  )
}

#' @title test_Stability_Excel.
#' @keywords internal
#' @noRd
test_Stability_Excel <- function() {
  # s_dat, after Upload and Output of the Uploading process
  structure(
    list(
      analyte = structure(
        .Data = rep(c(2, 1), each = 52), .Label = c("Mn", "Si"), class = "factor"
      ),
      Value = c(
        0.234, 0.235, 0.234, 0.2341, 0.2338, 0.2344, 0.2338, 0.2348, 0.2337,
        0.2334, 0.2344, 0.2351, 0.2347, 0.2346, 0.2344, 0.2319, 0.2337,
        0.234, 0.234, 0.2338, 0.234, 0.234, 0.234, 0.234, 0.2349, 0.2342,
        0.2338, 0.2342, 0.2327, 0.2343, 0.2336, 0.233, 0.2328, 0.2341,
        0.236, 0.2342, 0.2337, 0.2335, 0.2348, 0.2333, 0.2334, 0.2324,
        0.2347, 0.2326, 0.2358, 0.232, 0.2321, 0.2319, 0.2319, 0.2321,
        0.2311, 0.2324, 97.04, 97.35, 97, 97.62, 97.45, 97.49, 96.78,
        97.65, 98.06, 97.1141, 97.53, 97.11, 98.205, 97.8, 97.69, 97.46,
        96.11, 97.54, 96.9778, 95.79, 97.74, 97.45, 97.46, 97.23, 98.38,
        98.28, 96.6, 97.79, 96.78, 97.7, 97.78, 96.7, 97.9, 96.7, 98.33,
        98.47, 98.13, 98.2, 98.06, 98.2, 98.37, 98.08, 97.93, 96.93,
        98.43, 96.6, 97.3, 95.9, 97.3, 96.13, 96.04, 97.53
      ),
      Date = structure(
        .Data = c(
          15069, 15070, 15091, 15257, 15259, 15260, 15261, 15303, 15317,
          15352, 15442, 15513, 15575, 15642, 15688, 15741, 15867, 15869,
          15905, 15974, 15993, 16010, 16077, 16101, 16189, 16202, 16274,
          16316, 16385, 16395, 16511, 16568, 16590, 16610, 16699, 16805,
          16870, 16895, 16986, 17065, 17112, 17120, 17169, 17211, 17381,
          17403, 17501, 17575, 17603, 17653, 17721, 17756, 15069, 15070,
          15091, 15257, 15259, 15260, 15261, 15303, 15317, 15352, 15442,
          15513, 15575, 15642, 15688, 15741, 15867, 15869, 15905, 15974,
          15993, 16010, 16077, 16101, 16189, 16202, 16274, 16316, 16385,
          16395, 16511, 16568, 16590, 16610, 16699, 16805, 16870, 16895,
          16986, 17065, 17112, 17120, 17169, 17211, 17381, 17403, 17501,
          17575, 17603, 17653, 17721, 17756
        ),
        class = "Date"
      )
    ),
    row.names = c(NA, -104L), class = "data.frame"
  )
}
