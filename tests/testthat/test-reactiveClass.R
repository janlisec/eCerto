testthat::test_that(
  desc = "eCerto class works",
  code = {
    # set up empty eCerto object
    tmp <- eCerto::eCerto$new(rv = eCerto:::init_rv())
    testthat::expect_null(shiny::isolate(eCerto::getValue(tmp, c("Certification","data"))))

    # fill a value and check it setValue was successful
    shiny::isolate(eCerto::setValue(tmp, c("Certification","data"), 5))
    testthat::expect_equal(shiny::isolate(eCerto::getValue(tmp, c("Certification","data"))), 5)

    # create filled example eCerto object and...
    tmp <- eCerto::eCerto$new()
    # check if analyte is returned
    testthat::expect_equal(shiny::isolate(tmp$cur_an), "Si")
    # check if analytes are returned
    testthat::expect_equal(unname(tmp$c_analytes()), c("Si", "Fe", "Cu"))
    # check if lab_means are returned
    testthat::expect_true(is.data.frame(tmp$c_lab_means()))
    # check if plot can be generated
    shiny::isolate(tmp$c_plot())
    # check if analyte can be set
    shiny::isolate(tmp$cur_an <- "Cu")
    testthat::expect_equal(shiny::isolate(tmp$cur_an), "Cu")
    # check if flt_data are returned
    testthat::expect_true(is.data.frame(tmp$c_fltData()))
    # check if flt_data is recalculated based on apm
    x <- shiny::isolate(eCerto::getValue(tmp, c("General","apm")))
    x[[shiny::isolate(tmp$cur_an)]][["lab_filter"]] <- "L1"
    shiny::isolate(eCerto::setValue(tmp, c("General","apm"), x))
    testthat::expect_equal(sum(tmp$c_fltData(recalc = TRUE)[,"L_flt"]), 4)
  }
)
