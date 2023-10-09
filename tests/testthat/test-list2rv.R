testthat::test_that(
  desc = "list2rv works",
  code = {
    # even an empty list should return a (default) 'eCerto' object
    tmp <- list()
    out <- eCerto:::list2rv(x=tmp)
    testthat::expect_true(inherits(out, "eCerto"))
    # example data should run smoothly
    tmp <- eCerto::CRM001
    out <- eCerto:::list2rv(x=tmp)
    testthat::expect_true(all(c("c_fltData", "c_analytes", "c_lab_means", "c_plot", "set", "get") %in% names(out)))
    # warn if more elements in source list than expected
    tmp <- c(tmp, "prove_error"="test")
    testthat::expect_warning(eCerto:::list2rv(x=tmp))
    # warn if expected elements are missing in source list
    tmp <- eCerto::CRM001
    testthat::expect_equal(isolate(getValue(eCerto:::list2rv(x=tmp),c("General","user"))), "Jan Lisec")
    tmp[["General"]][["user"]] <- NULL
    testthat::expect_null(isolate(getValue(eCerto:::list2rv(x=tmp),c("General","user"))))
})
