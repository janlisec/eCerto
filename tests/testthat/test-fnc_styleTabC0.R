testthat::test_that("styleTabC0 works", {
  suppressMessages({
    rv <- eCerto:::test_rv(type = "SR3")
    x1 <- shiny::isolate(eCerto:::get_input_data(rv = rv))
    x2 <- shiny::isolate(eCerto:::get_input_data(rv = rv, excl_file=TRUE))
    x3 <- shiny::isolate(eCerto:::get_input_data(rv = rv, type="s"))
    x4 <- shiny::isolate(eCerto:::get_input_data(rv = rv, type="s", excl_file=TRUE))
    ap <- shiny::isolate(eCerto::getValue(rv, c("General", "apm"))[[rv$cur_an]])
    xs1 <- eCerto:::styleTabC0(x = x1, ap = ap, type=c("compact", "standard")[1])$x$data
    xs2 <- eCerto:::styleTabC0(x = x2, ap = ap, type=c("compact", "standard")[1])$x$data
    xs3 <- eCerto:::styleTabC0(x = x3, ap = ap, type=c("compact", "standard")[2])$x$data
    xs4 <- eCerto:::styleTabC0(x = x4, ap = ap, type=c("compact", "standard")[2])$x$data
    ap[["sample_filter"]] <- 1
    ap[["lab_filter"]] <- "L01"
    xs5 <- eCerto:::styleTabC0(x = x1, ap = ap, type=c("compact", "standard")[1])
  })
  testthat::expect_true(all(c("Lab", "R1", "mean", "File") %in% colnames(xs1)))
  testthat::expect_true(!all(c("File") %in% colnames(xs2)))
  testthat::expect_true(all(c("ID", "Lab", "value", "unit", "replicate", "File") %in% colnames(xs3)))
  testthat::expect_true(!all(c("File") %in% colnames(xs4)))
  testthat::expect_true(inherits(xs5, "datatables"))
})
