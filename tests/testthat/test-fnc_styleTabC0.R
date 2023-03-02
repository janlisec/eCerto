testthat::test_that("styleTabC0 works", {
  suppressMessages({
    rv <- eCerto:::test_rv(type = "SR3")
    x1 <- isolate(get_input_data(rv = rv))
    x2 <- isolate(get_input_data(rv = rv, excl_file=TRUE))
    x3 <- isolate(get_input_data(rv = rv, type="s"))
    x4 <- isolate(get_input_data(rv = rv, type="s", excl_file=TRUE))
    ap <- shiny::isolate(eCerto::getValue(rv, c("General", "apm"))[[rv$cur_an]])
    xs1 <- styleTabC0(x = x1, ap = ap, type=c("kompakt", "standard")[1])$x$data
    xs2 <- styleTabC0(x = x2, ap = ap, type=c("kompakt", "standard")[1])$x$data
    xs3 <- styleTabC0(x = x3, ap = ap, type=c("kompakt", "standard")[2])$x$data
    xs4 <- styleTabC0(x = x4, ap = ap, type=c("kompakt", "standard")[2])$x$data
  })
  testthat::expect_true(all(c("Lab", "R1", "mean", "File") %in% colnames(xs1)))
  testthat::expect_true(!all(c("File") %in% colnames(xs2)))
  testthat::expect_true(all(c("ID", "Lab", "value", "unit", "replicate", "File") %in% colnames(xs3)))
  testthat::expect_true(!all(c("File") %in% colnames(xs4)))
})
