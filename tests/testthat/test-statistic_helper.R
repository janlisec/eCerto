# load external test data from collaborative trial SR3 and prepare eCerto object
rv <- eCerto:::test_rv(type = "SR3")
flt_data <- rv$c_fltData()
err_data <- data.frame("Lab"=gl(1,10), value=1:10)
err_data2 <- data.frame("Lab"=gl(1,10), value=rep(1,10))
lab_means <- shiny::isolate(rv$c_lab_means())
err_means <- data.frame("Lab"=gl(3,1), "mean"=1:3, "sd"=rep(0,3), "n"=rep(3,3))

testthat::test_that(
  desc = "Cochran-Test function is working",
  code = {
    # test qualitative Cochran method
    out_alpha <- eCerto:::Cochran(data = flt_data, fmt = "alpha")
    testthat::expect_equal(class(out_alpha[,"Cochran"]), "character")
    testthat::expect_equal(out_alpha["L13","Cochran"], "[1] .01")
    testthat::expect_equal(out_alpha["L01","Cochran"], "[2] .05")
    # test quantitative Cochran method
    out_pval <- eCerto:::Cochran(data = flt_data, fmt = "pval")
    testthat::expect_equal(class(out_pval[,"Cochran"]), "numeric")
    testthat::expect_equal(out_pval["L13","Cochran"], 0.0048881972)
    testthat::expect_equal(out_pval["L01","Cochran"], 0.014294748)
    # test if 'Error' is returned for bad input data
    out_err <- eCerto:::Cochran(data = err_data2, fmt = "alpha")
    testthat::expect_equal(out_err[1,"Cochran"], "excl")
    out_err <- eCerto:::Cochran(data = err_data, fmt = "pval")
    testthat::expect_equal(out_err[1,"Cochran"], NA)
  }
)

testthat::test_that(
  desc = "Scheffe-Test function is working",
  code = {
    # test Scheffe method
    out <- eCerto:::Scheffe(data = flt_data)
    testthat::expect_equal(class(out[,"Scheffe_05"]), "character")
    testthat::expect_equal(out[,"Scheffe_05"], c('f', 'ab', 'abc', 'e', 'f', 'de', 'cde', 'a', 'abcd', 'bcde'))
    testthat::expect_equal(class(out[,"Scheffe_01"]), "character")
    testthat::expect_equal(out[,"Scheffe_01"], c('d', 'ab', 'ab', 'c', 'd', 'c', 'bc', 'a', 'abc', 'bc'))
    # test if 'Error' is returned for bad input data
    out_err <- eCerto:::Scheffe(data = err_data)
    testthat::expect_equal(out_err[,"Scheffe_01"], "Error")
  }
)

testthat::test_that(
  desc = "Dixon-Test function is working",
  code = {
    # test qualitative Dixon method
    out_alpha <- eCerto:::Dixon(lab_means = lab_means, fmt = "alpha")
    testthat::expect_equal(class(out_alpha[,"Dixon"]), "character")
    testthat::expect_equal(out_alpha[,"Dixon"], c('.', '.', '.', '.', 'n.s.', '.', '.', 'n.s.', '.', '.'))
    # test quantitative Dixon method
    out_pval <- eCerto:::Dixon(lab_means = lab_means, fmt = "pval")
    testthat::expect_equal(class(out_pval[,"Dixon"]), "numeric")
    testthat::expect_equal(out_pval[c("L07","L13"),"Dixon"], c(0.98506944, 0.60879367))
    # test if 'Error' is returned for bad input data
    out_err <- eCerto:::Dixon(lab_means = err_means[1:2,], fmt = "alpha")
    testthat::expect_equal(out_err[1,"Dixon"], "n<3")
  }
)

testthat::test_that(
  desc = "Grubbs-Test function is working",
  code = {
    # test qualitative Grubbs method
    out_alpha <- eCerto:::Grubbs(lab_means = lab_means, fmt = "alpha")
    testthat::expect_equal(class(out_alpha[,"Grubbs1"]), "character")
    testthat::expect_equal(class(out_alpha[,"Grubbs2"]), "character")
    testthat::expect_equal(out_alpha[,"Grubbs1"], c('.', '.', '.', '.', 'n.s.', '.', '.', 'n.s.', '.', '.'))
    testthat::expect_equal(out_alpha[,"Grubbs2"], c('n.s.', 'n.s.', '.', '.', 'n.s.', '.', '.', 'n.s.', '.', '.'))
    # test quantitative Grubbs method
    out_pval <- eCerto:::Grubbs(lab_means = lab_means, fmt = "pval")
    testthat::expect_equal(class(out_pval[,"Grubbs1"]), "numeric")
    testthat::expect_equal(class(out_pval[,"Grubbs2"]), "numeric")
    testthat::expect_equal(out_pval[c("L07","L13"),"Grubbs1"], c(0.45402331, 0.90086514))
    testthat::expect_equal(out_pval[c("L01","L02","L07","L13"),"Grubbs2"], c(0.10668752, 0.84065467, 0.10668752, 0.84065467))
    # test if 'Error' is returned for bad input data
    out_err <- eCerto:::Grubbs(lab_means = err_means[1:2,], fmt = "alpha")
    testthat::expect_equal(out_err[1,"Grubbs1"], "Error")
  }
)
