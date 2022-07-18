testthat::test_that(
  desc = "Cochran-Test function is working",
  code = {
    # load external test data from collaborative trial SR3 and prepare eCerto object
    load(system.file(package = "eCerto","extdata","SR3_Fe_v26chs.RData"))
    rv <- eCerto$new(init_rv())
    shiny::isolate({
      setValue(rv, key = c("Certification","data"), value = res$Certification$data)
      setValue(rv, key = c("General","apm"), value = init_apm(res$Certification$data))
      suppressMessages(
        flt_data <- eCerto:::c_filter_data(x = getValue(rv, key = c("Certification","data")), c_apm = getValue(rv, c("General","apm"))[["Si"]])
      )
    })
    # test qualitative Cochran method
    out_alpha <- eCerto:::Cochran(data = flt_data, fmt = "alpha")
    testthat::expect_equal(class(out_alpha[,"Cochran"]), "character")
    testthat::expect_equal(out_alpha["L13","Cochran"], "[1] .01")
    testthat::expect_equal(out_alpha["L01","Cochran"], "[2] .05")
    # test quantitative Cochran method
    out_pval <- eCerto:::Cochran(data = flt_data, fmt = "pval")
    testthat::expect_equal(class(out_pval[,"Cochran"]), "numeric")
    testthat::expect_equal(out_pval["L13","Cochran"], 0.004890046)
    testthat::expect_equal(out_pval["L01","Cochran"], 0.014302227)
  }
)

testthat::test_that(
  desc = "Scheffe-Test function is working",
  code = {
    # load external test data from collaborative trial SR3 and prepare eCerto object
    load(system.file(package = "eCerto","extdata","SR3_Fe_v26chs.RData"))
    rv <- eCerto$new(init_rv())
    shiny::isolate({
      setValue(rv, key = c("Certification","data"), value = res$Certification$data)
      setValue(rv, key = c("General","apm"), value = init_apm(res$Certification$data))
      suppressMessages(
        flt_data <- eCerto:::c_filter_data(x = getValue(rv, key = c("Certification","data")), c_apm = getValue(rv, c("General","apm"))[["Si"]])
      )
    })
    # test Scheffe method
    out <- eCerto:::Scheffe(data = flt_data)
    testthat::expect_equal(class(out[,"Scheffe_05"]), "character")
    testthat::expect_equal(out[,"Scheffe_05"], c('f', 'ab', 'abc', 'e', 'f', 'de', 'cde', 'a', 'abcd', 'bcde'))
    testthat::expect_equal(class(out[,"Scheffe_01"]), "character")
    testthat::expect_equal(out[,"Scheffe_01"], c('d', 'ab', 'ab', 'c', 'd', 'c', 'bc', 'a', 'abc', 'bc'))
  }
)

testthat::test_that(
  desc = "Dixon-Test function is working",
  code = {
    # load external test data from collaborative trial SR3 and prepare eCerto object
    load(system.file(package = "eCerto","extdata","SR3_Fe_v26chs.RData"))
    #lab_means <- res$Certification$lab_means
    rv <- eCerto$new(init_rv())
    shiny::isolate({
      setValue(rv, key = c("Certification","data"), value = res$Certification$data)
      setValue(rv, key = c("General","apm"), value = init_apm(res$Certification$data))
      suppressMessages({
        rv$c_analyte <- "Si"
        lab_means <- rv$c_lab_means()
      })
    })
    # test qualitative Dixon method
    out_alpha <- eCerto:::Dixon(lab_means = lab_means, fmt = "alpha")
    testthat::expect_equal(class(out_alpha[,"Dixon_p"]), "character")
    testthat::expect_equal(out_alpha[,"Dixon_p"], c('.', '.', '.', '.', 'n.s.', '.', '.', 'n.s.', '.', '.'))
    # test quantitative Dixon method
    out_pval <- eCerto:::Dixon(lab_means = lab_means, fmt = "pval")
    testthat::expect_equal(class(out_pval[,"Dixon_p"]), "numeric")
    testthat::expect_equal(out_pval[c("L07","L13"),"Dixon_p"], c(0.98506944, 0.60879367))
  }
)

testthat::test_that(
  desc = "Grubbs-Test function is working",
  code = {
    # load external test data from collaborative trial SR3 and prepare eCerto object
    load(system.file(package = "eCerto","extdata","SR3_Fe_v26chs.RData"))
    rv <- eCerto$new(init_rv())
    shiny::isolate({
      setValue(rv, key = c("Certification","data"), value = res$Certification$data)
      setValue(rv, key = c("General","apm"), value = init_apm(res$Certification$data))
      suppressMessages({
        rv$c_analyte <- "Si"
        lab_means <- rv$c_lab_means()
      })
    })
    # test qualitative Grubbs method
    out_alpha <- eCerto:::Grubbs(lab_means = lab_means, fmt = "alpha")
    testthat::expect_equal(class(out_alpha[,"Grubbs1_p"]), "character")
    testthat::expect_equal(class(out_alpha[,"Grubbs2_p"]), "character")
    testthat::expect_equal(out_alpha[,"Grubbs1_p"], c('.', '.', '.', '.', 'n.s.', '.', '.', 'n.s.', '.', '.'))
    testthat::expect_equal(out_alpha[,"Grubbs2_p"], c('n.s.', 'n.s.', '.', '.', 'n.s.', '.', '.', 'n.s.', '.', '.'))
    # test quantitative Grubbs method
    out_pval <- eCerto:::Grubbs(lab_means = lab_means, fmt = "pval")
    testthat::expect_equal(class(out_pval[,"Grubbs1_p"]), "numeric")
    testthat::expect_equal(class(out_pval[,"Grubbs2_p"]), "numeric")
    testthat::expect_equal(out_pval[c("L07","L13"),"Grubbs1_p"], c(0.45250505, 0.90129343))
    testthat::expect_equal(out_pval[c("L01","L02","L07","L13"),"Grubbs2_p"], c(0.105435887, 0.840986945, 0.105435887, 0.840986945))
  }
)

testthat::test_that(
  desc = "Nalimov-Test function is working",
  code = {
    # load external test data from collaborative trial SR3 and prepare eCerto object
    load(system.file(package = "eCerto","extdata","SR3_Fe_v26chs.RData"))
    #lab_means <- res$Certification$lab_means
    rv <- eCerto$new(init_rv())
    shiny::isolate({
      setValue(rv, key = c("Certification","data"), value = res$Certification$data)
      setValue(rv, key = c("General","apm"), value = init_apm(res$Certification$data))
      suppressMessages({
        rv$c_analyte <- "Si"
        lab_means <- rv$c_lab_means()
      })
    })
    # test qualitative Nalimov method
    out <- eCerto:::Nalimov(lab_means = lab_means)
    testthat::expect_equal(class(out[,"Nalimov"]), "character")
    testthat::expect_equal(out[,"Nalimov"], rep('.', 10))
    out2 <- sapply(seq(0.040,0.045, by=0.0005), function(x) {
      lab_means[1,"mean"] <- x
      eCerto:::Nalimov(lab_means = lab_means)[1,1]
    })
    testthat::expect_equal(out2, rep(c('.01',".05","."), times=c(5,5,1)))
    # currently no quantitative Nalimov method
  }
)
