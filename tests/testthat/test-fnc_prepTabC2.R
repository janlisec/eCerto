testthat::test_that(
  desc = "fnc_preTabC2 works",
  code = {
    x <- eCerto:::test_Certification_Excel()
    # Anscombe-Test returns a warning for this test data
    suppressWarnings({
      out <- eCerto:::prepTabC2(dat = x)
    })
    testthat::expect_true(is.data.frame(out))
    testthat::expect_equal(out[,"Median"], 1.4935875, tolerance = 8)
    testthat::expect_equal(out[,"SD"], 0.003152518, tolerance = 8)
    x2 <- x
    x2[,"Lab"] <- gsub("L2", "L4", gsub("L1", "L3", x2[,"Lab"]))
    x2[,"value"] <- sample(x2[,"value"])
    x3 <- rbind(x, x2)
    out <- eCerto:::prepTabC2(dat = x3, excl_labs = TRUE)
    testthat::expect_equal(out[,"SD"], 0.002521652, tolerance = 8)
  }
)
