testthat::test_that(
  desc = "fnc_preTabC0 works",
  code = {
    x <- list(
      data.frame(
        "A"=paste0("A",1:4),
        "U"=rep("U",4),
        "R1"=1:4,
        "R2"=rnorm(4),
        "File"=rep("F1",4)
      ),
      data.frame(
        "A"=paste0("A",1:4),
        "U"=rep("U",4),
        "R1"=5:8,
        "R2"=rep(NA,4),
        "File"=rep("F2",4)
      )
    )
    suppressMessages({ out <- eCerto:::prepTabC0(x) })
    testthat::expect_true(is.data.frame(out))
    testthat::expect_equal(out[1,"unit"], "U")
    testthat::expect_equal(colnames(out), c("ID","Lab","analyte","replicate","value","unit","File","S_flt","L_flt"))
  }
)
