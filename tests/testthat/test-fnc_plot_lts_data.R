s <- eCerto:::test_Stability_Excel()
apm <- list("Mn"=list("confirmed"=TRUE))
mt <- data.frame("analyte"="Mn", "cert_val"=1, "U_abs"=1, "sd"=1, "unit"="unit")

testthat::test_that(
  desc = "plot_lts_data works",
  code = {
    x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm=apm)
    out <- eCerto:::plot_lts_data(x = x_prep, type = 0)
    testthat::expect_true(is.numeric(out))
    testthat::expect_equal(names(out), "2184-02-22")
    testthat::expect_equal(unname(out), 2076)

    out <- eCerto:::plot_lts_data(x = x_prep, type = 0, slope_of_means = TRUE)
    testthat::expect_equal(unname(out), 2076)

    skip_if_not_linux_ci()

    x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm=apm, U_Def="U", mt=mt)
    vdiffr::expect_doppelganger(
      title = "FigS1 type1",
      fig = function() eCerto:::plot_lts_data(x=x_prep, type = 1)
    )
    out2 <- eCerto:::plot_lts_data(x=x_prep, type = 2)
    testthat::expect_equal(names(out2), "2135-01-04")
    vdiffr::expect_doppelganger(
      title = "FigS1 type2",
      fig = function() eCerto:::plot_lts_data(x=x_prep, type = 2)
    )
    x_prep[["def"]][,"KW"] <- "test_text"
    x_prep[["val"]][,"Comment"] <- c(rep(NA, 51), "test_text")
    vdiffr::expect_doppelganger(
      title = "FigS1 type1 with KW",
      fig = function() eCerto:::plot_lts_data(x=x_prep, type = 1)
    )
  }
)

testthat::test_that(
  desc = "plot_lts_data type=2 works",
  code = {
    x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm=apm, U_Def="U", mt=mt)
    out2 <- eCerto:::plot_lts_data(x=x_prep, type = 2)
    testthat::expect_equal(names(out2), "2135-01-04")
  }
)

testthat::test_that(
  desc = "plot_lts_data Comment points are colored",
  code = {
    x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm=apm, U_Def="U", mt=mt)
    x_prep[["val"]][,"Comment"] <- c(rep(NA, 51), "test_text")
    out3 <- eCerto:::plot_lts_data(x=x_prep, type = 0)
    testthat::expect_equal(names(out3), "2135-01-04")
  }
)

testthat::test_that(
  desc = "plot_lts_data type=3 works",
  code = {
    x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm=apm, U_Def="U", mt=mt)
    out4 <- eCerto:::plot_lts_data(x=x_prep, type = 3)
    testthat::expect_equal(names(out4), "2024-07-01")
  }
)

testthat::test_that(
  desc = "plot_lts_data type=3 works with legend",
  code = {

    skip_if_not_linux_ci()

    x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm=apm, U_Def="U", mt=mt)
    vdiffr::expect_doppelganger(
      title = "FigS1 type3 with Legend",
      fig = function() eCerto:::plot_lts_data(x=x_prep, type = 3, show_legend = TRUE)
    )
  }
)
