testthat::test_that(
	desc = "fnc_load_xlsx works as expected",
	code = {
	  fn <- tempfile(fileext = ".xlsx")
	  x <- list("Input" = matrix(1:9, ncol = 3, dimnames = list(1:3, paste0("Header", 1:3))))
	  openxlsx::write.xlsx(x = x, file = fn, asTable = FALSE)
	  #openxlsx::read.xlsx(xlsxFile = fn, sheet = "Input")

	  # standard use (tidyxl)
	  out <- eCerto:::fnc_load_xlsx(filepath = fn, sheet = 1)
	  testthat::expect_true(is.data.frame(out))
	  testthat::expect_true(nrow(out)==4)

	  # standard use alternative openxlsx
	  out <- eCerto:::fnc_load_xlsx(filepath = fn, sheet = 1, method = "openxlsx")
	  testthat::expect_true(is.data.frame(out))
	  testthat::expect_true(nrow(out)==3)

	  # check if it works for reactive inputs
	  out <- eCerto:::fnc_load_xlsx(filepath = shiny::reactive({fn}), sheet = shiny::reactive({1}))
	  testthat::expect_true(is.data.frame(out))

	  # test if checks
	  testthat::expect_warning(eCerto:::fnc_load_xlsx(filepath = "C:/not_existent.file", sheet = 1))
	  testthat::expect_warning(eCerto:::fnc_load_xlsx(filepath = fn, sheet = 2))

	  openxlsx::write.xlsx(x = data.frame(NULL), file = fn)
	  #openxlsx::read.xlsx(xlsxFile = fn)
	  testthat::expect_warning(eCerto:::fnc_load_xlsx(filepath = fn, sheet = 1, method = "openxlsx"))

	  fn <- tempfile(fileext = ".txt")
	  cat("test", file = fn)
	  testthat::expect_error(eCerto:::fnc_load_xlsx(filepath = fn))
	  testthat::expect_warning(eCerto:::fnc_load_xlsx(filepath = fn, sheet = 1))

  }
)
