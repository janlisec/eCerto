#' @title fnc_load_xlsx.
#'
#' @description \code{fnc_load_xlsx} will handle upload of a single Excel file.
#'
#' @details Function can handle reactive and non reactive parameters to be used in shiny apps.
#'
#' @param filepath A list of imported Excel tables.
#' @param sheet The name or number of the sheet to read from files in `filepath`.
#' @param method Either 'tidyxl' or 'openxlsx'.
#' @param ... Further parameters to the read functions specified by method.
#'
#' @examples
#' # test function with
#' x <- tempfile(fileext = ".xlsx")
#' openxlsx::write.xlsx(x = matrix(rnorm(9), ncol = 3, dimnames = list(1:3, paste0("Header", 1:3))), file = x)
#' eCerto:::fnc_load_xlsx(filepath = x, sheet = 1)
#' eCerto:::fnc_load_xlsx(filepath = x, sheet = 1, method = "openxlsx")
#' eCerto:::fnc_load_xlsx(filepath = "C:/not_existent.file", sheet = 1)
#' eCerto:::fnc_load_xlsx(filepath = x, sheet = 2)
#' x <- system.file("extdata", "EmptyExcel.xlsx", package = "ecerto")
#' eCerto:::fnc_load_xlsx(filepath = x, sheet = 1, method = "openxlsx")
#'
#' @return A dataframe.
#'
#' @noRd
#' @keywords internal
fnc_load_xlsx <- function(filepath, sheet, method = c("tidyxl", "openxlsx"), ...) {
  method <- match.arg(method)
  # isolate reactive variables if provided
  if (shiny::is.reactive(filepath)) filepath <- shiny::isolate(filepath())
  if (shiny::is.reactive(sheet)) sheet <- shiny::isolate(sheet())
  # make some tests
  if (!file.exists(filepath)) {
    warning("Invalid file; File-Path does not exist")
    return(NULL)
  }
  if (!any(grep("[Xx][Ll][Ss][Xx]", tools::file_ext(filepath)))) {
    warning("Invalid file; Please upload a .xlsx file")
    return(NULL)
  }
  if (!sheet %in% 1:length(openxlsx::getSheetNames(filepath))) {
    warning("Invalid sheet; Sheet number does not exist")
    return(NULL)
  }

  # load file with specified method
  a <- switch(method,
    "tidyxl" = tidyxl::xlsx_cells(path = filepath, sheets = sheet, include_blank_cells = FALSE, ...),
    "openxlsx" = openxlsx::read.xlsx(xlsxFile = filepath, sheet = sheet, detectDates = TRUE, ...)
  )

  # post process data
  if (method == "tidyxl") {
    # in case, the uploaded excel is empty/contains no information in cells return NULL
    # if (nrow(a[,"row"]) == 0) return(NULL)
    if (nrow(a) == 0) {
      return(NULL)
    }
    out <- matrix("",
      nrow = max(a[, "row"]), ncol = max(a[, "col"]),
      dimnames = list(1:max(a[, "row"]), LETTERS[1:max(a[, "col"])])
    )
    # print(out)

    for (tp in c("numeric", "character")) {
      flt <- which(a[, "data_type"] == tp)
      if (length(flt) >= 1) {
        for (i in flt) {
          out[as.numeric(a[i, "row"]), as.numeric(a[i, "col"])] <- as.character(a[i, tp])
        }
      }
    }
    # print(out)
    out <- as.data.frame(out)
  } else {
    out <- a
  }

  return(out)
}
