#' @title read_lts_input.
#'
#' @description Reads Excel function for Long-term-stability data.
#'
#' @param file Path to Excel File.
#' @param simplify Try to simplify list of imported Excel files into data frame.
#'
#' @return List of data frames from worksheets of the imported Excel file or a
#'     single data frame if simplify = TRUE.
#'
#' @importFrom openxlsx read.xlsx getSheetNames
#'
#' @examples
#' fl <- system.file("extdata", "eCerto_LTS_example_input.xlsx", package = "eCerto")
#' read_lts_input(file = fl)
#' read_lts_input(file = fl, simplify = TRUE)
#'
#' @noRd
#' @keywords internal
read_lts_input <- function(file = NULL, simplify = FALSE) {
  sheets <- openxlsx::getSheetNames(file = file)
  out <- vector("list", length(sheets))
  for (i in 1:length(sheets)) {
    out[[i]][["def"]] <- openxlsx::read.xlsx(xlsxFile = file, sheet = i, startRow = 1, rows = 1:2)
    out[[i]][["val"]] <- openxlsx::read.xlsx(xlsxFile = file, sheet = i, startRow = 4, detectDates = TRUE)
    # fix Date format already here, although it is checked in app as well
    if ("Date" %in% colnames(out[[i]][["val"]])) {
      if (!inherits(out[[i]][["val"]][, "Date"], "Date")) {
        out[[i]][["val"]][, "Date"] <- ldply_base(out[[i]][["val"]][, "Date"], function(x) {
          as.Date.character(x, tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%Y/%m/%d"))
        })
      }
    }
  }
  if (simplify) {
    out <- ldply_base(out, function(x) {
      cbind(x[["val"]], x[["def"]])
    })
  }
  return(out)
}
