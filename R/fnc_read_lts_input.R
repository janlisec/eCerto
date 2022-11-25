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
#' @importFrom plyr ldply
#'
#' @noRd
#' @keywords internal
read_lts_input <- function(file=NULL, simplify=FALSE) {
  sheets <- openxlsx::getSheetNames(file = file)
  out <- vector("list", length(sheets))
  for (i in 1:length(sheets)) {
    out[[i]][["def"]] <- openxlsx::read.xlsx(xlsxFile = file, sheet = i, startRow = 1, rows = 1:2)
    out[[i]][["val"]] <- openxlsx::read.xlsx(xlsxFile = file, sheet = i, startRow = 4, detectDates=TRUE)
    out[[i]][["val"]] <- cbind(out[[i]][["val"]], "Comment"=NA)
  }
  if (simplify) {
    out <- plyr::ldply(out, function(x) {
      cbind(x[["val"]], x[["def"]])
    })
  }
  return(out)
}