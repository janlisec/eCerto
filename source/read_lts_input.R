#file <- "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/Stabilität_Langzeit/Statistik - P106 LTS/LTS_Testdata_CP001.xlsx"
read_lts_input <- function(file=NULL, simplify=FALSE) {
  sheets <- openxlsx::getSheetNames(file = file)
  out <- vector("list", length(sheets))
  for (i in 1:length(sheets)) {
    out[[i]][["def"]] <- openxlsx::read.xlsx(xlsxFile = file, sheet = i, startRow = 1, rows = 1:2)
    out[[i]][["val"]] <- openxlsx::read.xlsx(xlsxFile = file, sheet = i, startRow = 4, detectDates=TRUE)
  }
  if (simplify) {
    out <- plyr::ldply(out, function(x) {
      cbind(x[["val"]], x[["def"]])
    })
  }
  return(out)
}