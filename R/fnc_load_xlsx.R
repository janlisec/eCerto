fnc_load_xlsx <- function(filepath, sheet, method=c("tidyxl", "openxlsx")[1], ...) {
  # isolate reactive variables if provided
  if (shiny::is.reactive(filepath)) filepath <- shiny::isolate(filepath)
  if (shiny::is.reactive(sheet)) sheet <- shiny::isolate(sheet)
  
  # make some tests
  if(!file.exists(filepath))  {warning("Invalid file; File-Path does not exist"); return(NULL)}
  if(!any(grep("[Xx][Ll][Ss][Xx]", tools::file_ext(filepath)))) {warning("Invalid file; Please upload a .xlsx file"); return(NULL)}
  if(!sheet %in% 1:length(openxlsx::getSheetNames(filepath))) {warning("Invalid sheet; Sheet number does not exist"); return(NULL)}
  
  # load file with specified method
  a <- switch(method,
      "tidyxl"=tidyxl::xlsx_cells(path = filepath, sheets = sheet, ...),
      "openxlsx"=openxlsx::read.xlsx(xlsxFile = filepath, sheet = sheet, ...)
  )
  
  # post process data
  if (method=="tidyxl") {
    out <- matrix("", nrow=max(a[,"row"]), ncol=max(a[,"col"]), 
                  dimnames=list(1:max(a[,"row"]), LETTERS[1:max(a[,"col"])]))
    for (tp in c("numeric","character")) {
      flt <- which(a[,"data_type"]==tp)
      if (length(flt)>=1) for (i in flt) out[a[i,"row"], a[i,"col"]] <- as.character(a[i,tp])
    }
    out <- as.data.frame(out)
  } else {
    out <- a
  }
  
  return(out)
}
# test function with
# x <- tempfile(fileext = ".xlsx")
# openxlsx::write.xlsx(x=matrix(rnorm(9),ncol=3,dimnames=list(1:3,paste0("Header",1:3))), file=x)
# fnc_load_xlsx(filepath=x, sheet=1)
# fnc_load_xlsx(filepath=x, sheet=1, method="openxlsx")
# fnc_load_xlsx(filepath="C:/not_existent.file", sheet=1)
# fnc_load_xlsx(filepath=x, sheet=2)