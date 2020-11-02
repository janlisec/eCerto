read_excel_input <- function(xlsxFile = x, sheet=input$Excel_Sheet_Name, rows=input$start_row:input$end_row, cols=input$start_col:input$end_col) {
  x <- openxlsx::read.xlsx(xlsxFile = xlsxFile, sheet=sheet, rows=rows, cols=cols)
  #x <- data.frame("analyte"=factor(rep(x[,1],times=ncol(x)-2), levels=x[,1]), "replicate"=factor(rep((3:ncol(x))-2,each=nrow(x))), "value"=unlist(x[,-c(1:2)]), "unit"=x[1,2])
  # remove incomplete or non-numeric data
  flt <- apply(x[,-c(1:2)], 1, function(y) {any(is.finite(as.numeric(y)))})
  if (any(flt)) x <- x[which(flt),,drop=F]
  #combine into data frame and return
  analyte <- x[,1]
  unit <- x[,2]
  dat <- x[,-c(1:2)]
  x <- data.frame("analyte"=factor(rep(analyte,times=ncol(dat)), levels=analyte),
                  "replicate"=factor(rep((1:ncol(dat)),each=nrow(dat))),
                  "value"=as.numeric(unlist(dat)),
                  #"value"=unlist(dat),
                  "unit"=as.character(rep(unit,times=ncol(dat))))
  return(x)
}
