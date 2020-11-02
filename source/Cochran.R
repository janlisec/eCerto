#'BAMTool
#'Modul: Zertifizierung
#'Cochran test
#'data : data table
Cochran <- function(data=NULL) {
  vars <- sapply(split(data[,"value"], data[,"Lab"]), var, na.rm=T)
  ns <- sapply(split(data[,"value"], data[,"Lab"]), function(x) { sum(is.finite(x)) })
  out <- data.frame("Cochran"=rep(NA, length(vars)), row.names=names(vars))
  # there might be labs reporting data without variance --> these should be excluded from/before Cochrane
  if (any(vars==0)) {
    flt <- vars>0
    vars <- vars[flt]      
    ns <- ns[flt]
    out[!flt,"Cochran"] <- "excl"
  }
  i <- 1
  while (length(vars)>=3 & i>0) {
    ctest <- outliers::cochran.test(object=vars, data=ns)
    j <- which.max(vars)
    if (is.finite(ctest$p.value) && ctest$p.value<=0.05) {
      out[rownames(out)==names(j),"Cochran"] <- paste0("[",i,"] ", ifelse(ctest$p.value<0.01,".01",".05"))
      vars <- vars[-j]
      ns <- ns[-j]
      i <- i+1
    } else {
      out[is.na(out[,"Cochran"]),"Cochran"] <- "."
      i <- 0
    }
  }
  return(out)
}