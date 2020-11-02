#'BAMTool
#'Modul: Zertifizierung
#'Lab stats
#'data : data table
Stats <- function(data=NULL, precision=4) {
  lab_means <- plyr::ldply(split(data$value, data$Lab), function(x) {data.frame("mean"=round(mean(x,na.rm=T),precision), "sd"=round(sd(x,na.rm=T),precision), "n"=sum(is.finite(x)), stringsAsFactors = FALSE) }, .id="Lab")
  rownames(lab_means) <- lab_means$Lab
  out <- data.frame(
    lab_means,
    Scheffe(data=data),
    Dixon(lab_means=lab_means),
    Grubbs(lab_means=lab_means),
    Nalimov(lab_means=lab_means),
    Cochran(data=data),
    stringsAsFactors = FALSE)
  return(out[order(out[,"mean"]),])
}
