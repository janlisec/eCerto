#'BAMTool
#'Modul: Zertifizierung
#'Test Plot
#'x : data table
TestPlot <- function(data=NULL) {
  data[,"Lab"] <- factor(data[,"Lab"], levels = names(sort(sapply(split(data[,"value"], data[,"Lab"]),mean))))
  plot(value~Lab, data=data, main="", ylab=paste0(unique(data[,"analyte"])[1], " [",unique(data[,"unit"])[1],"]"))
  text(x=jitter(as.numeric(data[,"Lab"])), y=data[,"value"], labels = data[,"ID"], col=4)
}
