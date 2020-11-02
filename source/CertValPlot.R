#'BAMTool
#'Modul: Zertifizierung
#'Certified Value Plot
#'data : data table
CertValPlot <- function(data=NULL) {
  data.stats <- plyr::ldply(split(data[,"value"], data[,"Lab"]), function(x) {data.frame("MW"=mean(x,na.rm=T), "Median"=median(x,na.rm=T), "SD"=sd(x,na.rm=T), "n"=sum(is.finite(x))) }, .id="Lab")
  data.stats <- data.frame(data.stats, "Filter"=sapply(split(data[,"L_flt"], data$Lab), all))
  data.stats <- data.stats[order(data.stats$MW),]
  par(mar=c(5,4,0,0)+0.2)
  plot(x=range(1:nrow(data.stats)), axes=F, y=range(c(data.stats$MW+data.stats$SD, data.stats$MW-data.stats$SD)), type="n", main="", ylab=paste0(unique(data[,"analyte"])[1], " [",unique(data[,"unit"])[1],"]"), xlab="Lab", xlim=c(0.5,nrow(data.stats)+0.5))
  axis(1, at=1:nrow(data.stats), labels = data.stats$Lab)
  axis(2, las=2)
  abline(h=mean(data.stats$MW[!data.stats[,"Filter"]]), col=3, lwd=2)
  abline(h=mean(data.stats$MW[!data.stats[,"Filter"]])+c(-1,1)*sd(data.stats$MW[!data.stats[,"Filter"]]), lty=2, col=grey(0.8))
  segments(x0=1:nrow(data.stats), y0=data.stats$MW-data.stats$SD, y1=data.stats$MW+data.stats$SD)
  lw <- 0.15
  segments(x0=1:nrow(data.stats)-lw, x1=1:nrow(data.stats)+lw, y0=data.stats$MW-data.stats$SD)
  segments(x0=1:nrow(data.stats)-lw, x1=1:nrow(data.stats)+lw, y0=data.stats$MW+data.stats$SD)
  symbols(x=1:nrow(data.stats), y=data.stats$MW, circles=rep(lw,nrow(data.stats)), bg=c(3, grey(0.8))[1+data.stats[,"Filter"]], add=T, inches=FALSE)
  legend(x="topleft", bty="n", lty=c(1,2), col = c(3, grey(0.8)), legend = c("mean", "sd"))
  box()
}
