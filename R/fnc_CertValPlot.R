#'@title CertValPlot.
#'
#'@description \code{CertValPlot} will generate a certified values plot.
#'
#'@details not yet
#'
#'@param data data.frame containing columns 'value', 'Lab' and 'Filter'.
#'
#'@return A specific type of boxplot.
#'
#'@export

CertValPlot <- function(data=NULL) {
  data.stats <- plyr::ldply(split(data[,"value"], data[,"Lab"]), function(x) {data.frame("MW"=mean(x,na.rm=T), "Median"= stats::median(x,na.rm=T), "SD"=stats::sd(x,na.rm=T), "n"=sum(is.finite(x))) }, .id="Lab")
  data.stats <- data.frame(data.stats, "Filter"=sapply(split(data[,"L_flt"], data$Lab), all))
  data.stats <- data.stats[order(data.stats$MW),]
  ylab <- paste0(unique(data[,"analyte"])[1], " [",unique(data[,"unit"])[1],"]")
  graphics::par(mar=c(5,6,0,0)+0.2)
  plot(x=range(1:nrow(data.stats)), y=range(c(data.stats$MW+data.stats$SD, data.stats$MW-data.stats$SD)), type="n", ann=FALSE, axes=FALSE, xlim=c(0.5,nrow(data.stats)+0.5))
  graphics::title(xlab="Lab")
  # compute number of lines we need to reserve fo the axis numbers
  lh <- max(graphics::strwidth(graphics::axTicks(2),units = "inch"))/graphics::strheight("0",units = "inch")
  graphics::title(ylab=ylab, line=1.25+0.75*lh)
  graphics::axis(1, at=1:nrow(data.stats), labels = data.stats$Lab)
  graphics::axis(2, las=2)
  graphics::abline(h=mean(data.stats$MW[!data.stats[,"Filter"]]), col=3, lwd=2)
  graphics::abline(h=mean(data.stats$MW[!data.stats[,"Filter"]])+c(-1,1)*stats::sd(data.stats$MW[!data.stats[,"Filter"]]), lty=2, col=grDevices::grey(0.8))
  graphics::segments(x0=1:nrow(data.stats), y0=data.stats$MW-data.stats$SD, y1=data.stats$MW+data.stats$SD)
  lw <- 0.15
  graphics::segments(x0=1:nrow(data.stats)-lw, x1=1:nrow(data.stats)+lw, y0=data.stats$MW-data.stats$SD)
  graphics::segments(x0=1:nrow(data.stats)-lw, x1=1:nrow(data.stats)+lw, y0=data.stats$MW+data.stats$SD)
  graphics::symbols(x=1:nrow(data.stats), y=data.stats$MW, circles=rep(lw,nrow(data.stats)), bg=c(3, grDevices::grey(0.8))[1+data.stats[,"Filter"]], add=T, inches=FALSE)
  graphics::legend(x="topleft", bty="n", lty=c(1,2), col = c(3, grDevices::grey(0.8)), legend = c("mean", "sd"))
  graphics::box()
}