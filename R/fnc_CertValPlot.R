#'@title CertValPlot.
#'
#'@description \code{CertValPlot} will generate a certified values plot.
#'
#'@details Individual samples have been stripped from the data frame upfront.
#'
#'@param data data.frame containing columns 'value', 'Lab' and 'L_flt'.
#'@param annotate_id T/F to overlay the plot with ID as text if column 'ID' is present.
#'@param filename_labels T/F to use imported file names as labels on x-axes.
#'
#'@return A specific type of boxplot.
#'
#'@examples
#'data <- data.frame("ID"=1:20, "value"=rnorm(20), "analyte"="X", "Lab"=gl(2,10), "L_flt"=FALSE)
#'CertValPlot(data=data)
#'CertValPlot(data=data, annotate_id=TRUE)
#'
#'@export

CertValPlot <- function(data=NULL, annotate_id=FALSE, filename_labels=FALSE) {
  data.stats <- plyr::ldply(split(data[,"value"], data[,"Lab"]), function(x) {data.frame("MW"=mean(x,na.rm=T), "Median"= stats::median(x,na.rm=T), "SD"=stats::sd(x,na.rm=T), "n"=sum(is.finite(x))) }, .id="Lab")
  data.stats <- data.frame(data.stats, "Filter"=sapply(split(data[,"L_flt"], data$Lab), all))
  data.stats <- data.stats[order(data.stats$MW),]
  xlabs <- as.character(data.stats$Lab)
  xlab <- "Lab"
  mar <- c(5,6,0,0)+0.2
  if (filename_labels & "File" %in% colnames(data.stats)) {
    xlabs <- sapply(xlabs, function(x) { unique(sub(pattern = "(.*?)\\..*$", replacement = "\\1", basename(data[data[,"Lab"]==x,"File"]))) })
    mar <- c(3+floor(max(nchar(xlabs))*0.6),6,0,0)+0.2
    xlab <- NULL
  }
  #browser()
  if (all(c("analyte","unit") %in% colnames(data))) {
    ylab <- paste0(unique(data[,"analyte"])[1], " [",unique(data[,"unit"])[1],"]")
  } else {
    ylab=""
  }
  opar <- graphics::par(no.readonly = TRUE)
  graphics::par(mar=mar)
  on.exit(graphics::par(mar=opar$mar))
  # y range is minimized dependent if Sample-IDs are shown or not
  y_range <- range(c(data.stats$MW+data.stats$SD, data.stats$MW-data.stats$SD), na.rm=TRUE)
  if (annotate_id) y_range <- range(c(y_range, data[,"value"]), na.rm=TRUE)
  plot(x=range(1:nrow(data.stats)), y=y_range, type="n", ann=FALSE, axes=FALSE, xlim=c(0.5,nrow(data.stats)+0.5))
  graphics::title(xlab=xlab)
  # compute number of lines we need to reserve fo the axis numbers
  lh <- max(graphics::strwidth(graphics::axTicks(2),units = "inch"))/graphics::strheight("0",units = "inch")
  graphics::title(ylab=ylab, line=1.25+0.75*lh)
  graphics::axis(1, at=1:nrow(data.stats), labels = xlabs, las=ifelse(filename_labels, 2, 1))
  graphics::axis(2, las=2)
  graphics::abline(h=mean(data.stats$MW[!data.stats[,"Filter"]]), col=3, lwd=2)
  graphics::abline(h=mean(data.stats$MW[!data.stats[,"Filter"]])+c(-1,1)*stats::sd(data.stats$MW[!data.stats[,"Filter"]]), lty=2, col=grDevices::grey(0.8))
  graphics::segments(x0=1:nrow(data.stats), y0=data.stats$MW-data.stats$SD, y1=data.stats$MW+data.stats$SD)
  lw <- 0.15
  graphics::segments(x0=1:nrow(data.stats)-lw, x1=1:nrow(data.stats)+lw, y0=data.stats$MW-data.stats$SD)
  graphics::segments(x0=1:nrow(data.stats)-lw, x1=1:nrow(data.stats)+lw, y0=data.stats$MW+data.stats$SD)
  graphics::symbols(x=1:nrow(data.stats), y=data.stats$MW, circles=rep(lw,nrow(data.stats)), bg=c(3, grDevices::grey(0.8))[1+data.stats[,"Filter"]], add=T, inches=FALSE)
  graphics::legend(x="topleft", bty="n", lty=c(1,2), col = c(3, grDevices::grey(0.8)), legend = c("mean", "sd"))
  if (annotate_id & "ID" %in% colnames(data)) {
    tmp_x <- sapply(data[,"Lab"], function(x) { which(data.stats[,"Lab"]==x) })
    graphics::text(x = jitter(tmp_x, amount = 0.25), y = data[,"value"], label=data[,"ID"], col=4)
  }
  graphics::box()
}