#'@title fnc_prepFigH1.
#'@description \code{prepFigH1} will perform statistics on imported homogeneity data.
#'@details tbd.
#'@param x The Hom data from an session R6 object.
#'@param sa Selected analyte (name.type combination within x).
#'@param prec Precision value (for plot annotations).
#'@examples
#'x <- eCerto:::test_homog()$data
#'eCerto:::prepFigH1(x = x, sa = "Fe.radial")
#'@return A data frame.
#'@keywords internal
prepFigH1 <- function(x, sa=NULL, prec=4, xlab="Flasche") {
  message("[prepFigH1] generate boxplot for imported homogeneity data")
  stopifnot(all(c("analyte", "H_type", "Flasche", "value") %in% colnames(x)))
  prec <- try(as.integer(prec[1]))
  if (inherits(prec, "try-error") || length(prec)!=1 || is.na(prec)) prec <- 4L
  h_dat <- x
  an <- ifelse(
    length(unique(h_dat[,"H_type"]))==1,
    as.character(h_dat[interaction(h_dat[,"analyte"], h_dat[,"H_type"])==sa,"analyte"]),
    sa
  )
  h_dat <- h_dat[interaction(h_dat[,"analyte"], h_dat[,"H_type"])==sa,]
  h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
  omn <- round(mean(h_dat[,"value"],na.rm=T), prec)
  osd <- round(stats::sd(h_dat[,"value"],na.rm=T), prec)
  shiny::validate(shiny::need(
    expr = any(is.finite(h_dat[,"value"])),
    message = "Not enough finite values to generate a plot."
  ))
  graphics::par(mar=c(5,4,2.5,0)+0.1)
  graphics::plot(
    x = c(0.6,0.4+length(levels(h_dat[,"Flasche"]))),
    y = range(h_dat[,"value"], na.rm=T),
    type = "n", axes=F,
    xlab = xlab, ylab = paste0(an, " [", unique(h_dat["unit"]), "]")
  )
  graphics::abline(h=omn, lty=2)
  graphics::abline(h=omn+c(-1,1)*osd, lty=2, col=grDevices::grey(0.8))
  graphics::boxplot(h_dat[,"value"] ~ h_dat[,"Flasche"], add=TRUE)
  graphics::mtext(text = paste("Overall mean =", omn), side = 3, line = 1.5, adj = 1)
  graphics::mtext(text = paste("Overall sd =", osd), side = 3, line = 0.25, adj = 1)
  invisible(NULL)
}