#'@title fnc_prepFigH1.
#'@description \code{prepFigH1} will perform statistics on imported homogeneity data.
#'@details tbd.
#'@param x The Hom data from an session R6 object.
#'@param sa Selected analyte (name.type combination within x).
#'@param prec Precision value (for plot annotations).
#'@examples
#'x <- eCerto:::test_homog()$data
#'eCerto:::prepFigH1(x = x, sa = "Fe.radial")
#'eCerto:::prepFigH1(x = x, sa = "Fe.radial", prec=3, xlab="Bottle", showIDs = TRUE)
#'eCerto:::prepFigH1(x = x, sa = NULL)
#'@return A data frame.
#'@keywords internal
prepFigH1 <- function(x, sa=NULL, prec=4, xlab="Flasche", showIDs = FALSE) {
  message("[prepFigH1] generate boxplot for imported homogeneity data")
  stopifnot(all(c("analyte", "H_type", "Flasche", "value") %in% colnames(x)))
  prec <- try(as.integer(prec[1]))
  if (inherits(prec, "try-error") || length(prec)!=1 || is.na(prec)) prec <- 4L
  h_dat <- x
  if (is.null(sa)) {
    # normalize all analytes and modify df internally to work for boxplot
    tmp <- plyr::ldply(split(h_dat, interaction(h_dat[,"analyte"], h_dat[,"H_type"])), function(df) {
      #df[,"value"] <- log2(df[,"value"]/median(df[,"value"], na.rm=TRUE))
      df[,"value"] <- (df[,"value"]-mean(df[,"value"], na.rm=TRUE))/sd(df[,"value"], na.rm=TRUE)
      return(df)
    }, .id=NULL)
    tmp[,"analyte"] <- "dummy"
    tmp[,"H_type"] <- "type"
    sa <- "dummy.type"
    h_dat <- tmp
    ylab <- expression(plain(Normalized~analyte~values)~~(x["a,i"]-bar(x[a]))/sigma[a])
  } else {
    unique_H_type <- length(unique(h_dat[,"H_type"]))==1
    an <- as.character(h_dat[interaction(h_dat[,"analyte"], h_dat[,"H_type"])==sa,"analyte"])
    ylab <- paste(ifelse(unique_H_type, an, sa), " [", unique(h_dat["unit"]), "]")
  }
  h_dat <- h_dat[interaction(h_dat[,"analyte"], h_dat[,"H_type"])==sa,]
  h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
  omn <- round(mean(h_dat[,"value"],na.rm=T), prec)
  osd <- round(stats::sd(h_dat[,"value"],na.rm=T), prec)
  shiny::validate(shiny::need(
    expr = any(is.finite(h_dat[,"value"])),
    message = "Not enough finite values to generate a plot."
  ))
  opar <- graphics::par(no.readonly = TRUE)
  on.exit(par(opar))
  graphics::par(mar=c(5,4,2.5,0)+0.1)
  graphics::plot(
    x = c(0.6,0.4+length(levels(h_dat[,"Flasche"]))),
    y = range(h_dat[,"value"], na.rm=T),
    type = "n", axes=F, xlab = xlab, ylab = ylab
  )
  graphics::abline(h=omn, lty=2)
  graphics::abline(h=omn+c(-1,1)*osd, lty=2, col=grDevices::grey(0.8))
  graphics::boxplot(h_dat[,"value"] ~ h_dat[,"Flasche"], add=TRUE)
  graphics::mtext(text = paste("Overall mean =", omn), side = 3, line = 1.5, adj = 1)
  graphics::mtext(text = paste("Overall sd =", osd), side = 3, line = 0.25, adj = 1)
  if (showIDs) {
    h_dat[,"Rep"] <- NA
    for (l in 1:length(levels(h_dat[,"Flasche"]))) {
      h_dat[h_dat[,"Flasche"]==levels(h_dat[,"Flasche"])[l],"Rep"] <- 1:sum(h_dat[,"Flasche"]==levels(h_dat[,"Flasche"])[l])
    }
    graphics::text(x = as.numeric(h_dat[,"Flasche"]), y = h_dat[,"value"], labels = h_dat[,"Rep"], col=4, cex=2)
  }
  invisible(NULL)
}