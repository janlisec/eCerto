#' @title fnc_prepFigV3.
#' @description \code{prepFigV3} will generate Fig.V3.
#' @details tbd.
#' @param x A list of data frames containing a column 'Value'.
#' @param fix_ylim Logical. Fix the y-axis for all subplots.
#' @return A figure.
#' @keywords internal
#' @noRd
prepFigV3 <- function(x, fix_ylim = FALSE) {
  opar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(opar))
  cex <- 1.5
  graphics::par(mfrow=c(1, length(x)))
  graphics::par(mar=c(5,4,2,0)+0.1)
  graphics::par(cex=1.5)
  if (fix_ylim) ylim <- range(lapply(x, function(y) {y[,"Value"]}), na.rm=TRUE) else ylim <- NULL
  for (i in 1:length(x)) {
    plot(x[[i]][,"Value"], ylab=ifelse(i==1, paste0(as.character(x[[i]][1,"Analyte"]), " (", as.numeric(x[[i]][1,"Analyte"]), ")"), ""), xlab="Replicate", main="", ylim=ylim)
    graphics::mtext(text = paste("Level =", names(x)[i]), side = 3, line = 0.2, adj = 0, cex = cex)
  }
}
