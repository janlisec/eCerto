#' @title fnc_prepFigV3.
#' @description \code{prepFigV3} will generate Fig.V3.
#' @details tbd.
#' @param x A data.frame as imported by V module.
#' @examples
#' inp <- system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx")
#' tab <- eCerto:::read_Vdata(file = inp)
#' x <- eCerto:::flt_Vdata(x = tab, l = c(2,4), a = "PFBA")
#' eCerto:::prepFigV3(x = x, cex = 0.8)
#' @return A figure.
#' @keywords internal
#' @noRd
prepFigV3 <- function(x, cex = 1.5) {
  stopifnot(all(c("Analyte", "Level", "Area_Analyte", "Area_IS", "rel_norm") %in% colnames(x)))
  opar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(opar))
  m <- length(unique(x[,"Analyte"]))
  n <- length(unique(x[,"Level"]))
  graphics::layout(mat = matrix(1:(3*m * n), ncol = n, byrow = T))
  graphics::par(cex=cex)
  for (j in 1:m) {
    a_name <- unique(as.character(x[,"Analyte"]))[j]
    a_id <- which(levels(x[,"Analyte"])==a_name)
    flt.a <- as.character(x[,"Analyte"])==a_name
    for (i in 1:n) {
      flt.L <- x[,"Level"]==unique(x[,"Level"])[i]
      graphics::par(mar=c(1, ifelse(i==1, 4, 3), 3, ifelse(i==1, 0, 1))+0.1)
      plot(x[flt.a & flt.L, "Area_Analyte"], ylab=ifelse(i==1, "Area_Analyte", ""), main="", xlab="")
      graphics::mtext(text = paste("Level =", unique(x[,"Level"])[i]), side = 3, line = 0.2, adj = 0, cex = cex)
      graphics::mtext(text = ifelse(i==1, paste0(a_name, " (", a_id, ")"), ""), side = 3, line = 1.4, adj = 0, cex = cex)
    }
    ylim <- range(x[flt.a, "Area_IS"], na.rm=TRUE)
    for (i in 1:n) {
      flt.L <- x[,"Level"]==unique(x[,"Level"])[i]
      graphics::par(mar=c(2, ifelse(i==1, 4, 3), 2, ifelse(i==1, 0, 1))+0.1)
      plot(x[flt.a & flt.L, "Area_IS"], ylab=ifelse(i==1, "Area_IS", ""), xlab="", main="", ylim=ylim)
    }
    graphics::par(mar=c(3,4,0.2,0)+0.1)
    ylim <- range(x[flt.a, "rel_norm"], na.rm=TRUE)
    for (i in 1:n) {
      flt.L <- x[,"Level"]==unique(x[,"Level"])[i]
      graphics::par(mar=c(4, ifelse(i==1, 4, 3), 0, ifelse(i==1, 0, 1))+0.1)
      plot(x[flt.a & flt.L, "rel_norm"], ylab=ifelse(i==1, "rel_norm", ""), xlab="Replicate", main="", ylim=ylim)
      graphics::abline(h=1, lty=2, col=grDevices::grey(0.9))
    }
  }
}
