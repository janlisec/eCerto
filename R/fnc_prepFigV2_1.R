#' @title fnc_prepFigV2_1.
#' @description \code{prepFigV1} will generate Fig.V2_1.
#' @details plot of raw data per level and lab.
#' @param inp The imported data from the validation2 module..
#' @param q Numeric vector of levels to plot. NULL defaults to all.
#' @return A figure.
#' @examples
#' inp <- eCerto:::init_V2_data()
#' eCerto:::prepFigV2_1(inp = inp)
#' eCerto:::prepFigV2_1(inp = inp, q=1)
#' inp <- cbind(inp, "Unit" = "mg/cm^3^")
#' eCerto:::prepFigV2_1(inp = inp)
#' @keywords internal
#' @noRd
prepFigV2_1 <- function(inp, q = NULL) {
  opar <- graphics::par(no.readonly = TRUE)
  if (is.null(q)) q <- 1:length(unique(inp[,"Level"]))
  n_p <- length(unique(inp[,"Lab"]))
  graphics::par(mfrow=c(1, length(q)))
  graphics::par(mar=c(3.5,1.5,1,0)+0.5)
  for (qi in q) {
    flt <- inp[,"Level"]==unique(inp[,"Level"])[qi]
    y_num <- as.numeric(factor(inp[flt,"Lab"], levels=unique(inp[,"Lab"])))
    plot(x = inp[flt,"Value"], y = y_num, type="n", axes = FALSE, ann = FALSE, ylim = rev(range(y_num)))
    graphics::abline(h = y_num, col=grDevices::grey(0.9))
    graphics::abline(v = mean(inp[flt,"Value"], na.rm=TRUE), lwd=2)
    graphics::axis(1)
    graphics::axis(2, at = 1:n_p, las=1)
    graphics::box()
    graphics::mtext(text = "Lab", side = 3, line = 0.15, at = graphics::par("usr")[1], adj = 1.15)
    graphics::mtext(text = markdown2expression(paste0("**Level ", unique(inp[,"Level"])[qi], "**")), side = 3, line = 0.15, at = graphics::par("usr")[2], adj = 1)
    if ("Unit" %in% colnames(inp)) {
      graphics::mtext(text = markdown2expression(inp[1,"Unit"]), side = 1, line = 2.3, at = stats::median(graphics::par("usr")[1:2]), adj = 0.5)
    }
    for (p in 1:n_p) {
      flt2 <- flt & inp[,"Lab"]==unique(inp[,"Lab"])[p]
      x <- inp[flt2, "Value"]
      y <- rep(p, length(x))
      y[duplicated(x)] <- y[duplicated(x)] + 0.2*c(-1,1,-2,2)[sum(duplicated(x))]
      pchs <- c(21:25)[inp[flt2,"Replicate"]]
      cols <- c(2:6)[inp[flt2,"Replicate"]]
      cols[is.removed(inp[flt2,])] <- grDevices::grey(0.8)
      graphics::points(y = y, x = x, pch = pchs, bg = cols, cex = 2)
    }
  }
  graphics::par(opar)
}
