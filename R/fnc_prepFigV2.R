#' @title fnc_prepFigV2.
#' @description \code{prepFigV2} will generate Fig.V2.
#' @details tbd.
#' @param ab The ab() object from the validation module..
#' @return A figure.
#' @keywords internal
#' @noRd
prepFigV2 <- function(tab = NULL, a = NULL, alpha = 0.05, k = 3, flt_outliers = flt_outliers) {
  opar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(opar))
  cex <- 1.5
  vals <- prepTabV1(tab = tab, a = a, alpha = alpha, k = k, flt_outliers = flt_outliers)
  df <- attr(vals, "df")
  e <- attr(vals, "residuals")
  l <- NULL
  if (flt_outliers) { l <- levels(tab[,"Level"])[as.numeric(rownames(df))] }
  tmp <- prepDataV1(tab = tab, a = a, l = l, fmt = "norm")
  conc <- rep(attr(tmp, "Concentration"), times = sapply(tmp, length))
  # compute quadratic model
  dfq <- cbind(df, "Conc2" = df$Conc^2)
  qm <- stats::lm(Area_norm ~ Conc + Conc2, data=dfq)
  e.qm <- stats::residuals(qm)
  res_rng <- range(c(e, e.qm))
  graphics::layout(mat = matrix(1:6, ncol=2), heights = c(0.45,0.2,0.35))
  graphics::par(cex=cex)
  # linear model
  graphics::par(mar=c(5,4,3,2)+0.1)
  plot(x = conc, y = unlist(tmp), xlab = "Concentration", ylab = "Area/Area_IS", main = a)
  graphics::abline(b = vals[,"b1"], a = vals[,"b0"], col = 3)
  graphics::points(df, pch=4, col=2, cex=2)
  graphics::mtext(text = expression(y==b[0]+b[1]*x), side = 3, line = -1.3, at = 0, adj = 0, cex = cex)
  graphics::mtext(text = bquote(b[0]==.(round(vals[,"b0"], 4))), side = 3, line = -2.5, at = 0, adj = 0, cex = cex)
  graphics::mtext(text = bquote(b[1]==.(round(vals[,"b1"], 4))), side = 3, line = -3.7, at = 0, adj = 0, cex = cex)
  graphics::mtext(text = paste("r =", round(stats::cor((stats::lm(Area_norm ~ Conc, data=df))$fitted.values, df$Area_norm), 4)), side = 3, line = -4.9, at = 0, adj = 0, cex = cex)
  graphics::par(mar=c(3,4,0,2)+0.1)
  tmp.x <- graphics::barplot(unname(e), ylim = res_rng, ann=F)
  flt <- e>=0
  graphics::text(x = tmp.x[flt], y = 0, labels = names(e)[flt], srt=90, adj=1.2, cex = 2/3)
  graphics::text(x = tmp.x[!flt], y = 0, labels = names(e)[!flt], srt=90, adj=-0.2, cex = 2/3)
  graphics::mtext(text = "Residuals", side = 2, line = 3, cex = cex)
  i <- 1+max(which(df[,1]<=vals[,"LOQ"]))
  graphics::par(mar=c(5,4,0,2)+0.1)
  plot(x = conc, y = unlist(tmp), xlab = "Concentration", ylab = "Area/Area_IS", xlim = c(0, df[i,1]), ylim = c(0, df[i,2]))
  graphics::abline(b = vals[,"b1"], a = vals[,"b0"], col = 3)
  graphics::points(df, pch=4, col=2, cex=2)

  # quadratic model
  graphics::par(mar=c(5,4,3,2)+0.1)
  plot(x = conc, y = unlist(tmp), xlab = "Concentration", ylab = "Area/Area_IS", main = a)
  new_conc <- seq(min(c(0, min(conc))), max(conc), length.out=100)
  area_predicted <- stats::predict(object = qm, list(Conc = new_conc, Conc2 = new_conc^2))
  graphics::lines(x = new_conc, y = area_predicted, col = 3)
  graphics::points(df, pch=4, col=2, cex=2)
  graphics::mtext(text = expression(y==b[0]+b[1]*x+b[2]*x^2), side = 3, line = -1.3, at = 0, adj = 0, cex = cex)
  graphics::mtext(text = bquote(b[0]==.(round(stats::coef(qm)[1], 4))), side = 3, line = -2.5, at = 0, adj = 0, cex = cex)
  graphics::mtext(text = bquote(b[1]==.(round(stats::coef(qm)[2], 4))), side = 3, line = -3.7, at = 0, adj = 0, cex = cex)
  graphics::mtext(text = bquote(b[2]==.(round(stats::coef(qm)[3], 4))), side = 3, line = -4.9, at = 0, adj = 0, cex = cex)
  graphics::mtext(text = paste("r =", round(stats::cor(qm$fitted.values, dfq$Area_norm), 4)), side = 3, line = -6.1, at = 0, adj = 0, cex = cex)
  graphics::par(mar=c(3,4,0,2)+0.1)
  tmp.x <- graphics::barplot(unname(e.qm), ylim = res_rng, ann=F)
  flt <- e.qm>=0
  graphics::text(x = tmp.x[flt], y = 0, labels = names(e.qm)[flt], srt=90, adj=1.1, cex = 2/3)
  graphics::text(x = tmp.x[!flt], y = 0, labels = names(e.qm)[!flt], srt=90, adj=-0.1, cex = 2/3)
  graphics::mtext(text = "Residuals", side = 2, line = 3, cex = cex)
  i <- 1+max(which(df[,1]<=vals[,"LOQ"]))
  graphics::par(mar=c(5,4,0,2)+0.1)
  plot(x = conc, y = unlist(tmp), xlab = "Concentration", ylab = "Area/Area_IS", xlim = c(0, df[i,1]), ylim = c(0, df[i,2]))
  graphics::lines(x = new_conc, y = area_predicted, col = 3)
  graphics::points(df, pch=4, col=2, cex=2)
  invisible(NULL)
}
