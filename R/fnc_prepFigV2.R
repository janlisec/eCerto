#' @title fnc_prepFigV2.
#' @description \code{prepFigV2} will generate Fig.V2 (linearity details).
#' @details tbd.
#' @param tab The imported V data.
#' @param a Analyte name.
#' @param alpha Probability of error.
#' @param k result uncertainty.
#' @param flt_outliers Logical. Shall outliers, determined via an F-test testing
#'     the highest residual be removed from the analysis.
#' @param cex Character expansion of Figure. In an app 1.5 is a nice scaling to
#'     get a detailed figure comparable to the other text.
#' @examples
#' inp <- system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx")
#' tab <- eCerto:::read_Vdata(file = inp)
#' eCerto:::prepFigV2(tab = tab, a = "PFOA", alpha = 0.01, cex = 1)
#' @return A figure.
#' @keywords internal
#' @noRd
prepFigV2 <- function(tab = NULL, a = NULL, alpha = 0.05, k = 3, flt_outliers = FALSE, cex = 1.5) {
  opar <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(opar))
  vals <- prepTabV1(tab = tab, a = a, alpha = alpha, k = k, flt_outliers = flt_outliers)
  dfl <- attr(vals, "df")
  e.l <- attr(vals, "residuals")
  l <- levels(tab[,"Level"])
  if (flt_outliers) { l <- levels(tab[,"Level"])[levels(tab[,"Level"]) %in% rownames(dfl)] }

  flt <- tab[,"Analyte"]==a & tab[,"Level"] %in% l
  conc <- tab[flt,"Concentration"]
  Area_norm <- tab[flt,"norm"]

  # compute quadratic model
  dfq <- cbind(dfl, "Conc2" = dfl$Conc^2)
  qm <- stats::lm(Area_norm ~ Conc + Conc2, data=dfq)
  e.qm <- stats::residuals(qm)
  res_rng <- range(c(e.l, e.qm))
  graphics::layout(mat = matrix(1:6, ncol=2), heights = c(0.45,0.2,0.35))
  graphics::par(cex=cex)
  for (m in 1:2) {
    # linear model: m = 1
    # quadratic model: m = 2

    # fitted data plot
    graphics::par(mar=c(5,ifelse(m==1, 4, 3),3,ifelse(m==1, 0, 1))+0.1)
    plot(x = conc, y = Area_norm, xlab = "Concentration", ylab = ifelse(m==1, "Area/Area_IS", ""), main = ifelse(m==1, a, ""))
    if (m==1) {
      df <- dfl
      e <- e.l
      graphics::abline(b = vals[,"b1"], a = vals[,"b0"], col = 3)
      plot_ann <- list(
        expression(y==b[0]+b[1]*x),
        bquote(b[0]==.(round(vals[,"b0"], 4))),
        bquote(b[1]==.(round(vals[,"b1"], 4))),
        paste("r =", round(stats::cor((stats::lm(Area_norm ~ Conc, data=df))$fitted.values, df$Area_norm), 4))
      )
      for (i in 1:length(plot_ann)) graphics::mtext(text = plot_ann[[i]], side = 3, line = -0.2-1.2*i, at = graphics::par("usr")[1], adj = -0.04, cex = cex)
    } else {
      df <- dfq
      e <- e.qm
      new_conc <- seq(min(c(0, min(conc))), max(conc), length.out=100)
      area_predicted <- stats::predict(object = qm, list(Conc = new_conc, Conc2 = new_conc^2))
      graphics::lines(x = new_conc, y = area_predicted, col = 3)
      graphics::mtext(text = "(quadratic model)", side = 3, line = 1, cex = cex)
      plot_ann <- list(
        expression(y==b[0]+b[1]*x+b[2]*x^2),
        bquote(b[0]==.(round(stats::coef(qm)[1], 4))),
        bquote(b[1]==.(round(stats::coef(qm)[2], 4))),
        bquote(b[2]==.(round(stats::coef(qm)[3], 4))),
        paste("r =", round(stats::cor(qm$fitted.values, dfq$Area_norm), 4))
      )
      for (i in 1:length(plot_ann)) graphics::mtext(text = plot_ann[[i]], side = 3, line = -0.2-1.2*i, at = graphics::par("usr")[1], adj = -0.04, cex = cex)
    }
    graphics::points(df, pch=4, col=2, cex=2)

    # barplot of residuals
    graphics::par(mar=c(3,ifelse(m==1, 4, 3),0,ifelse(m==1, 0, 1))+0.1)
    barplot.x <- graphics::barplot(unname(e), ylim = res_rng, ann=F)
    flt <- e>=0
    graphics::text(x = barplot.x[flt], y = 0, labels = paste(names(e)[flt], " "), srt=90, adj = 1, cex = 1/cex)
    graphics::text(x = barplot.x[!flt], y = 0, labels = paste(" ", names(e)[!flt]), srt=90, adj = 0, cex = 1/cex)
    graphics::mtext(text = ifelse(m==1, "Residuals", ""), side = 2, line = 3, cex = cex)

    # zoomed plot
    graphics::par(mar=c(5,ifelse(m==1, 4, 3),0,ifelse(m==1, 0, 1))+0.1)
    if (all(df[,1] > vals[,"LOQ"])) {
      plot(1,1,ann=FALSE,axes=F,type="n")
      graphics::text(x = 1, y = 1, labels = expression(LOQ<c[min]))
    } else {
      i <- 1+max(which(df[,1]<=vals[,"LOQ"]))
      # xlim <- c(0, df[i,1])
      # ylim <- c(0, df[i,2])
      xlim <- c(0, vals[,"LOQ"])
      y_max <- stats::predict(object = stats::lm(Area_norm ~ Conc, data=dfl), list(Conc = c(vals[,"LOD"], vals[,"LOQ"])))
      ylim <- c(0, max(y_max))
      plot(x = conc, y = Area_norm, xlab = "Concentration", ylab = ifelse(m==1, "Area/Area_IS (LOQ range)", ""), xlim = xlim, ylim = ylim)
      if (m==1) {
        graphics::abline(b = vals[,"b1"], a = vals[,"b0"], col = 3)
        graphics::segments(x0 = vals[,"LOD"], y0 = 0, y1 = y_max[1], col = 4, lty = 2)
        graphics::mtext(text = "LOD", at = vals[,"LOD"], side = 3, line = 0.1, cex = 2/3*cex, col = 4)
        graphics::segments(x0 = vals[,"LOQ"], y0 = 0, y1 = y_max[2], col = 4, lty = 2)
        graphics::mtext(text = "LOQ", at = vals[,"LOQ"], side = 3, line = 0.1, cex = 2/3*cex, col = 4)
      } else {
        graphics::lines(x = new_conc, y = area_predicted, col = 3)
      }
      graphics::points(df, pch=4, col=2, cex=2)
    }
  }
  invisible(NULL)
}
