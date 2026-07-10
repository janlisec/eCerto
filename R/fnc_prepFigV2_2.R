#' @title fnc_prepFigV2_2.
#' @description \code{prepFigV2_2} will generate Fig.V2_2.
#' @details Mandel h or k plot.
#' @param mns The calculated Lab means of the imported data from validation2 module.
#' @param type Either h or k.
#' @param digits Number of digits to round critical values to. Use NA to omit annotation of critical values.
#' @return A figure.
#' @examples
#' inp <- eCerto:::init_V2_data()
#' mns <- eCerto:::V2_calc_stats(inp = inp)
#' eCerto:::prepFigV2_2(mns = mns)
#' eCerto:::prepFigV2_2(mns = mns, "k")
#' eCerto:::prepFigV2_2(mns = mns, "k", digits = NA)
#' mns[mns[,"Lab"]=="Lab05","Mandel_k"] <- NA
#' eCerto:::prepFigV2_2(mns = mns, "k")
#' mns[mns[,"Level"]=="Lev01","Mandel_h"] <- NA
#' eCerto:::prepFigV2_2(mns = mns, "h")
#' @keywords internal
#' @noRd
prepFigV2_2 <- function(mns, type = c("h", "k"), digits = 3) {
  type <- match.arg(type)
  idx <- switch(type, "h" = "Mandel_h", "k" = "Mandel_k")
  all_labs <- unique(mns[,"Lab"])
  all_levels <- unique(mns[,"Level"])
  mns <- mns[is.finite(mns[,idx]),]
  n_p <- length(unique(mns[,"Lab"]))
  if (n_p < length(all_labs)) { ann_lab <- (1:length(all_labs))[all_labs %in% unique(mns[,"Lab"])] } else { ann_lab <- 1:n_p }
  n_q <- length(unique(mns[,"Level"]))
  if (n_p < length(all_labs)) { ann_lev <- (1:length(all_levels))[all_levels %in% unique(mns[,"Level"])] } else { ann_lev <- 1:n_q }
  n_k <- floor(stats::median(mns[,"n"]))
  if (type == "h") {
    m_crit <- qmandel_h(p = n_p, alpha = c(0.01, 0.05))
    ylab <- expression("Mandel's statistic," ~~ italic(h))
    fac <- c(1,1,-1,-1)
  } else {
    m_crit <- qmandel_k(k = n_k, p = n_p, alpha = c(0.01, 0.05))
    ylab <- expression("Mandel's statistic," ~~ italic(k))
    fac <- c(1,1)
  }
  graphics::par(mar=c(1,3.5,1,0)+0.5)
  plot(x = c(0, n_p*n_q*1.2+0.2), y = range(c(0, mns[,idx], fac*m_crit)), type="n", axes=F, ylab=ylab, xlab="", xaxs="i")
  # determine the bar positions
  tmp_x <- as.vector(graphics::barplot(
    formula = mns[,idx] ~ interaction(mns[,"Level"], mns[,"Lab"]), plot = FALSE
  ))
  # draw shadow rectangles behind every second lab
  usr <- graphics::par("usr")
  for (x in seq_len(n_p)) {
    if (x %% 2 == 0) {
      xl  <- stats::median(tmp_x[1:n_q+(x-1)*n_q]) - (1.2*n_q)/2
      xr <- xl + (1.2*n_q)
      graphics::rect(
        xleft = xl, ybottom = usr[3], xright = xr, ytop = usr[4], col = grDevices::grey(0.95), border = NA
      )
    }
  }
  # draw critical value lines
  if (!is.na(digits)) { graphics::abline(h = fac*m_crit, lty = 2, col = grDevices::grey(0.8)) }
  # now draw bars
  graphics::barplot(
    formula = mns[,idx] ~ interaction(mns[,"Level"], mns[,"Lab"]),
    las = 2,
    col = c(grDevices::grey(0.8), grDevices::grey(0.4))[rep(rep(1:2, each = n_q), length.out=n_q*n_p)],
    add = TRUE,
    axisnames = FALSE
  )
  if (!is.na(digits)) {
    #graphics::text(x = 0, y = fac[1:2]*m_crit, labels = c(".01", ".05"), adj = c(-0.15,1.15))
    graphics::text(x = 0, y = m_crit[1], labels = expression(alpha[.01]), adj = c(-0.15,1.15))
    graphics::text(x = 0, y = m_crit[2], labels = expression(alpha[.05]), adj = c(-0.15,1.15))
    graphics::text(x = n_p*n_q*1.2+0.2, y = fac[1:2]*m_crit, labels = round(m_crit, digits = digits), adj = c(1.05,1.15))
  }
  # finally draw axis labels and box
  graphics::mtext(text = "Lab", side = 3, line = 0.15, at = graphics::par("usr")[2], adj = 1)
  graphics::mtext(text = ann_lab, side = 3, line = 0.15, at = sapply(1:n_p, function(x) { stats::median(tmp_x[1:n_q+(x-1)*n_q]) }))
  graphics::mtext(text = "Level", side = 1, line = 0.15, at = graphics::par("usr")[2], adj = 1)
  graphics::mtext(text = c(rep(ann_lev, 2), "...", "j"), side = 1, line = 0.15, at = tmp_x[1:(2*n_q + 2)])
  graphics::box()
}

