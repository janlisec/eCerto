#' @title fnc_prepFigV1.
#' @description \code{prepFigV1} will generate Fig.V1.
#' @details tbd.
#' @param ab The ab() object from the validation module..
#' @return A figure.
#' @keywords internal
#' @noRd
prepFigV1 <- function(ab = NULL) {
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  par(mar=c(7,5,5,0)+0.1)
  par(xaxs="i")
  plot(x=c(1,length(ab)), y=range(ab, na.rm=TRUE), xlim=c(0.25,length(ab)+0.75), type="n", axes=F, xlab="", ylab=expression(x[i]/bar(x)~~with~~x==Area[Analyte]/Area[IS]))
  abline(v=1:length(ab), lty=2, col=grey(0.9))
  box(); axis(2)
  boxplot(ab, add=TRUE, axes=FALSE, col = grey((3+as.numeric(attr(ab, "Level")))/(5+max(as.numeric(attr(ab, "Level"))))))
  names(ab)

  # show Analyte-ID and Level-ID
  mtext(text = "Analyte-ID", side = 3, line = 1.5, at = 0, adj = 1)
  a_id <- as.numeric(attr(ab, "Analyte"))
  #a_id[duplicated(a_id)] <- "_"
  #mtext(text = a_id[!duplicated(a_id)], side = 3, line = 1.5, at = (1:length(ab))[!duplicated(a_id)])
  #mtext(text = a_id, side = 3, line = 1.5, at = 1:length(ab))
  mtext(text = a_id[!duplicated(a_id)], side = 3, line = 1.5, at = sapply(split(1:length(ab), a_id), mean))
  mtext(text = "Level", side = 3, line = 0.25, at = 0, adj = 1)
  mtext(text = as.numeric(attr(ab, "Level")), side = 3, line = 0.25, at = 1:length(ab))

  # F test to check for Variance homogeneity
  P_F <- sapply(split(ab, attr(ab, "Analyte"), drop=TRUE), function(x) {
    if (length(x)==2) var.test(x = x[[1]], y = x[[2]], alternative = "two.sided")$p.value else NA
  })
  if (!all(is.na(P_F))) {
    F_p_text <- sapply(P_F, function(x) { ifelse(x<=0.01, "**", ifelse(x<=0.05, "*", "ns")) })
    F_p_col <- sapply(P_F, function(x) { ifelse(x<=0.01, 2, ifelse(x<=0.05, "orange", 3)) })
    mtext(text = expression(P[F-test]), side = 3, line = 2.75, at = 0, adj = 1)
    mtext(text = F_p_text, side = 3, line = 2.75, at = sapply(split(1:length(ab), a_id), mean), col=F_p_col)
  }

  # show n
  mtext(text = expression(n), side = 1, line = 0.25, at = 0, adj = 1)
  mtext(text = sapply(ab, length), side = 1, line = 0.25, at = 1:length(ab))

  # normality test
  KS_p <- sapply(ab, function(x) {
    stats::ks.test(x = x, y = "pnorm", mean = mean(x), sd = stats::sd(x))$p.value
  })
  KS_p_text <- sapply(KS_p, function(x) { ifelse(x<=0.01, "**", ifelse(x<=0.05, "*", "ns")) })
  KS_p_col <- sapply(KS_p, function(x) { ifelse(x<=0.01, 2, ifelse(x<=0.05, "orange", 3)) })
  mtext(text = expression(P[KS]), side = 1, line = 1.5, at = 0, adj = 1)
  mtext(text = KS_p_text, side = 1, line = 1.5, at = 1:length(ab), col=KS_p_col)

  # outlier test Grubbs
  out_Grubbs <- lapply(ab, function(x) {
    cbind(x, Grubbs(lab_means = data.frame("mean"=x)))
  })
  Grubbs_text <- sapply(out_Grubbs, function(x) { ifelse(any(x[,"Grubbs1"]==".01"), "**", ifelse(any(x[,"Grubbs1"]==".05"), "*", "ns")) })
  Grubbs_col <- sapply(out_Grubbs, function(x) { ifelse(any(x[,"Grubbs1"]==".01"), 2, ifelse(any(x[,"Grubbs1"]==".05"), "orange", 3)) })
  mtext(text = expression(P[Grubbs1]), side = 1, line = 2.75, at = 0, adj = 1)
  mtext(text = Grubbs_text, side = 1, line = 2.75, at = 1:length(ab), col=Grubbs_col)
  if (any(Grubbs_text!="ns")) {
    for (i in which(Grubbs_text!="ns")) {
      y <- out_Grubbs[[i]]
      idx <- which(!(y[,"Grubbs1"] %in% c(".", "n.s.")))
      points(x = rep(i, length(idx)), y = y[idx,1], pch=21, bg=2)
    }
  }
  Grubbs_text <- sapply(out_Grubbs, function(x) { ifelse(any(x[,"Grubbs2"]==".01"), "**", ifelse(any(x[,"Grubbs2"]==".05"), "*", "ns")) })
  Grubbs_col <- sapply(out_Grubbs, function(x) { ifelse(any(x[,"Grubbs2"]==".01"), 2, ifelse(any(x[,"Grubbs2"]==".05"), "orange", 3)) })
  mtext(text = expression(P[Grubbs2]), side = 1, line = 4, at = 0, adj = 1)
  mtext(text = Grubbs_text, side = 1, line = 4, at = 1:length(ab), col=Grubbs_col)
  if (any(Grubbs_text!="ns")) {
    for (i in which(Grubbs_text!="ns")) {
      y <- out_Grubbs[[i]]
      idx <- which(!(y[,"Grubbs2"] %in% c(".", "n.s.")))
      points(x = rep(i, length(idx)), y = y[idx,1], pch=21, bg=2)
    }
  }

  # Neumann Trend Test
  out_Neumann <- sapply(ab, function(x) {
    VonNeumannTest(x, unbiased = FALSE)$p.val
  })
  NM_text <- sapply(out_Neumann, function(x) { ifelse(x<=0.01, "**", ifelse(x<=0.05, "*", "ns")) })
  NM_col <- sapply(out_Neumann, function(x) { ifelse(x<=0.01, 2, ifelse(x<=0.05, "orange", 3)) })
  mtext(text = expression(P[Neumann]), side = 1, line = 5.25, at = 0, adj = 1)
  mtext(text = NM_text, side = 1, line = 5.25, at = 1:length(ab), col=NM_col)

  invisible(NULL)

}