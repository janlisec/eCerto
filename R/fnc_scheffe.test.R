#' @title scheffe.test.
#' @description A re-implementation of the scheffe.test as published in the
#'     `agricolae` package <https://rdrr.io/cran/agricolae/man/scheffe.test.html>.
#' @param y Output of linear model function `lm`.
#' @param trt Factor which was used in the call to `lm`.
#' @param alpha alpha level for grouping.
#' @examples
#' test <- eCerto:::test_rv("SR3")$c_fltData()
#' eCerto:::scheffe.test(y = stats::lm(value ~ Lab, data = test), trt = "Lab", alpha = 0.05)$group
#' @noRd
#' @keywords internal
scheffe.test <- function(y, trt, alpha = 0.05) {
  name.y <- paste(deparse(substitute(y)))
  name.t <- paste(deparse(substitute(trt)))
  A <- y$model
  DFerror <- stats::df.residual(y)
  MSerror <- stats::deviance(y) / DFerror
  y <- A[, 1]
  ipch <- pmatch(trt, names(A))
  nipch <- length(ipch)
  for (i in 1:nipch) {
    if (is.na(ipch[i])) {
      return(trt)
    }
  }
  name.t <- names(A)[ipch][1]
  trt <- A[, ipch]
  if (nipch > 1) {
    trt <- A[, ipch[1]]
    for (i in 2:nipch) {
      name.t <- paste(name.t, names(A)[ipch][i], sep = ":")
      trt <- paste(trt, A[, ipch[i]], sep = ":")
    }
  }
  name.y <- names(A)[1]
  df <- subset(data.frame("value" = y, "Lab" = trt), is.na(y) == FALSE)
  # JL
  means <- plyr::ldply(split(df$value, df$Lab), function(x) {
    data.frame(
      "mean" = mean(x, na.rm = T),
      "sd" = stats::sd(x, na.rm = T),
      "n" = sum(is.finite(x)),
      stringsAsFactors = FALSE
    )
  }, .id = "Lab")
  ntr <- nrow(means)
  Fprob <- stats::qf(1 - alpha, ntr - 1, DFerror)
  Tprob <- sqrt(Fprob * (ntr - 1))
  nr <- unique(means[, 4])
  scheffe <- Tprob * sqrt(2 * MSerror / nr)
  statistics <- data.frame(
    "MSerror" = MSerror,
    "Df" = DFerror,
    "F" = Fprob,
    "Mean" = mean(df[, 1]),
    "CV" = sqrt(MSerror) * 100 / mean(df[, 1])
  )
  if (length(nr) == 1) {
    statistics <- data.frame(statistics, "Scheffe" = Tprob, "CriticalDifference" = scheffe)
  }
  comb <- utils::combn(ntr, 2)
  nn <- ncol(comb)
  dif <- rep(0, nn)
  LCL <- dif
  UCL <- dif
  pval <- rep(0, nn)
  for (k in 1:nn) {
    i <- comb[1, k]
    j <- comb[2, k]
    dif[k] <- means[i, 2] - means[j, 2]
    sdtdif <- sqrt(MSerror * (1 / means[i, 4] + 1 / means[j, 4]))
    pval[k] <- round(1 - stats::pf(abs(dif[k])^2 / ((ntr - 1) * sdtdif^2), ntr - 1, DFerror), 4)
    LCL[k] <- dif[k] - Tprob * sdtdif
    UCL[k] <- dif[k] + Tprob * sdtdif
  }
  pmat <- matrix(1, ncol = ntr, nrow = ntr)
  k <- 0
  for (i in 1:(ntr - 1)) {
    for (j in (i + 1):ntr) {
      k <- k + 1
      pmat[i, j] <- pval[k]
      pmat[j, i] <- pval[k]
    }
  }
  groups <- orderPvalue(means, alpha, pmat)
  names(groups)[1] <- name.y
  parameters <- data.frame(test = "Scheffe", name.t = name.t, ntr = ntr, alpha = alpha)
  rownames(parameters) <- " "
  rownames(statistics) <- " "
  rownames(means) <- means[, 1]
  means <- means[, -1]
  output <- list(
    statistics = statistics,
    parameters = parameters,
    means = means,
    comparison = NULL,
    groups = groups
  )
  class(output) <- "group"
  return(output)
}
