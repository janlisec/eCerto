#' @title scheffe.test.
#' @description A re-implementation of the scheffe.test as published in the `agricolae` package.
#' @param y Output of linear model function `lm`.
#' @param trt Factor which was used in the call to `lm`.
#' @param alpha alpha level for grouping.
#' @examples
#' test <- eCerto:::test_rv("SR3")$c_fltData()
#' eCerto:::scheffe.test(y = stats::lm(value~Lab, data=test), trt="Lab", alpha = 0.05)$group
#' @noRd
#' @keywords internal
scheffe.test <- function(y, trt, alpha = 0.05) {
  # helper functions
  last_char <- function(x)  {
    x <- sub(" +$", "", x)
    return(substr(x, nchar(x), nchar(x)))
  }
  orderPvalue <- function(means, alpha, pmat) {
    letras <- c(letters[1:26], LETTERS[1:26], rep(" ", 2000))
    n <- nrow(means)
    idx <- (1:n)[order(means[, 2], decreasing = TRUE)]
    w <- means[order(means[, 2], decreasing = TRUE), ]
    M <- rep("", n)
    k <- 1
    j <- 1
    i <- 1
    cambio <- n
    cambio1 <- 0
    chequeo <- 0
    M[1] <- letras[k]
    while (j < n) {
      chequeo <- chequeo + 1
      if (chequeo > n) {
        break
      }
      for (i in j:n) {
        s <- pmat[idx[i], idx[j]] > alpha
        if (s) {
          if (last_char(M[i]) != letras[k]) {
            M[i] <- paste(M[i], letras[k], sep = "")
          }
        } else {
          k <- k + 1
          cambio <- i
          cambio1 <- 0
          ja <- j
          M[cambio] <- paste(M[cambio], letras[k], sep = "")
          for (v in ja:cambio) {
            if (pmat[idx[v], idx[cambio]] <= alpha) {
              j <- j + 1
              cambio1 <- 1
            } else {
              break
            }
          }
          break
        }
      }
      if (cambio1 == 0) {
        j <- j + 1
      }
    }
    output <- data.frame("mean" = as.numeric(w[,2]), groups = M, row.names = as.character(w[,1]))
    if (k > 52) {
      cat("\n", k, "groups are estimated. The number of groups exceeded the maximum of 52 labels.\n")
    }
    invisible(output)
  }
  # scheffe test
  name.y <- paste(deparse(substitute(y)))
  name.t <- paste(deparse(substitute(trt)))
  A <- y$model
  DFerror <- stats::df.residual(y)
  MSerror <- stats::deviance(y) / DFerror
  #Fc <- anova(y)[trt, 4]
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
  statistics <- data.frame("MSerror" = MSerror, "Df" = DFerror, "F" = Fprob, "Mean" = mean(df[, 1]), "CV" = sqrt(MSerror) * 100 / mean(df[, 1]))
  if (length(nr) == 1) { statistics <- data.frame(statistics, "Scheffe" = Tprob, "CriticalDifference" = scheffe) }
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
    sdtdif <- sqrt(MSerror * (1 / means[i, 4] + 1 / means[j,4]))
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
  output <- list(statistics = statistics, parameters = parameters, means = means, comparison = NULL, groups = groups)
  class(output) <- "group"
  return(output)
}