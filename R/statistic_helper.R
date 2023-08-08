#' @description BAMTool, Modul: Certification, Scheffe's multiple t-test
#' @param data Table with columns 'Lab' and 'value'.
#' @examples
#' test <- eCerto:::test_rv("SR3")$c_fltData()
#' Scheffe(data = test)
#' @noRd
Scheffe <- function(data=NULL) {
  S05 <- try(scheffe.test(y = stats::lm(value~Lab, data=data), trt="Lab", alpha = 0.05)$group[levels(data$Lab),"groups"], silent=TRUE)
  if (inherits(S05, "try-error")) S05 <- rep("Error", length(levels(data$Lab)))
  S01 <- try(scheffe.test(y = stats::lm(value~Lab, data=data), trt="Lab", alpha = 0.01)$group[levels(data$Lab),"groups"], silent=TRUE)
  if (inherits(S01, "try-error")) S01 <- rep("Error", length(levels(data$Lab)))
  return(data.frame(
    "Scheffe_05"=S05,
    "Scheffe_01"=S01,
    row.names=levels(data$Lab))
  )
}

#' @description BAMTool, Modul: Certification, Dixon Test
#' @param lab_means data.frame, output of Stats function.
#' @param fmt Output format. Either the p-values directly or expressed qualitatively.
#' @noRd
#' @examples
#' test <- shiny::isolate(eCerto:::test_rv("SR3")$c_lab_means())
#' plyr::ldply(c("alpha", "pval", "cval", "cval05", "cval01"), function(x) {
#'   t(Dixon(lab_means=test, fmt=x))
#' })
Dixon <- function(lab_means=NULL, fmt=c("alpha", "pval", "cval", "cval05", "cval01")) {
  fmt <- match.arg(fmt)
  x <- lab_means[,"mean"]
  n <- length(x)
  out <- rep(NA, n)
  if (n>=3 && n<=100 && diff(range(x))>0) {
    smallest_is_extreme <- (max(x) - mean(x)) <= (mean(x) - min(x))
    # calculate outliers at both ends
    # d_upper <- outliers::dixon.test(x=x, type = 0, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,TRUE,FALSE))
    # d_lower <- outliers::dixon.test(x=x, type = 0, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,FALSE,TRUE))
    d_upper <- dixon.test(x=x, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,TRUE,FALSE))
    d_lower <- dixon.test(x=x, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,FALSE,TRUE))
    out[which.max(x)] <- d_upper$p.value
    out[which.min(x)] <- d_lower$p.value
    # reformat p-values
    if (fmt=="alpha") out <- pval2level(p = out)
    if (fmt=="cval") {
      out[which.max(x)] <- d_upper$statistic
      out[which.min(x)] <- d_lower$statistic
    }
    if (fmt=="cval05") {
      out[!is.na(out)] <- qdixon(p = 0.05, n = n)
    }
    if (fmt=="cval01") {
      out[!is.na(out)] <- qdixon(p = 0.01, n = n)
    }
  } else {
    err <- ifelse(n<3, "n<3", ifelse(n>100, "n>30", ifelse(diff(range(x))<=0, "var(x)=0", "Error")))
    out <- rep(err, n)
  }
  return(data.frame("Dixon"=out, row.names=row.names(lab_means)))
}

#' @description BAMTool, Modul: Certification, Grubbs Test
#' @param lab_means data.frame, output of Stats function.
#' @param fmt Output format. Either the p-values directly or expressed qualitatively.
#' @noRd
#' @examples
#' test <- shiny::isolate(eCerto:::test_rv("SR3")$c_lab_means())
#' Grubbs(lab_means=test, fmt=c("alpha", "pval", "cval")[3])
#' Grubbs(lab_means=test, fmt="cval05")
#' Grubbs(lab_means=test, fmt="cval05")
Grubbs <- function(lab_means = NULL, fmt=c("alpha", "pval", "cval", "cval05", "cval01")) {
  fmt <- match.arg(fmt)
  x <- lab_means[, "mean"]
  n <- length(x)
  out <- data.frame("Grubbs1" = rep(NA, n), row.names = row.names(lab_means))
  if (n>=3 && n<=100 && diff(range(x))>0) {
    #smallest_is_extreme <- (max(x) - mean(x)) <= (mean(x) - min(x))
    # out$Grubbs1[which.max(x)] <- outliers::grubbs.test(x = x, type = 10, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, TRUE, FALSE))$p.value
    # out$Grubbs1[which.min(x)] <- outliers::grubbs.test(x = x, type = 10, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, FALSE, TRUE))$p.value
    out$Grubbs1[which.min(x)] <- grubbs.test(x = x, type = "10", tail = "lower")$p.value
    out$Grubbs1[which.max(x)] <- grubbs.test(x = x, type = "10", tail = "upper")$p.value
    test_Grubbs1_min <- out$Grubbs1[which.min(x)]>0.05
    test_Grubbs1_max <- out$Grubbs1[which.max(x)]>0.05
    if (fmt=="alpha") out$Grubbs1 <- pval2level(p = out$Grubbs1)
    if (fmt=="cval") { out$Grubbs1[!is.na(out$Grubbs1)] <- abs(lab_means[!is.na(out$Grubbs1),"mean"]-mean(x))/sd(x) }
    if (fmt=="cval05") { out$Grubbs1[!is.na(out$Grubbs1)] <- qgrubbs(1-0.05/2, n) }
    if (fmt=="cval01") { out$Grubbs1[!is.na(out$Grubbs1)] <- qgrubbs(1-0.01/2, n) }
    if (n >= 4 && (test_Grubbs1_min | test_Grubbs1_max) && n<=100) {
      out$Grubbs2 <- rep(NA, n)
      maxvals <- order(x, decreasing = TRUE)[1:2]
      minvals <- order(x, decreasing = FALSE)[1:2]
      # if (test_Grubbs1_min) out$Grubbs2[minvals] <- outliers::grubbs.test(x = x, type = 20, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, FALSE, TRUE))$p.value
      # if (test_Grubbs1_max) out$Grubbs2[maxvals] <- outliers::grubbs.test(x = x, type = 20, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, TRUE, FALSE))$p.value
      if (test_Grubbs1_min) out$Grubbs2[minvals] <- grubbs.test(x = x, type = "20", tail = "lower")$p.value
      if (test_Grubbs1_max) out$Grubbs2[maxvals] <- grubbs.test(x = x, type = "20", tail = "upper")$p.value
      if (fmt=="alpha") out$Grubbs2 <- pval2level(p = out$Grubbs2)
      if (fmt=="cval") {
        if (test_Grubbs1_min) out$Grubbs2[minvals] <- stats::var(x[-minvals])/stats::var(x) * (n - 3)/(n - 1)
        if (test_Grubbs1_max) out$Grubbs2[maxvals] <- stats::var(x[-maxvals])/stats::var(x) * (n - 3)/(n - 1)
      }
      if (fmt=="cval05") {
        if (test_Grubbs1_min) out$Grubbs2[minvals] <- qgrubbs(0.05, n, type = "20")
        if (test_Grubbs1_max) out$Grubbs2[maxvals] <- qgrubbs(0.05, n, type = "20")
      }
      if (fmt=="cval01") {
        if (test_Grubbs1_min) out$Grubbs2[minvals] <- qgrubbs(0.01, n, type = "20")
        if (test_Grubbs1_max) out$Grubbs2[maxvals] <- qgrubbs(0.01, n, type = "20")
      }
    }
  } else {
    if (fmt=="alpha") out$Grubbs1 <- rep("Error", length(x))
  }
  return(out)
}

#' @description BAMTool, Modul: Certification, Cochran Test
#' @param data Table with columns 'Lab' and 'value'.
#' @param fmt Output format. Either the p-values directly or expressed qualitatively.
#' @noRd
#' @examples
#' test <- eCerto:::test_rv("SR3")$c_fltData()
#' Cochran(data=test, fmt=c("alpha", "pval")[1])
#' cbind(Cochran(data=test, fmt="cval"), Cochran(data=test, fmt="cval05"))
Cochran <- function(data=NULL, fmt=c("alpha", "pval", "cval", "cval05", "cval01")) {
  fmt <- match.arg(fmt)
  vars <- sapply(split(data[,"value"], data[,"Lab"]), stats::var, na.rm=T)
  ns <- sapply(split(data[,"value"], data[,"Lab"]), function(x) { sum(is.finite(x)) })
  out <- data.frame("Cochran"=rep(NA, length(vars)), row.names=names(vars))
  # there might be labs reporting data without variance --> these should be excluded from/before performing Cochrane test
  if (any(vars==0)) {
    flt <- vars>0
    vars <- vars[flt]
    ns <- ns[flt]
    out[!flt,"Cochran"] <- switch(
      fmt,
      "alpha" = "excl",
      "pval" = NA,
      "cval" = "excl",
      "cval05" = NA,
      "cval01" = NA
    )
  }
  i <- 1
  while (length(vars)>=3 & i>0) {
    #ctest <- outliers::cochran.test(object=vars, data=ns)
    ctest <- cochran.test(vars = vars, ns = ns)
    j <- which.max(vars)
    if (is.finite(ctest$p.value) && ctest$p.value<=0.05) {
      out[rownames(out)==names(j),"Cochran"] <- switch(
        fmt,
        "alpha" = paste0("[",i,"] ", pval2level(ctest$p.value)),
        "pval" = ctest$p.value,
        "cval" = ctest$statistic,
        "cval05" = qcochran(p = 0.05, n = ns[names(j)], k = length(vars)),
        "cval01" = qcochran(p = 0.01, n = ns[names(j)], k = length(vars))
      )
      vars <- vars[-j]
      ns <- ns[-j]
      i <- i+1
    } else {
      # no more testing necessary
      #if (fmt=="alpha") out[is.na(out[,"Cochran"]),"Cochran"] <- "."
      out[rownames(out)==names(j),"Cochran"] <- switch(
        fmt,
        "alpha" = ".",
        "pval" = ctest$p.value,
        "cval" = ctest$statistic,
        "cval05" = qcochran(p = 0.05, n = ns[names(j)], k = length(vars)),
        "cval01" = qcochran(p = 0.01, n = ns[names(j)], k = length(vars))
      )
      i <- 0
    }
  }
  return(out)
}

#' @title qgrubbs.
#' @description To calculate the critical Grubbs value z_alpha.
#'     ToDo [JL] This should be re-implemented to really get q for a specific p,
#'     while currently we have to modify p as shown in examples.
#' @param p The desired p-value level, i.e. 0.01 or 0.05 for a one sided test.
#' @param n The number of values or labs.
#' @param type Use type=10 for single Grubbs and 20 for double Grubbs.
#' @param rev Use rev=TRUE to obtain the P-value instead of z_alpha.
#' @noRd
#' @keywords internal
#' @examples
#' qgrubbs(0.05, 5)
#' qgrubbs(p = (1-0.05/2), n = 5)
#' qgrubbs_test <- function(p, n) {
#'   t2 <- stats::qt(p = p/(2*n), df = n-2)^2
#'   return((n-1)/sqrt(n) * sqrt(t2 / (n-2 + t2)))
#' }
#' qgrubbs_test(0.05, 5)
qgrubbs <- function (p, n, type = 10, rev = FALSE) {
  if (type == 10) {
    if (!rev) {
      t2 <- stats::qt((1 - p)/n, n - 2)^2
      return(((n - 1)/sqrt(n)) * sqrt(t2/(n - 2 + t2)))
    } else {
      s <- (p^2 * n * (2 - n))/(p^2 * n - (n - 1)^2)
      t <- sqrt(s)
      if (is.nan(t)) {
        res <- 0
      } else {
        res <- n * (1 - stats::pt(t, n - 2))
        res[res > 1] <- 1
      }
      return(1 - res)
    }
  } else {
    if (n > 30) warning("[qgrubbs] critical value is estimated for n>30")
    gtwo <- eCerto::cvals_Grubbs2
    pp <- as.numeric(colnames(gtwo))
    if (!rev) res <- qtable(p, pp, gtwo[n-3,]) else res <- qtable(p, gtwo[n-3,], pp)
    res[res < 0] <- 0
    res[res > 1] <- 1
    return(unname(res))
  }
}

#' @title qgrubbs2.
#' @description To calculate a critical Grubbs value for the double Grubbs test.
#' @param p The desired p-value level, i.e. 0.01 or 0.05 for a one sided test.
#' @param n The number of values or labs.
#' @noRd
#' @keywords internal
#' @examples
#' qgrubbs(0.05, 5, type = 20)
#' qgrubbs2(0.05, 5)
#' par(mfrow=c(1,4))
#' for (p in c(0.01, 0.025, 0.05, 0.1)) {
#'   x <- sapply(4:100, function(n) { qgrubbs2(p = p, n = n) })
#'   plot(
#'     x - eCerto::cvals_Grubbs2[,as.character(p)],
#'     main=paste("alpha =", p),
#'     ylab="Deviation from Tabulated values for Double Grubbs",
#'     ylim=c(-0.003,0.003), xlab="n"
#'   )
#'   abline(h=seq(-0.003, 0.003, 0.001), col=grey(0.9))
#' }
qgrubbs2 <- function(p = 0.05, n = 5) {
  tmp <- structure(
    c(0.001, 0.005, 0.01, 0.025, 0.05, 0.1,
      0.0443, 0.0388, 0.0362, 0.0322, 0.0289, 0.0251,
      1.0012, 0.9558, 0.925, 0.8833, 0.8501, 0.8169,
      -4.2493, -3.6613, -3.3101, -2.858, -2.5075, -2.1615
    ), dim = c(6L, 4L), dimnames = list(NULL, c("p", "a", "b", "c"))
  )
  idx <- which(sapply(tmp[,"p"], identical, p))
  if (length(idx)==0) {
    warning("[qgrubbs2] no coefficients for this p available")
    idx <- 1
  }
  fn <- tmp[idx,"a"]*n^2 + tmp[idx,"b"]*n + tmp[idx,"c"]
  fc <- (1-p)^(1/fn)
  out <- 1 / (1 + 2/(n-3) * stats::qf(p = fc, df1 = 2, df2 = n-3))
  return(unname(out))
}

#' @title pgrubbs.
#' @description To calculate the Grubbs P value.
#' @param q The obtained z_alpha-value or Grubbs statistic.
#' @param n The number of values or labs.
#' @param type type.
#' @noRd
#' @keywords internal
#' @examples
#' pgrubbs(q = 0.91233, n = 5)
pgrubbs <- function (q, n, type = 10) {
  qgrubbs(q, n, type, rev = TRUE)
}

#' @title grubbs.test.
#' @description To calculate the Grubbs test statistic.
#' @param x The vector of lab means.
#' @param type Test either single (10) or double (20).
#' @param tail Test either lower or upper extreme value(s).
#' @noRd
#' @keywords internal
#' @examples
#' test <- shiny::isolate(eCerto:::test_rv("SR3")$c_lab_means())
#' grubbs.test(x = test[,"mean"])
#' grubbs.test(x = test[,"mean"], tail = "upper")
#' # comparison with other packages
#' grubbs.test(x = test[,"mean"], type = 20)
#' # identical result as for outliers package
#' outliers::grubbs.test(x = test[,"mean"], type = 20, two.sided = FALSE)
#' # moderate difference to PMCMRplus package
#' PMCMRplus::doubleGrubbsTest(x = test[,"mean"], alternative = "less")

grubbs.test <- function (x, type = 10, tail = c("lower", "upper")) {
  tail <- match.arg(tail)
  if (!any(type==c(10, 20))) {
    message("'grubbs.test' is only implemented for type = 10 or 20. Using type = 20 (double Grubbs).")
  }
  x <- sort(x[stats::complete.cases(x)])
  n <- length(x)
  if (type == 10) {
    # single Grubbs
    if (tail=="lower") {
      alt = paste("lowest value", x[1], "is an outlier")
      o <- x[1]
      d <- x[2:n]
    } else {
      alt = paste("highest value", x[n], "is an outlier")
      o <- x[n]
      d <- x[1:(n - 1)]
    }
    g <- abs(o - mean(x))/sd(x)
    u <- stats::var(d)/stats::var(x) * (n - 2)/(n - 1)
    pval <- 1 - pgrubbs(g, n, type = 10)
    method <- "Grubbs test for one outlier"
  } else {
    # double Grubbs
    if (tail=="lower") {
      alt = paste("lowest values", x[1], "and", x[2], "are outliers")
      u <- stats::var(x[3:n])/stats::var(x) * (n - 3)/(n - 1)
    } else {
      alt = paste("highest values", x[n - 1], "and", x[n], "are outliers")
      u <- stats::var(x[1:(n - 2)])/stats::var(x) * (n - 3)/(n - 1)
    }
    g <- NULL
    pval <- pgrubbs(u, n, type = 20)
    method <- "Grubbs test for two outliers"
  }
  out <- list(
    "statistic" = c(G = g, U = u),
    "alternative" = alt,
    "p.value" = pval,
    "method" = method,
    "data.name" = "eCerto internal"
  )
  class(out) <- "htest"
  return(out)
}

#' @title pcochran.
#' @description To calculate the Cochran P-value.
#' @param q The obtained quantile or Cochran test statistic value.
#' @param n The number of replicates per group.
#' @param k The number of groups.
#' @noRd
#' @keywords internal
#' @examples
#' 1-pcochran(0.54403,5,5)
pcochran <- function (q, n, k) {
  f <- (1/q - 1)/(k - 1)
  p <- 1 - stats::pf(f, (n - 1) * (k - 1), n - 1) * k
  p[p < 0] <- 0
  p[p > 1] <- 1
  return(p)
}

#' @title qcochran.
#' @description To calculate the critical Cochran value C.
#' @param p The desired p-value level, i.e. 0.01 or 0.05 for a one sided test.
#' @param n The number of replicates per group.
#' @param k The number of groups.
#' @noRd
#' @keywords internal
#' @examples
#' qcochran(0.05,5,5)
#' outliers::qcochran(1-0.05,5,5)
#' outliers::cochran.test(c(rep(1,4),4.7725758), rep(5,5))$p.value
#' outliers::cochran.test(c(rep(1,4),4.7725758), rep(5,5))$statistic
qcochran <- function (p, n, k) {
  f <- stats::qf(p/k, (n - 1) * (k - 1), n - 1)
  c <- 1/(1 + (k - 1) * f)
  return(c)
}

#' @title cochran.test.
#' @description To calculate the Cochran test statistic.
#' @param vars The vector of lab variances.
#' @param ns The (mean) number of replicates per lab.
#' @noRd
#' @keywords internal
#' @examples
#' cochran.test(c(rep(1,4),4.7725758), ns = 5)
#' cochran.test(vars = c(rep(1,4),4.7725758), ns = rep(5,5))
#' cochran.test(vars = c(rep(1,4),4.7725758), ns = c(rep(5,4),4))
cochran.test <- function (vars, ns) {
  k <- length(vars)
  names(vars) <- 1:k
  if (length(ns)!=1 && length(ns)==k) ns <- mean(ns)
  val <- max(vars)/sum(vars)
  out <- list(
    "statistic" = c("C" = val),
    "parameter" = c("df" = ns, "k" = k),
    "alternative" = paste("Group", names(which(vars == max(vars))), "has outlying variance"),
    "p.value" = 1 - pcochran(val, ns, k),
    "method" = "Cochran test for outlying variance",
    "estimate" = vars,
    "data.name" = "eCerto internal"
  )
  class(out) <- "htest"
  return(out)
}

#' @title qdixon.
#' @description To calculate (approximate) the critical Dixon value Q.
#' @param p The desired p-value level, i.e. 0.01 or 0.05 for a one sided test.
#' @param n The number of values or labs.
#' @param rev Set reverse = TRUE to return a P value rather than the critical value.
#' @noRd
#' @keywords internal
#' @examples
#' dixon_q <- qdixon(p = 0.05, n = 5)
#' qdixon(p = dixon_q, n = 5, rev = TRUE)
qdixon <- function (p, n, rev = FALSE) {
  q <- eCerto::cvals_Dixon
  pp <- as.numeric(colnames(q))
  nn <- as.numeric(rownames(q))
  q0 <- q[min(which(n<=nn)),]
  if (rev) { res <- qtable(p, q0, pp) } else { res <- qtable(p, pp, q0) }
  res[res < 0] <- 0
  res[res > 1] <- 1
  return(res)
}

#' @title pdixon.
#' @description To calculate the Dixon P value based on a given test statistic q.
#' @param q The obtained Dixon test statistic.
#' @param n The number of values or labs.
#' @noRd
#' @keywords internal
pdixon <- function (q, n) {
  qdixon(q, n, rev = TRUE)
}

#' @title dixon.test.
#' @description To calculate the Dixon test statistic.
#' @param x The vector of lab variances.
#' @param opposite Testing lower or upper end for FALSE and TRUE respectively.
#'     In eCerto only the one sided version is used.
#' @param two.sided two.sided.
#' @noRd
#' @keywords internal
#' @examples
#' test <- shiny::isolate(eCerto:::test_rv("SR3")$c_lab_means())
#' dixon.test(x = test[,"mean"])
dixon.test <- function (x, opposite = FALSE, two.sided = FALSE) {
  x <- sort(x[stats::complete.cases(x)])
  n <- length(x)
  if (n <= 7) {
    type <- "10"
  } else if (n > 7 & n <= 10) {
    type <- "11"
  } else if (n > 10 & n <= 13) {
    type <- "21"
  } else type <- "22"
  if (xor(((x[n] - mean(x)) < (mean(x) - x[1])), opposite)) {
    alt = paste("lowest value", x[1], "is an outlier")
    q <- switch(
      type,
      "10" = (x[2] - x[1])/(x[n] - x[1]),
      "11" = (x[2] - x[1])/(x[n - 1] - x[1]),
      "21" = (x[3] - x[1])/(x[n - 1] - x[1]),
      (x[3] - x[1])/(x[n - 2] - x[1])
    )
  } else {
    alt = paste("highest value", x[n], "is an outlier")
    q <- switch(
      type,
      "10" = (x[n] - x[n - 1])/(x[n] - x[1]),
      "11" = (x[n] - x[n - 1])/(x[n] - x[2]),
      "21" = (x[n] - x[n - 2])/(x[n] - x[2]),
      (x[n] - x[n - 2])/(x[n] - x[3])
    )
  }
  pval <- pdixon(q, n)
  if (two.sided) {
    pval <- 2 * pval
    if (pval > 1) pval <- 2 - pval
  }
  out <- list(
    "statistic" = c(Q = q),
    "alternative" = alt,
    "p.value" = pval,
    "method" = "Dixon test for outliers",
    "data.name" = "eCerto internal"
  )
  class(out) <- "htest"
  return(out)
}

#' @title qtable.
#' @description This function calculates critical values or p-values which cannot be obtained numerically, and only tabulated version is available.
#' @param p The vector of probabilities.
#' @param probs The vector of probabilities.
#' @param quants The vector of quantiles corresponding to `probs`.
#' @noRd
#' @keywords internal
#' @examples
#' data(cvals_Dixon)
#' qtable(p = 0.95, probs = pp, quants = cvals_Dixon[1,])
qtable <- function (p, probs, quants) {
  quants <- quants[order(probs)]
  probs <- sort(probs)
  res <- vector()
  for (n in 1:length(p)) {
    pp <- p[n]
    if (pp <= probs[1]) {
      q0 <- quants[c(1, 2)]
      p0 <- probs[c(1, 2)]
      fit <- stats::lm(q0 ~ p0)
    }
    else if (pp >= probs[length(probs)]) {
      q0 <- quants[c(length(quants) - 1, length(quants))]
      p0 <- probs[c(length(probs) - 1, length(probs))]
      fit <- stats::lm(q0 ~ p0)
    }
    else {
      x0 <- which(abs(pp - probs) == min(abs(pp - probs)))
      x1 <- which(abs(pp - probs) == sort(abs(pp - probs))[2])
      x <- min(c(x0, x1))
      if (x == 1)
        x <- 2
      if (x > length(probs) - 2)
        x <- length(probs) - 2
      i <- c(x - 1, x, x + 1, x + 2)
      q0 <- quants[i]
      p0 <- probs[i]
      fit <- stats::lm(q0 ~ poly(p0, 3))
    }
    res <- c(res, stats::predict(fit, newdata = list(p0 = pp)))
  }
  return(res)
}

#' @title round_up.
#' @description Round always up.
#' @param x A number.
#' @param n precision after decimal.
#' @examples
#' x <- c(0.011, -0.021, 0.0299999, 0.03, 0.031, 0.000299, 29.01, 3.01, 200, -300)
#' eCerto:::round_up(x, n = 2)
#' eCerto:::round_up(x = x, n = eCerto:::digits_DIN1333(x))
#' @noRd
#' @keywords internal
round_up <- function(x, n=0) {
  sign(x)*ceiling(abs(x)*10^n + sqrt(.Machine$double.eps))/(10^n)
}

#' @title pval2level.
#' @description Will convert a numeric vector of p-values into a character vector indicating levels.
#' @param p A numeric vector of p-values.
#' @param fmt Character description of the requested output format. Currently only 'eCerto' is supported.
#' @examples
#' x <- c(NA, 0.009, 0.049, 0.09, 0.9, 9)
#' eCerto:::pval2level(p = x)
#' @noRd
#' @keywords internal
pval2level <- function(p, fmt = c("eCerto")) {
  sapply(p, function(x) { ifelse(is.na(x),".",ifelse(x<0.01,".01",ifelse(x<0.05,".05","n.s."))) })
}
