#' @description BAMTool, Modul: Certification, Scheffe's multiple t-test
#' @param data Table with columns 'Lab' and 'value'.
#' @importFrom agricolae scheffe.test
#' @noRd
Scheffe <- function(data=NULL) {
  S05 <- try(agricolae::scheffe.test(y = stats::lm(value~Lab, data=data), trt="Lab", alpha = 0.05)$group[levels(data$Lab),"groups"], silent=TRUE)
  if (inherits(S05, "try-error")) S05 <- rep("Error", length(levels(data$Lab)))
  S01 <- try(agricolae::scheffe.test(y = stats::lm(value~Lab, data=data), trt="Lab", alpha = 0.01)$group[levels(data$Lab),"groups"], silent=TRUE)
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
#' @importFrom outliers dixon.test
#' @noRd
Dixon <- function(lab_means=NULL, fmt=c("alpha", "pval", "cval")) {
  fmt <- match.arg(fmt)
  x <- lab_means[,"mean"]
  n <- length(x)
  out <- rep(NA, n)
  if (n>=3 && n<=30 && diff(range(x))>0) {
    smallest_is_extreme <- (max(x) - mean(x)) <= (mean(x) - min(x))
    # calculate outliers at both ends
    d_upper <- outliers::dixon.test(x=x, type = 0, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,TRUE,FALSE))
    d_lower <- outliers::dixon.test(x=x, type = 0, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,FALSE,TRUE))
    out[which.max(x)] <- d_upper$p.value
    out[which.min(x)] <- d_lower$p.value
    # reformat p-values
    if (fmt=="alpha") out <- pval2level(p = out)
    if (fmt=="cval") {
      out[which.max(x)] <- d_upper$statistic
      out[which.min(x)] <- d_lower$statistic
    }
  } else {
    err <- ifelse(n<3, "n<3", ifelse(n>30, "n>30", ifelse(diff(range(x))>0, "var(x)=0", "Error")))
    out <- rep(err, n)
  }
  return(data.frame("Dixon_p"=out, row.names=row.names(lab_means)))
}

#' @description BAMTool, Modul: Certification, Grubbs Test
#' @param lab_means data.frame, output of Stats function.
#' @param fmt Output format. Either the p-values directly or expressed qualitatively.
#' @noRd
#' @importFrom outliers grubbs.test
#' @examples
#' test <- shiny::isolate(eCerto:::test_rv("SR3")$c_lab_means())
#' Grubbs(lab_means=test, fmt=c("alpha", "pval", "cval")[3])
Grubbs <- function(lab_means = NULL, fmt=c("alpha", "pval", "cval")) {
  fmt <- match.arg(fmt)
  out <- data.frame("Grubbs1_p" = rep(NA, nrow(lab_means)), row.names = row.names(lab_means))
  x <- lab_means[, "mean"]
  n <- length(x)
  if (n>=3 && n<=30 && diff(range(x))>0) {
    smallest_is_extreme <- (max(x) - mean(x)) <= (mean(x) - min(x))
    out$Grubbs1_p[which.max(x)] <- outliers::grubbs.test(x = x, type = 10, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, TRUE, FALSE))$p.value
    out$Grubbs1_p[which.min(x)] <- outliers::grubbs.test(x = x, type = 10, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, FALSE, TRUE))$p.value
    if (fmt=="alpha") out$Grubbs1_p <- pval2level(p = out$Grubbs1_p)
    if (fmt=="cval") {
      out$Grubbs1_p[!is.na(out$Grubbs1_p)] <- abs(lab_means[!is.na(out$Grubbs1_p),"mean"]-mean(x))/sd(x)
    }
    if (n >= 4) {
      out$Grubbs2_p <- rep(NA, n)
      minvals <- order(x, decreasing = TRUE)[1:2]
      maxvals <- order(x, decreasing = FALSE)[1:2]
      out$Grubbs2_p[minvals] <- outliers::grubbs.test(x = x, type = 20, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, TRUE, FALSE))$p.value
      out$Grubbs2_p[maxvals] <- outliers::grubbs.test(x = x, type = 20, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, FALSE, TRUE))$p.value
      if (fmt=="alpha") out$Grubbs2_p <- pval2level(p = out$Grubbs2_p)
      if (fmt=="cval") {
        out$Grubbs2_p[minvals] <- stats::var(x[-minvals])/stats::var(x) * (n - 3)/(n - 1)
        out$Grubbs2_p[maxvals] <- stats::var(x[-maxvals])/stats::var(x) * (n - 3)/(n - 1)
      }
    }
  } else {
    if (fmt=="alpha") out$Grubbs1_p <- rep("Error", length(x))
  }
  return(out)
}

#' @description BAMTool, Modul: Certification, Nalimov Test
#' @param lab_means data.frame, output of Stats function with at least numeric column 'mean' and potentially row names.
#' @noRd
#' @importFrom outliers qtable
#' @examples
#' test <- data.frame("mean"=rnorm(5))
#' Nalimov(lab_means = test)
#' Nalimov(lab_means = rbind(test,3))
#' Nalimov(lab_means = rbind(test,3), fmt = "cval")
#' Nalimov(lab_means = rbind(test,3), fmt = "pval")
Nalimov <- function(lab_means = NULL, fmt = c("alpha", "pval", "cval")) {
  fmt <- match.arg(fmt)
  # a critical value table for Nalimov from http://www.statistics4u.info/fundstat_germ/ee_nalimov_outliertest.html
  # [JL, 20230216] This is probably the wrong table and contains Nalimov r (for homogeneity testing) and not r_max (for outlier testing)
  # nalimov_crit <- structure(list(
  #   f = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 100L, 200L, 300L, 400L, 500L, 600L, 700L, 800L, 1000L),
  #   a_05 = c(1.409, 1.645, 1.757, 1.814, 1.848, 1.87, 1.885, 1.895, 1.903, 1.91, 1.916, 1.92, 1.923, 1.926, 1.928, 1.931, 1.933, 1.935, 1.936, 1.937, 1.942, 1.945, 1.948, 1.949, 1.95, 1.951, 1.956, 1.958, 1.958, 1.959, 1.959, 1.959, 1.959, 1.959, 1.96),
  #   a_01 = c(1.414, 1.715, 1.918, 2.051, 2.142, 2.208, 2.256, 2.294, 2.324, 2.348, 2.368, 2.385, 2.399, 2.412, 2.423, 2.432, 2.44, 2.447, 2.454, 2.46, 2.483, 2.498, 2.509, 2.518, 2.524, 2.529, 2.553, 2.564, 2.566, 2.568, 2.57, 2.571, 2.572, 2.573, 2.576),
  #   a_001 = c(1.414, 1.73, 1.982, 2.178, 2.329, 2.447, 2.54, 2.616, 2.678, 2.73, 2.774, 2.812, 2.845, 2.874, 2.899, 2.921, 2.941, 2.959, 2.975, 2.99, 3.047, 3.085, 3.113, 3.134, 3.152, 3.166, 3.227, 3.265, 3.271, 3.275, 3.279, 3.281, 3.283, 3.285, 3.291)
  # ), class = "data.frame", row.names = c(NA, -35L))
  # calculating the critical values for the Nalimov test (basically Grubbs G with a weighting factor depending on n)
  m <- mean(lab_means[, "mean"])
  s <- stats::sd(lab_means[, "mean"])
  n <- nrow(lab_means)
  # [JL, 20230216] we performed Nalimov for all but should only be applied for extreme deviation from population mean
  # cval <- sapply(lab_means[, "mean"], function(x) {
  #   abs((x - m) / s) * sqrt(n / (n - 1))
  # })
  out <- data.frame("Nalimov" = rep(NA, n), row.names = rownames(lab_means))
  l <- which.max(abs(m-lab_means[, "mean"]))
  cval <- abs((lab_means[l, "mean"] - m) / s) * sqrt(n / (n - 1))
  pval <- pnalimov(q = cval, n = n)
  out[l, "Nalimov"] <- pval
  # formatting the output
  if (fmt=="cval") {
    out[l, "Nalimov"] <- cval
  }
  # if (fmt=="pval") {
  #   # pp <- c(0.0001, 0.001, 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.4, 0.6, 0.8, 0.9, 0.95, 0.975, 0.99, 1)
  #   # qtab <- sapply(pp, function(x) { qnalimov(p=x, n=n) })
  #   # pval <- outliers::qtable(p=out[l, "Nalimov"], qtab, pp)
  #   # if (pval>1) pval <- 1
  #   # if (pval<0) pval <- 0
  #   out[l, "Nalimov"] <- pval
  # }
  if (fmt=="alpha") {
    out[, "Nalimov"] <- pval2level(p = out[, "Nalimov"])
    # out[l, "Nalimov"] <- switch(
    #   as.character(sum(out[l, "Nalimov"] >= sapply(c(0.05, 0.01), function(x) { qnalimov(p = x, n = n) }))),
    #   "0" = ".",
    #   "1" = ".05",
    #   "2" = ".01"
    # )
    # f <- (n - 2)
    # out[,"Nalimov"] <- sapply(cval, function(x) {
    #   switch(
    #     as.character(sum(x >= nalimov_crit[max(which(nalimov_crit[, "f"] <= f)), 2:3])),
    #     "0" = ".",
    #     "1" = ".05",
    #     "2" = ".01"
    #   )
    # })
  }
  return(out)
}

#' @description BAMTool, Modul: Certification, Cochran Test
#' @param data Table with columns 'Lab' and 'value'.
#' @param fmt Output format. Either the p-values directly or expressed qualitatively.
#' @noRd
#' @importFrom outliers cochran.test
#' @examples
#' test <- eCerto:::test_rv("SR3")$c_fltData()
#' Cochran(data=test, fmt=c("alpha", "pval")[1])
Cochran <- function(data=NULL, fmt=c("alpha", "pval", "cval")) {
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
      "cval" = "excl"
    )
  }
  i <- 1
  while (length(vars)>=3 & i>0) {
    ctest <- outliers::cochran.test(object=vars, data=ns)
    j <- which.max(vars)
    if (is.finite(ctest$p.value) && ctest$p.value<=0.05) {
      out[rownames(out)==names(j),"Cochran"] <- switch(
        fmt,
        "alpha" = paste0("[",i,"] ", pval2level(ctest$p.value)),
        "pval" = ctest$p.value,
        "cval" = ctest$statistic
      )
      vars <- vars[-j]
      ns <- ns[-j]
      i <- i+1
    } else {
      # no more testing necessary
      if (fmt=="alpha") out[is.na(out[,"Cochran"]),"Cochran"] <- "."
      i <- 0
    }
  }
  return(out)
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
#' @description Will convert a numeric vector of p-values ino a character vector indicating levels.
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

#' @title qgrubbs.
#' @description To calculate the critical Grubbs value z_alpha.
#' @param p The desired p-value level, i.e. 0.01 or 0.05 for a one sided test.
#' @param n The number of replicates.
#' @noRd
#' @keywords internal
#' @examples
#' qgrubbs(0.05, 5)
#' outliers::qgrubbs(p = (1-0.05/2), n = 5)
qgrubbs <- function(p, n) {
  t2 <- stats::qt(p = p/(2*n), df = n-2)^2
  return((n-1)/sqrt(n) * sqrt(t2 / (n-2 + t2)))
}

#' @title qnalimov.
#' @description To calculate the critical Nalimov value r_max.
#' @param p The desired p-value level, i.e. 0.01 or 0.05 for a one sided test.
#' @param n The number of replicates.
#' @noRd
#' @keywords internal
#' @example
#' qnalimov(0.05, 5)
qnalimov <- function(p, n) {
  t2 <- stats::qt(p = p/n, df = n-2)^2
  return((n-1)/sqrt(n) * sqrt(t2 / (n-2 + t2)) * sqrt(n/(n-1)))
}

#' @title pnalimov.
#' @description To calculate p for a critical Nalimov value r_max.
#' @param q The r_max obtained by qnalimov.
#' @param n The number of replicates.
#' @noRd
#' @keywords internal
#' @example
#' r_max <- qnalimov(0.05, 5)
#' pnalimov(q = r_max, n = 5)
pnalimov <- function(q, n) {
  # reverse Nalimov weighting to yield Grubbs G
  G <- q/sqrt(n/(n-1))
  # use reverse function of qgrubbs from outliers package
  t <- sqrt((G^2*n*(2-n)) / (G^2*n-(n-1)^2))
  if (is.nan(t)) {
    res <- 0
  }
  else {
    res <- n * (1 - pt(t, n - 2))
    res[res > 1] <- 1
  }
  return(res)
}

