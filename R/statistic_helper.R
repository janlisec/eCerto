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
  return(data.frame("Dixon"=out, row.names=row.names(lab_means)))
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
  out <- data.frame("Grubbs1" = rep(NA, nrow(lab_means)), row.names = row.names(lab_means))
  x <- lab_means[, "mean"]
  n <- length(x)
  if (n>=3 && n<=30 && diff(range(x))>0) {
    smallest_is_extreme <- (max(x) - mean(x)) <= (mean(x) - min(x))
    out$Grubbs1[which.max(x)] <- outliers::grubbs.test(x = x, type = 10, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, TRUE, FALSE))$p.value
    out$Grubbs1[which.min(x)] <- outliers::grubbs.test(x = x, type = 10, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, FALSE, TRUE))$p.value
    if (fmt=="alpha") out$Grubbs1 <- pval2level(p = out$Grubbs1)
    if (fmt=="cval") {
      out$Grubbs1[!is.na(out$Grubbs1)] <- abs(lab_means[!is.na(out$Grubbs1),"mean"]-mean(x))/sd(x)
    }
    if (n >= 4) {
      out$Grubbs2 <- rep(NA, n)
      minvals <- order(x, decreasing = TRUE)[1:2]
      maxvals <- order(x, decreasing = FALSE)[1:2]
      out$Grubbs2[minvals] <- outliers::grubbs.test(x = x, type = 20, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, TRUE, FALSE))$p.value
      out$Grubbs2[maxvals] <- outliers::grubbs.test(x = x, type = 20, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, FALSE, TRUE))$p.value
      if (fmt=="alpha") out$Grubbs2 <- pval2level(p = out$Grubbs2)
      if (fmt=="cval") {
        out$Grubbs2[minvals] <- stats::var(x[-minvals])/stats::var(x) * (n - 3)/(n - 1)
        out$Grubbs2[maxvals] <- stats::var(x[-maxvals])/stats::var(x) * (n - 3)/(n - 1)
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
      #if (fmt=="alpha") out[is.na(out[,"Cochran"]),"Cochran"] <- "."
      out[rownames(out)==names(j),"Cochran"] <- switch(
        fmt,
        "alpha" = ".",
        "pval" = ctest$p.value,
        "cval" = ctest$statistic
      )
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