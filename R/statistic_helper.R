#' @description BAMTool, Modul: Certification, Scheffe's multiple t-test
#' @param data Table with columns 'Lab' and 'value'.
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
#' @noRd
Dixon <- function(lab_means=NULL, fmt=c("alpha", "pval")[1]) {
  x <- lab_means[,"mean"]
  out <- rep(NA, length(x))
  if (length(x)>=3 && length(x)<=30 && diff(range(x))>0) {
    smallest_is_extreme <- (max(x) - mean(x)) <= (mean(x) - min(x))
    # calculate outlier p to the max
    l_max <- x==max(x)
    out[l_max] <- outliers::dixon.test(x=x, type = 0, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,TRUE,FALSE))$p.value
    # calculate outlier p to the min
    l_min <- x==min(x)
    out[l_min] <- outliers::dixon.test(x=x, type = 0, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,FALSE,TRUE))$p.value
    # reformat p-values
    if (fmt=="alpha") out <- sapply(out, function(x) { ifelse(is.na(x),".",ifelse(x<0.01,".01",ifelse(x<0.05,".05","n.s."))) })
  } else {
    if (fmt=="alpha") out <- rep("Error", length(x))
  }
  return(data.frame("Dixon_p"=out, row.names=row.names(lab_means)))
}

#' @description BAMTool, Modul: Certification, Grubbs Test
#' @param lab_means data.frame, output of Stats function.
#' @param fmt Output format. Either the p-values directly or expressed qualitatively.
#' @noRd
Grubbs <- function(lab_means = NULL, fmt=c("alpha", "pval")[1]) {
  out <- data.frame("Grubbs1_p" = rep(NA, nrow(lab_means)), row.names = row.names(lab_means))
  x <- lab_means[, "mean"]
  if (length(x)>=3 && length(x)<=30 && diff(range(x))>0) {
    smallest_is_extreme <- (max(x) - mean(x)) <= (mean(x) - min(x))
    out$Grubbs1_p[which.max(x)] <- outliers::grubbs.test(x = x, type = 10, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, TRUE, FALSE))$p.value
    out$Grubbs1_p[which.min(x)] <- outliers::grubbs.test(x = x, type = 10, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, FALSE, TRUE))$p.value
    if (fmt=="alpha") out$Grubbs1_p <- sapply(out$Grubbs1_p, function(x) { ifelse(is.na(x), ".", ifelse(x < 0.01, ".01", ifelse(x < 0.05, ".05", "n.s."))) })
    if (length(x) >= 4) {
      out$Grubbs2_p <- rep(NA, length(x))
      out$Grubbs2_p[order(x, decreasing = T)[1:2]] <- outliers::grubbs.test(x = x, type = 20, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, TRUE, FALSE))$p.value
      out$Grubbs2_p[order(x, decreasing = F)[1:2]] <- outliers::grubbs.test(x = x, type = 20, two.sided = FALSE, opposite = ifelse(smallest_is_extreme, FALSE, TRUE))$p.value
      if (fmt=="alpha") out$Grubbs2_p <- sapply(out$Grubbs2_p, function(x) { ifelse(is.na(x), ".", ifelse(x < 0.01, ".01", ifelse(x < 0.05, ".05", "n.s."))) })
    }
  } else {
    if (fmt=="alpha") out$Grubbs1_p <- rep("Error", length(x))
  }
  return(out)
}

#' @description BAMTool, Modul: Certification, Nalimov Test
#' @param lab_means data.frame, output of Stats function.
#' @noRd
Nalimov <- function(lab_means=NULL) {
  nalimov_crit <- structure(
    list(
      f = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 100L, 200L, 300L, 400L, 500L, 600L, 700L, 800L, 1000L),
      a_05 = c(1.409, 1.645, 1.757, 1.814, 1.848, 1.87, 1.885, 1.895, 1.903, 1.91, 1.916, 1.92, 1.923, 1.926, 1.928, 1.931, 1.933, 1.935, 1.936, 1.937, 1.942, 1.945, 1.948, 1.949, 1.95, 1.951, 1.956, 1.958, 1.958, 1.959, 1.959, 1.959, 1.959, 1.959, 1.96),
      a_01 = c(1.414, 1.715, 1.918, 2.051, 2.142, 2.208, 2.256, 2.294, 2.324, 2.348, 2.368, 2.385, 2.399, 2.412, 2.423, 2.432, 2.44, 2.447, 2.454, 2.46, 2.483, 2.498, 2.509, 2.518, 2.524, 2.529, 2.553, 2.564, 2.566, 2.568, 2.57, 2.571, 2.572, 2.573, 2.576),
      a_001 = c(1.414, 1.73, 1.982, 2.178, 2.329, 2.447, 2.54, 2.616, 2.678, 2.73, 2.774, 2.812, 2.845, 2.874, 2.899, 2.921, 2.941, 2.959, 2.975, 2.99, 3.047, 3.085, 3.113, 3.134, 3.152, 3.166, 3.227, 3.265, 3.271, 3.275, 3.279, 3.281, 3.283, 3.285, 3.291)
    ), class = "data.frame", row.names = c(NA, -35L)
  )
  nalimov <- function(x, m, s, n) {
    abs((x-m)/s)*sqrt(n/(n-1))
  }
  cval <- sapply(lab_means$mean, function(x) {
    nalimov(x=x, m=mean(lab_means$mean), s=stats::sd(lab_means$mean), n=nrow(lab_means))
  })

  return(data.frame(
    "Nalimov"=sapply(cval, function(x) {
      l <- max(which(nalimov_crit[,"f"]<=(nrow(lab_means)-2)))
      ifelse(x<nalimov_crit[l,"a_05"], ".", ifelse(x>=nalimov_crit[l,"a_01"], ".01", ".05"))
    }),
    row.names=rownames(lab_means)
  )
  )
}

#' @description BAMTool, Modul: Certification, Cochran Test
#' @param data Table with columns 'Lab' and 'value'.
#' @param fmt Output format. Either the p-values directly or expressed qualitatively.
#' @noRd
Cochran <- function(data=NULL, fmt=c("alpha", "pval")[1]) {
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
      "alpha"="excl",
      "pval"=NA
    )
  }
  i <- 1
  while (length(vars)>=3 & i>0) {
    ctest <- outliers::cochran.test(object=vars, data=ns)
    j <- which.max(vars)
    if (is.finite(ctest$p.value) && ctest$p.value<=0.05) {
      out[rownames(out)==names(j),"Cochran"] <- switch(
        fmt,
        "alpha"=paste0("[",i,"] ", ifelse(ctest$p.value<0.01,".01",".05")),
        "pval"=ctest$p.value
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
