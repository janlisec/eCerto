#'BAMTool
#'Modul: Zertifizierung
#'Lab stats
#'data : data table
#'
#' @param data
#' @param precision
#' @noRd
Stats <- function(data = NULL, precision = 4) {
  lab_means <-
    plyr::ldply(split(data$value, data$Lab), function(x) {
      data.frame(
        "mean" = round(mean(x, na.rm = T), precision),
        "sd" = round(stats::sd(x, na.rm = T), precision),
        "n" = sum(is.finite(x)),
        stringsAsFactors = FALSE
      )
    }, .id = "Lab")
  rownames(lab_means) <- lab_means$Lab
  out <- data.frame(
    lab_means,
    Scheffe(data = data),
    Dixon(lab_means = lab_means),
    Grubbs(lab_means = lab_means),
    Nalimov(lab_means = lab_means),
    Cochran(data = data),
    stringsAsFactors = FALSE
  )
  return(out[order(out[, "mean"]), ])
}

#'BAMTool
#'Modul: Zertifizierung
#'Scheffe's multiple t-test
#'data : data table
#'
#' @param data
#' @noRd
Scheffe <- function(data=NULL) {
  return(data.frame(
    "Scheffe_05"=agricolae::scheffe.test(y = stats::lm(value~Lab, data=data), trt="Lab", alpha = 0.05)$group[levels(data$Lab),"groups"],
    "Scheffe_01"=agricolae::scheffe.test(y = stats::lm(value~Lab, data=data), trt="Lab", alpha = 0.01)$group[levels(data$Lab),"groups"],
    row.names=levels(data$Lab))
  )
}

#' Title
#' BAMTool
#' Modul: Zertifizierung
#' Dixon
#'
#' @param lab_means data.frame
#'
#' @return
#' @noRd
Dixon <- function(lab_means=NULL) {
  out <- data.frame("Dixon_p"=rep(NA,nrow(lab_means)), row.names=row.names(lab_means))
  x <- lab_means[,"mean"]
  smallest_is_extreme <- (max(x) - mean(x)) <= (mean(x) - min(x))
  if (nrow(lab_means)>=3) out$Dixon_p[which.max(lab_means$mean)] <- outliers::dixon.test(x=lab_means$mean, type = 0, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,TRUE,FALSE))$p.value
  if (nrow(lab_means)>=3) out$Dixon_p[which.min(lab_means$mean)] <- outliers::dixon.test(x=lab_means$mean, type = 0, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,FALSE,TRUE))$p.value
  out$Dixon_p <- sapply(out$Dixon_p, function(x) { ifelse(is.na(x),".",ifelse(x<0.01,".01",ifelse(x<0.05,".05","n.s."))) })
  return(out)
}

#'BAMTool
#'Modul: Zertifizierung
#'Grubbs
#'lab_means : data.frame
#'
#' @param lab_means
#' @noRd
Grubbs <- function(lab_means = NULL) {
  out <-
    data.frame("Grubbs1_p" = rep(NA, nrow(lab_means)),
               row.names = row.names(lab_means))
  x <- lab_means[, "mean"]
  smallest_is_extreme <- (max(x) - mean(x)) <= (mean(x) - min(x))
  out$Grubbs1_p[which.max(lab_means$mean)] <-
    outliers::grubbs.test(
      x = lab_means$mean,
      type = 10,
      two.sided = FALSE,
      opposite = ifelse(smallest_is_extreme, TRUE, FALSE)
    )$p.value
  out$Grubbs1_p[which.min(lab_means$mean)] <-
    outliers::grubbs.test(
      x = lab_means$mean,
      type = 10,
      two.sided = FALSE,
      opposite = ifelse(smallest_is_extreme, FALSE, TRUE)
    )$p.value
  out$Grubbs1_p <-
    sapply(out$Grubbs1_p, function(x) {
      ifelse(is.na(x), ".", ifelse(x < 0.01, ".01", ifelse(x < 0.05, ".05", "n.s.")))
    })
  if (nrow(lab_means) >= 4) {
    out$Grubbs2_p <- rep(NA, nrow(lab_means))
    out$Grubbs2_p[order(lab_means$mean, decreasing = T)[1:2]] <-
      outliers::grubbs.test(
        x = lab_means$mean,
        type = 20,
        two.sided = FALSE,
        opposite = ifelse(smallest_is_extreme, TRUE, FALSE)
      )$p.value
    out$Grubbs2_p[order(lab_means$mean, decreasing = F)[1:2]] <-
      outliers::grubbs.test(
        x = lab_means$mean,
        type = 20,
        two.sided = FALSE,
        opposite = ifelse(smallest_is_extreme, FALSE, TRUE)
      )$p.value
    out$Grubbs2_p <-
      sapply(out$Grubbs2_p, function(x) {
        ifelse(is.na(x), ".", ifelse(x < 0.01, ".01", ifelse(x < 0.05, ".05", "n.s.")))
      })
  }
  return(out)
}

#'BAMTool
#'Modul: Zertifizierung
#'Nalimov test
#'lab_means : data table
#'
#' @param lab_means
#' @noRd
Nalimov <- function(lab_means=NULL) {
  nalimov_crit <- structure(list(f = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 100L, 200L, 300L, 400L, 500L, 600L, 700L, 800L, 1000L),
                                 a_05 = c(1.409, 1.645, 1.757, 1.814, 1.848, 1.87, 1.885, 1.895, 1.903, 1.91, 1.916, 1.92, 1.923, 1.926, 1.928, 1.931, 1.933, 1.935, 1.936, 1.937, 1.942, 1.945, 1.948, 1.949, 1.95, 1.951, 1.956, 1.958, 1.958, 1.959, 1.959, 1.959, 1.959, 1.959, 1.96),
                                 a_01 = c(1.414, 1.715, 1.918, 2.051, 2.142, 2.208, 2.256, 2.294, 2.324, 2.348, 2.368, 2.385, 2.399, 2.412, 2.423, 2.432, 2.44, 2.447, 2.454, 2.46, 2.483, 2.498, 2.509, 2.518, 2.524, 2.529, 2.553, 2.564, 2.566, 2.568, 2.57, 2.571, 2.572, 2.573, 2.576),
                                 a_001 = c(1.414, 1.73, 1.982, 2.178, 2.329, 2.447, 2.54, 2.616, 2.678, 2.73, 2.774, 2.812, 2.845, 2.874, 2.899, 2.921, 2.941, 2.959, 2.975, 2.99, 3.047, 3.085, 3.113, 3.134, 3.152, 3.166, 3.227, 3.265, 3.271, 3.275, 3.279, 3.281, 3.283, 3.285, 3.291)),
                            class = "data.frame", row.names = c(NA, -35L))
  nalimov <- function(x, m, s, n) {
    abs((x-m)/s)*sqrt(n/(n-1))
  }
  cval <- sapply(lab_means$mean, function(x) {
    nalimov(x=x, m=mean(lab_means$mean), s=stats::sd(lab_means$mean), n=nrow(lab_means))
  })

  return(data.frame(
    "Nalimov"=sapply(cval, function(x) {
      #l <- which.max(nalimov_crit[,"f"]<=(nrow(lab_means)-2))
      l <- max(which(nalimov_crit[,"f"]<=(nrow(lab_means)-2)))
      ifelse(x<nalimov_crit[l,"a_05"], ".", ifelse(x>=nalimov_crit[l,"a_01"], ".01", ".05"))
    }),
    row.names=rownames(lab_means)
  )
  )
}

#'BAMTool
#'Modul: Zertifizierung
#'Cochran test
#'data : data table
#'
#' @param data
#' @noRd
Cochran <- function(data=NULL) {
  vars <- sapply(split(data[,"value"], data[,"Lab"]), stats::var, na.rm=T)
  ns <- sapply(split(data[,"value"], data[,"Lab"]), function(x) { sum(is.finite(x)) })
  out <- data.frame("Cochran"=rep(NA, length(vars)), row.names=names(vars))
  # there might be labs reporting data without variance --> these should be excluded from/before Cochrane
  if (any(vars==0)) {
    flt <- vars>0
    vars <- vars[flt]
    ns <- ns[flt]
    out[!flt,"Cochran"] <- "excl"
  }
  i <- 1
  while (length(vars)>=3 & i>0) {
    ctest <- outliers::cochran.test(object=vars, data=ns)
    j <- which.max(vars)
    if (is.finite(ctest$p.value) && ctest$p.value<=0.05) {
      out[rownames(out)==names(j),"Cochran"] <- paste0("[",i,"] ", ifelse(ctest$p.value<0.01,".01",".05"))
      vars <- vars[-j]
      ns <- ns[-j]
      i <- i+1
    } else {
      out[is.na(out[,"Cochran"]),"Cochran"] <- "."
      i <- 0
    }
  }
  return(out)
}

#'BAMTool
#'Modul: Zertifizierung
#'data : data table
#'
#' @param data
#' @param precision
#' @noRd
mstats <- function(data=NULL, precision=4) {
  lab_means <- plyr::ldply(split(data$value, data$Lab), function(x) {data.frame("mean"=mean(x,na.rm=T), "sd"=stats::sd(x,na.rm=T), "n"=sum(is.finite(x))) }, .id="Lab")
  n <- nrow(lab_means)
  out <- data.frame(
    "Mean"=round(mean(lab_means$mean),precision),
    "Median"=round(stats::median(lab_means$mean),precision),
    "SD"=round(stats::sd(lab_means$mean),precision),
    "MAD"=round(stats::mad(lab_means$mean),precision),
    "Bartlett_p"=formatC(stats::bartlett.test(value~Lab, data=data)$p.value,format="E",digits=2),
    #"Bartlett_p"=ecerto::pn(stats::bartlett.test(value~Lab, data=data)$p.value, precision),
    "ANOVA_p"=formatC(stats::anova(stats::lm(value~Lab, data=data))$Pr[1],format="E",digits=2),
    #"ANOVA_p"=ecerto::pn(stats::anova(stats::lm(value~Lab, data=data))$Pr[1],precision),
    "KS_p"=formatC(suppressWarnings(stats::ks.test(x=lab_means$mean, y="pnorm", mean = mean(lab_means$mean), sd = stats::sd(lab_means$mean))$p.value), format="E",digits=2),
    #"KS_p"=ecerto::pn(suppressWarnings(stats::ks.test(x=lab_means$mean, y="pnorm", mean = mean(lab_means$mean), sd = stats::sd(lab_means$mean))$p.value), precision),
    "Skewness"=round(moments::skewness(x = lab_means$mean),precision),
    "Agostino_p"=NA,
    "Kurtosis"=round(moments::kurtosis(x = lab_means$mean),precision),
    "Anscombe_p"=NA
  )
  test <- try(moments::agostino.test(x = lab_means$mean), silent=TRUE)
  if (!class(test)=="try-error") out$Agostino_p <- formatC(test$p.value,format="E",digits=2)
  test <- try(moments::anscombe.test(x = lab_means$mean), silent=TRUE)
  if (!class(test)=="try-error") out$Anscombe_p <- formatC(test$p.value,format="E",digits=2)
  return(out)
}
