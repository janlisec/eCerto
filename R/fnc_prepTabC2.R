#' @title prepTabC2.
#' @description Compute lab-mean stats for Tab C2 of C Module.
#' @param dat Table with columns 'Lab' and 'value'.
#' @param excl_labs Exclude filtered Labs from table.
#' @examples
#' x <- eCerto:::test_Certification_Excel()
#' eCerto:::prepTabC2(dat = x)
#' @return A data frame.
#' @keywords internal
#' @importFrom moments skewness kurtosis agostino.test anscombe.test
#' @noRd
prepTabC2 <- function(dat=NULL, excl_labs=FALSE) {
  if (excl_labs) {
    # remove filtered labs and re-factor column 'Lab'
    L_flt <- unique(as.character(dat[dat[,"L_flt"],"Lab"]))
    dat <- dat[!dat[,"L_flt"],]
    dat[,"Lab"] <- factor(dat[,"Lab"])
  }
  x <- sapply(split(dat$value, dat$Lab), mean)
  if (shiny::isRunning()) {
    shiny::validate(shiny::need(expr = length(x)>1, message = "No statistics for single Lab possible."))
  } else {
    stopifnot(length(x)>1)
  }
  out <- data.frame(
    "Mean" = mean(x),
    "Median" = stats::median(x),
    "SD" = stats::sd(x),
    "MAD" = stats::mad(x),
    "Bartlett_p" = stats::bartlett.test(value~Lab, data=dat)$p.value,
    "ANOVA_p" = stats::anova(stats::lm(value~Lab, data=dat))$Pr[1],
    "KS_p" = stats::ks.test(x=x, y="pnorm", mean = mean(x), sd = stats::sd(x))$p.value,
    "Skewness" = moments::skewness(x = x),
    "Agostino_p" = NA,
    "Kurtosis" = moments::kurtosis(x = x),
    "Anscombe_p" = NA
  )
  tmp <- list(
    "Agostino_p" = try(moments::agostino.test(x = x)$p.value, silent=TRUE),
    "Anscombe_p" = try(moments::anscombe.test(x = x)$p.value, silent=TRUE)
  )
  for (i in 1:length(tmp)) {
    if (!inherits(tmp[[i]], "try-error")) out[,names(tmp)[i]] <- tmp[[i]]
  }
  return(out)
}