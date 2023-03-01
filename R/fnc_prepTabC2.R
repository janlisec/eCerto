#' @title prepTabC2.
#' @description Compute lab-mean stats for Tab C2 of C Module.
#' @param data Table with columns 'Lab' and 'value'.
#' @param precision Rounding precision.
#' @examples
#' x <- eCerto:::test_Certification_Excel()
#' eCerto:::prepTabC2(data = x)
#' @return A data frame.
#' @keywords internal
#' @importFrom moments skewness kurtosis agostino.test anscombe.test
#' @noRd
prepTabC2 <- function(data=NULL, precision=4) {
  x <- sapply(split(data$value, data$Lab), mean)
  out <- data.frame(
    "Mean"=round(mean(x), precision),
    "Median"=round(stats::median(x), precision),
    "SD"=round(stats::sd(x), precision),
    "MAD"=round(stats::mad(x), precision),
    "Bartlett_p"=formatC(stats::bartlett.test(value~Lab, data=data)$p.value, format="E", digits=2),
    "ANOVA_p"=formatC(stats::anova(stats::lm(value~Lab, data=data))$Pr[1], format="E", digits=2),
    "KS_p"=formatC(stats::ks.test(x=x, y="pnorm", mean = mean(x), sd = stats::sd(x))$p.value, format="E", digits=2),
    "Skewness"=round(moments::skewness(x = x), precision),
    "Agostino_p"=NA,
    "Kurtosis"=round(moments::kurtosis(x = x), precision),
    "Anscombe_p"=NA
  )
  test <- try(moments::agostino.test(x = x), silent=TRUE)
  if (!inherits(test, "try-error")) out$Agostino_p <- formatC(test$p.value, format="E", digits=2)
  test <- try(moments::anscombe.test(x = x), silent=TRUE)
  if (!inherits(test, "try-error")) out$Anscombe_p <- formatC(test$p.value, format="E", digits=2)
  return(out)
}