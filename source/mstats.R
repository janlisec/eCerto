#'BAMTool
#'Modul: Zertifizierung
#'data : data table
mstats <- function(data=NULL, precision=4) {
  lab_means <- plyr::ldply(split(data$value, data$Lab), function(x) {data.frame("mean"=mean(x,na.rm=T), "sd"=sd(x,na.rm=T), "n"=sum(is.finite(x))) }, .id="Lab")
  n <- nrow(lab_means)
  out <- data.frame(
    "Mean"=round(mean(lab_means$mean),precision), 
    "Median"=round(median(lab_means$mean),precision), 
    "SD"=round(sd(lab_means$mean),precision), 
    "MAD"=round(stats::mad(lab_means$mean),precision), 
    "Bartlett_p"=formatC(stats::bartlett.test(value~Lab, data=data)$p.value,format="E",digits=2),
    "ANOVA_p"=formatC(anova(lm(value~Lab, data=data))$Pr[1],format="E",digits=2),
    "KS_p"=formatC(suppressWarnings(stats::ks.test(x=lab_means$mean, y="pnorm", mean = mean(lab_means$mean), sd = sd(lab_means$mean))$p.value), format="E",digits=2),
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
