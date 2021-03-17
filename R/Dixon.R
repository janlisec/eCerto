#' Title
#' BAMTool
#' Modul: Zertifizierung
#' Dixon
#'
#' @param lab_means data.frame
#'
#' @return
#' @export
Dixon <- function(lab_means=NULL) {
  out <- data.frame("Dixon_p"=rep(NA,nrow(lab_means)), row.names=row.names(lab_means))
  x <- lab_means[,"mean"]
  smallest_is_extreme <- (max(x) - mean(x)) <= (mean(x) - min(x))
  if (nrow(lab_means)>=3) out$Dixon_p[which.max(lab_means$mean)] <- outliers::dixon.test(x=lab_means$mean, type = 0, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,TRUE,FALSE))$p.value
  if (nrow(lab_means)>=3) out$Dixon_p[which.min(lab_means$mean)] <- outliers::dixon.test(x=lab_means$mean, type = 0, two.sided = FALSE, opposite = ifelse(smallest_is_extreme,FALSE,TRUE))$p.value
  out$Dixon_p <- sapply(out$Dixon_p, function(x) { ifelse(is.na(x),".",ifelse(x<0.01,".01",ifelse(x<0.05,".05","n.s."))) })
  return(out)
}
