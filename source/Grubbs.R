#'BAMTool
#'Modul: Zertifizierung
#'Grubbs
#'lab_means : data.frame
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
