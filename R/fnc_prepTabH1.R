#' @title fnc_prepTabH1.
#' @description \code{prepTabH1} will perform statistics on imported homogeneity
#'    data.
#' @details tbd.
#' @param x The Hom data from an session R6 object.
#' @param adjust TRUE/FALSE to apply bonferroni multiple testing correction
#' @examples
#' x <- eCerto:::test_homog()$data
#' eCerto:::prepTabH1(x = x)
#' @return A data frame.
#' @keywords internal
#' @noRd
prepTabH1 <- function(x, adjust = FALSE) {
  message("[prepTabH1] perform statistics on imported homogeneity data")
  stopifnot(all(c("analyte", "H_type", "Flasche", "value") %in% colnames(x)))
  if (!is.factor(x[, "Flasche"])) x[, "Flasche"] <- factor(x[, "Flasche"])
  out <- plyr::ldply(split(x, x[, "analyte"]), function(y) {
    plyr::ldply(split(y, y[, "H_type"]), function(z) {
      if (nrow(z) >= 2) {
        anm <- stats::anova(stats::lm(value ~ Flasche, data = z))
        M_between <- anm[1, "Mean Sq"]
        M_within <- anm[2, "Mean Sq"]
        mn <- mean(sapply(split(z[, "value"], z[, "Flasche"]), mean, na.rm = T), na.rm = T)
        n_i <- table(as.character(z[, "Flasche"]))
        N <- length(n_i)
        # n <- round(mean(table(as.character(z[,"Flasche"]))))
        # [modified to ISO35[B.4] on suggestion of KV]
        n <- 1 / (N - 1) * (sum(n_i) - sum(n_i^2) / sum(n_i))
        s_bb <- ifelse(M_between > M_within, sqrt((M_between - M_within) / n) / mn, 0)
        s_bb_min <- (sqrt(M_within / n) * (2 / (N * (n - 1)))^(1 / 4)) / mn
        return(data.frame("mean" = mn, "n" = n, "N" = N, "M_between" = M_between, "M_within" = M_within, "P" = anm$Pr[1], "s_bb" = s_bb, "s_bb_min" = s_bb_min))
      } else {
        return(data.frame("mean" = NA, "n" = 0, "N" = 0, "M_between" = 0, "M_within" = 0, "P" = 0, "s_bb" = 0, "s_bb_min" = 0))
      }
    }, .id = "H_type")
  }, .id = "analyte")
  if (!is.null(adjust) && adjust) {
    out[, "P"] <- stats::p.adjust(p = out[, "P"], method = "bonferroni")
    colnames(out) <- gsub("^P$", "P_adj", colnames(out))
  }
  return(out)
}
