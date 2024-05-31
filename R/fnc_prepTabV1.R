#' @title fnc_prepTabV1.
#' @description \code{prepTabV1} will perform statistics on imported validation
#'    data.
#' @details tbd.
#' @param tab The imported V data.
#' @param a Analyte name.
#' @param alpha Probability of error.
#' @param k result uncertainty.
#' @param flt_outliers Logical. Shall outliers, determined via an F-test testing
#'     the highest residual be removed from the analysis.
#' @examples
#' #
#' @return A data frame with attributes.
#' @keywords internal
#' @noRd
prepTabV1 <- function(tab = NULL, a = NULL, alpha = 0.05, k = 3, flt_outliers = FALSE) {

  # $$ consider externalizing the call to prepDataV1
  tmp <- prepDataV1(tab = tab, a = a, fmt = "norm")
  df <- data.frame("Conc"=attr(tmp, "Concentration"), "Area_norm"=sapply(tmp, mean, na.rm=TRUE), row.names = 1:length(tmp))
  # write.table(df, file = "clipboard", sep = "\t", row.names = FALSE, col.names = FALSE) # you can export this df for cross checking with DINTest

  # the linear model
  df.lm <- stats::lm(Area_norm ~ Conc, data = df)

  # F-test for outliers (checking highest residual)
  idx <- F_test_outlier(df.lm, alpha = alpha)
  if (!is.na(idx)) {
    check_more <- TRUE
    while (check_more) {
      new_F_Test <- F_test_outlier(stats::lm(Area_norm ~ Conc, data=df[-idx, ]), alpha = alpha)
      if (!is.na(new_F_Test)) {
        idx <- c(idx, as.numeric(rownames(df)[-idx][new_F_Test]))
        if (length(idx)>=(nrow(df)-3)) check_more <- FALSE
      } else {
        check_more <- FALSE
      }
    }
    F_Test <- paste(idx, collapse = ", ")
    if (flt_outliers) {
      df <- df[-idx, ]
      df.lm <- stats::lm(Area_norm ~ Conc, data = df)
      F_Test <- paste0("(", F_Test, ")")
    }
  } else {
    F_Test <- idx
  }

  e <- stats::residuals(df.lm)
  #write.table(data.frame(e), file = "clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)

  # number of calibration levels
  N <- length(e)
  s_yx <- sqrt(sum(e^2)/(N-2))
  s_x0 <- s_yx/stats::coef(df.lm)[2]
  V_x0 <- 100*(s_x0/mean(df[,1], na.rm=T))

  # Mandel test
  df2 <- cbind(df, "Conc2" = df$Conc^2)
  df.qm <- stats::lm(Area_norm ~ Conc + Conc2, data=df2)
  e.qm <- stats::residuals(df.qm)
  P_Mandel <- MandelTest(res_lm = e, res_qm = e.qm)

  # number of replicates
  n <- min(sapply(tmp, length))
  out <- data.frame(
    "Analyte" = levels(factor(attr(tmp, "Analyte"))),
    "N" = N,
    "n" = n,
    "alpha" = 0.05,
    "k" = round(1/k, 2),
    "b0" = stats::coef(df.lm)[1],
    "b1" = stats::coef(df.lm)[2],
    "P_KS_Res" = stats::ks.test(x = e, y="pnorm", mean=mean(e), sd=stats::sd(e))$p.val,
    "P_Neu_Res" = VonNeumannTest(e, unbiased = FALSE)$p.val,
    "F_Test" = F_Test,
    "LOD" = calc_LOD(x = df$Conc, y = df$Area_norm, alpha = alpha, n = n),
    "LOQ" = calc_LOQ(x = df$Conc, y = df$Area_norm, alpha = 0.05, n = n, k = k),
    "s_yx" = s_yx,
    "s_x0" = s_x0,
    "V_x0" = V_x0,
    "P_Mandel" = P_Mandel
  )
  attr(out, "df") <- df
  attr(out, "residuals") <- e
  return(out)
}
