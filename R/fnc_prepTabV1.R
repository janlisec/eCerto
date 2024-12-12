#' @title fnc_prepTabV1.
#' @description \code{prepTabV1} will perform statistics on imported validation
#'    data.
#' @details tbd.
#' @param tab The imported V data.
#' @param a Analyte name.
#' @param alpha Probability of error.
#' @param k result uncertainty.
#' @param flt_outliers Logical. Shall outliers, determined via an F-test on
#'     the highest residual, be removed from the analysis.
#' @param unit_cali Character.
#' @param unit_smpl Character.
#' @param conv_fac Numeric.
#' @examples
#' inp <- system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx")
#' tab <- eCerto:::read_Vdata(file = inp)
#' eCerto:::prepTabV1(tab = tab, a = "PFOA", alpha = 0.01)
#' plyr::ldply(levels(tab[,"Analyte"]), function(a) {
#'     eCerto:::prepTabV1(tab = tab, a = a)
#' })
#' @return A data frame with attributes.
#' @keywords internal
#' @noRd
prepTabV1 <- function(tab = NULL, a = NULL, alpha = 0.05, k = 3, flt_outliers = FALSE, unit_cali = "", unit_smpl = "", conv_fac = 1) {

  e_msg("Preparing Tab.V1 (statistics) from imported data")

  if (is.null(a)) a <- levels(factor(tab[,"Analyte"]))
  stopifnot(all(a %in% levels(factor(tab[,"Analyte"]))))

  alpha <- as.numeric(alpha)
  k <- as.numeric(k)

  plyr::ldply(a, function(a) {

    # extract the data
    l <- levels(tab[,"Level"])
    flt <- tab[,"Analyte"]==a & tab[,"Level"] %in% l
    df <- do.call(rbind, unname(lapply(split(tab[flt,], factor(tab[flt,"Level"])), function(x) {
      data.frame("Conc" = mean(x[, "Concentration"]), "Area_norm" = mean(x[, "norm"], na.rm=TRUE), "n" = sum(is.finite(x[, "norm"])), row.names = unique(as.character(x[,"Level"])))
    })))

    # fit the linear model
    df.lm <- stats::lm(Area_norm ~ Conc, data = df)

    # F-test for outliers (checking highest residual)
    idx <- F_test_outlier(df.lm, alpha = alpha)
    if (!is.na(idx)) {
      check_more <- TRUE
      while (check_more) {
        # $$JL, comment$$ because residual outliers are tested sequentially, it is
        # not always intuitive from the detailed linearity plot showing the full model
        # why some levels are removed as outliers (they can have a low residual in the
        # full model but become outlier in reduced models)
        new_F_Test <- F_test_outlier(stats::lm(Area_norm ~ Conc, data=df[-idx, ]), alpha = alpha)
        if (!is.na(new_F_Test)) {
          idx <- c(idx, which(rownames(df)==names(new_F_Test)))
          if (length(idx)>=(nrow(df)-3)) check_more <- FALSE
        } else {
          check_more <- FALSE
        }
      }
      F_Test <- paste(rownames(df)[idx], collapse = ", ")
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

    # Residuals of quadratic model (for Mandel test)
    df2 <- cbind(df, "Conc2" = df$Conc^2)
    df.qm <- stats::lm(Area_norm ~ Conc + Conc2, data=df2)
    e.qm <- stats::residuals(df.qm)

    # number of replicates
    n <- min(df[,"n"])
    c_min <- min(df[,"Conc"])
    c_max <- max(df[,"Conc"])
    out <- data.frame(
      "ID" = which(levels(tab[,"Analyte"]) == a),
      "Analyte" = a,
      "N" = N,
      "n" = n,
      "alpha" = alpha,
      "k" = round(1/k, 2),
      "b0" = stats::coef(df.lm)[1],
      "b1" = stats::coef(df.lm)[2],
      "r" = stats::cor(df.lm$fitted.values, df$Area_norm),
      "s_yx" = s_yx,
      "s_x0" = s_x0,
      "V_x0" = V_x0,
      "P_Mandel" = MandelTest(res_lm = e, res_qm = e.qm),
      "P_KS_Res" = stats::ks.test(x = e, y="pnorm", mean=mean(e), sd=stats::sd(e))$p.val,
      "P_Neu_Res" = VonNeumannTest(e, unbiased = FALSE)$p.val,
      "F_Test" = F_Test,
      "LOD" = calc_LOD(x = df$Conc, y = df$Area_norm, alpha = alpha, n = n),
      "LOQ" = calc_LOQ(x = df$Conc, y = df$Area_norm, alpha = alpha, n = n, k = k),
      "c_WR_min" = c_min,
      "c_WR_max" = c_max,
      "unit_cali" = unit_cali,
      "conv_fac" = conv_fac,
      "c_WR_min2" = round(c_min * conv_fac, decimal_count(c_min)),
      "c_WR_max2" = round(c_max * conv_fac, decimal_count(c_max)),
      "unit_smpl" = unit_smpl
    )
    attr(out, "df") <- df
    attr(out, "residuals") <- e
    return(out)

  })
}
