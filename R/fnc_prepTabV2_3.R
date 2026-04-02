#' @title fnc_prepTabV2_3.
#' @description \code{prepTabV2_3} will generate Tab.V2_3.
#' @details tabulate relevant repeatability values.
#' @param mns The calculated Lab means of the imported data from validation2 module.
#' @return A data frame.
#' @examples
#' inp <- eCerto:::init_V2_data()
#' mns <- eCerto:::V2_calc_stats(inp = inp)
#' eCerto:::prepTabV2_3(mns = mns)
#' @keywords internal
#' @noRd
prepTabV2_3 <- function(mns) {
  qs <- unique(mns[,"Level"])
  ldply_base(qs, function(q) {
    # filter for level and remove non-finite values
    x <- mns[mns[,"Level"]==q,]
    x <- x[is.finite(x[,"mean"]) & is.finite(x[,"sd"]),]

    # compute number and overall (weighted) mean
    np <- nrow(x)
    m_hat <- sum(x[,"n"]*x[,"mean"])/sum(x[,"n"])

    # for n==2
    if (all(x[,"n"]==2)) {
      # this is the formula based calculation
      s_r2 <- (1/(2*np))*sum(x[,"sd"]^2)
      s_L2 <- (1/(np-1))*sum((x[,"mean"]-m_hat)^2)-s_r2/2
      if (s_L2 < 0) s_L2 <- 0
      # this is the T1-T3 based calculation as described in B2 example of 5725-2
      if (FALSE) {
        T1 <- sum(x[,"mean"]-80)
        T2 <- sum((x[,"mean"]-80)^2)
        T3 <- sum(x[,"sd"]^2)
        s_r2 <- T3/(2*np)
        s_L2 <- (np*T2-T1^2)/(np*(np-1))-s_r2/2
      }
    } else {
      # for n>=3
      # this is the formula based calculation
      s_r2 <- sum((x[,"n"]-1) * x[,"sd"]^2)/sum(x[,"n"]-1)
      s_d2 <- (1/(np-1)) * sum(x[,"n"] * (x[,"mean"]-m_hat)^2)
      n_bar <- (1/(np-1)) * (sum(x[,"n"]) - sum(x[,"n"]^2) / sum(x[,"n"]))
      s_L2 <- (s_d2 - s_r2) / n_bar
      # this is the T1-T4 based calculation as described in B2 example of 5725-2
      if (FALSE) {
        T1 <- sum(x[,"n"]*x[,"mean"])
        T2 <- sum(x[,"n"]*x[,"mean"]^2)
        T3 <- sum(x[,"n"])
        T4 <- sum(x[,"n"]^2)
        s_r2 <- sum((x[,"n"]-1) * x[,"sd"]^2)/sum(x[,"n"]-1)
        s_L2 <- ((T2 * T3 - T1^2)/(T3 * (np - 1)) - s_r2) * ((T3 * (np - 1))/(T3^2 - T4))
      }
    }

    return(data.frame(
      "Level_j" = q,
      "p_j" = np,
      "m_j" = m_hat, # overall mean
      "s_rj" = sqrt(s_r2), # weighted mean of Lab sd's
      "s_Rj" = sqrt(s_r2 + s_L2),
      check.names = FALSE
    ))
  })
}
