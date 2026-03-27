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
  n_q <- unique(mns[,"Level"])
  ldply_base(n_q, function(q) {
    x <- mns[mns[,"Level"]==q,]
    x <- x[is.finite(x[,"mean"]),]
    x_n <- nrow(x)
    x_mn <- sum(x[,"n"]*x[,"mean"])/sum(x[,"n"])
    s_r <- sqrt(sum((x[,"n"]-1) * x[,"sd"]^2)/sum(x[,"n"]-1))
    #x_S <- sum((x[,"mean"] - x_mn)^2)/(x_n-1)
    #s_L <- x_S-(s_r/mean(x[,"n"]))
    T1 <- sum(x[,"n"]*x[,"mean"])
    T2 <- sum(x[,"n"]*x[,"mean"]^2)
    T3 <- sum(x[,"n"])
    T4 <- sum(x[,"n"]^2)
    s_L <- sqrt(((T2 * T3 - T1^2)/(T3 * (x_n-1)) - s_r^2)*((T3 * (x_n-1))/(T3^2 - T4)))
    s_R <- sqrt(s_r^2 + s_L^2)
    data.frame(
      "p_j" = x_n,
      "m_j" = x_mn,
      # weighted mean of Lab sd's
      "s_rj" = s_r,
      "s_Rj" = s_R,
      check.names = FALSE
    )
  })
}
