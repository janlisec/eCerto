#' @title fnc_prepTabV2_1.
#' @description \code{prepTabV2_1} will generate Tab.V2_1.
#' @details table of means and sds.
#' @param mns The calculated Lab means of the imported data from validation2 module.
#' @param prec Number of digits to show in table.
#' @param ... Arguments passed to eCerto:::ft_default().
#' @return A flextable object.
#' @examples
#' inp <- eCerto:::init_V2_data()
#' mns <- eCerto:::V2_calc_stats(inp = inp)
#' eCerto:::prepTabV2_1(mns = mns)
#' eCerto:::prepTabV2_1(mns = mns, prec = 5, id = "Tab2", caption = "caption")
#' @keywords internal
#' @noRd
prepTabV2_1 <- function(mns, prec = 3, ...) {
  n_p <- length(unique(mns[,"Lab"]))
  n_q <- length(unique(mns[,"Level"]))
  mns_print <- as.data.frame(matrix(NA, nrow = n_p, ncol = 3*n_q, dimnames = list(unique(sort(mns[,"Lab"])), paste0(c("y<sub>i", "s<sub>i", "n<sub>i"), rep(1:n_q, each=3), "</sub>"))))
  for (p in unique(sort(mns[,"Lab"]))) {
    for (q in unique(sort(mns[,"Level"]))) {
      j <- which(unique(sort(mns[,"Level"]))==q)
      mns_print[p,3*j-c(2,1,0)] <- mns[mns[,"Lab"]==p & mns[,"Level"]==q,c("mean","sd","n")]
    }
  }
  mns_print <- cbind("Lab <i>i</i>"=rownames(mns_print), mns_print)
  ft <- ft_default(df = mns_print, ...)
  ft <- flextable::add_header_row(x = ft, values = c("", 1:n_q), colwidths = c(1,rep(3,n_q)))
  ft <- flextable::compose(x = ft, i = 1, j = 1, value = flextable::as_paragraph("Level ", flextable::as_i("j")), part = "header")
  ft <- ft_set_formatter(ft = ft, j_idx = which(substr(ft$col_keys,1,1)%in%c("y","s")), fmt = ft_formatter_fixed_digits, digits = prec)
  ft <- flextable::align(x = ft, i = 1, align = "center", part = "header")
  ft <- flextable::align(x = ft, j = 1, align = "center", part = "all")
  ft <- flextable::vline(x = ft, j = 1+c(0,cumsum(rep(3,n_q))))
  ft <- flextable::border_outer(x = ft)
  return(ft)
}
