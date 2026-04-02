#' @title fnc_prepTabV2_2.
#' @description \code{prepTabV2_2} will generate Tab.V2_0.
#' @details Grubbs-Test for means and Cochran-Test for sds.
#' @param inp The imported data from the validation2 module.
#' @param q Level.
#' @param prec Number of digits to show in table.
#' @param ... Arguments passed to eCerto:::ft_default().
#' @return A flextable object.
#' @examples
#' inp <- eCerto:::init_V2_data()
#' eCerto:::prepTabV2_2(inp = inp)
#' eCerto:::prepTabV2_2(inp = inp, q = 2, prec = 5, id = "Tab2", caption = "caption")
#' @keywords internal
#' @noRd
prepTabV2_2 <- function(inp, q=1, prec=3, fmt="alpha", ...) {
  if (is.numeric(q) && !(q %in% inp[,"Level"])) q <- unique(inp[,"Level"])[q[1]]
  #tmp <- inp[inp[,"Level"]==q & !is.removed(inp), c("Lab","Value"), drop=FALSE]
  tmp <- inp[inp[,"Level"]==q & !is.removed(inp),]
  mns <- V2_calc_stats(tmp)
  if (any(mns[,"n"]==0)) {
    mns <- mns[mns[,"n"]>0,,drop=FALSE]
    tmp <- tmp[tmp[,"Lab"] %in% rownames(mns),,drop=FALSE]
  }
  out <- cbind(mns, Grubbs(lab_means = mns[, "mean", drop=FALSE], fmt=fmt))
  colnames(tmp) <- gsub("Value", "value", colnames(tmp))
  out <- cbind(out, Cochran(data = tmp, fmt=fmt))
  out <- out[order(out[,"mean"]),]
  colnames(out) <- gsub("1$", "<sub>1</sub>", colnames(out))
  colnames(out) <- gsub("2$", "<sub>2</sub>", colnames(out))
  colnames(out) <- gsub("_h$", "<i> h</i>", colnames(out))
  colnames(out) <- gsub("_k$", "<i> k</i>", colnames(out))
  ft <- ft_default(df = out, ...)
  ft <- flextable::align(x = ft, j = 1:2, align = "center")
  ft <- ft_set_formatter(ft = ft, j_idx = which(colnames(out) %in% c("mean", "sd")), fmt = ft_formatter_fixed_digits, digits = prec)
  ft <- ft_set_formatter(ft = ft, j_idx = grep("Mandel", colnames(out)), fmt = ft_formatter_fixed_digits, digits = 4)
  return(ft)
}
