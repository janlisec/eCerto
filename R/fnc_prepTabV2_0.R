#' @title fnc_prepTabV2_0.
#' @description \code{prepTabV2_0} will generate Tab.V2_0.
#' @details tabulate original data like in DIN Annex B.
#' @param inp The imported data from the validation2 module.
#' @param ... Arguments passed to eCerto:::ft_default().
#' @return A flextable object.
#' @examples
#' inp <- eCerto:::init_V2_data()
#' eCerto:::prepTabV2_0(inp = inp)
#' eCerto:::prepTabV2_0(inp = inp, id = "Tab1", caption = "caption")
#' @keywords internal
#' @noRd
prepTabV2_0 <- function(inp, ...) {
  nms_level <- unique(inp[,"Level"])
  tab0 <- ldply_base(unique(inp[,"Lab"]), function(p) {
    x <- inp[inp[,"Lab"]==p,]
    ldply_base(unique(sort(x[,"Replicate"])), function(k) {
      y <- x[x[,"Replicate"]==k,]
      out <- data.frame(t((stats::setNames(rep(NA, length(nms_level)), nms_level))), check.names = FALSE)
      out[,y[,"Level"]] <- y[,"Value"]
      cbind("Lab"=p, "Rep"=k, out)
    })
  })
  ft <- ft_default(df = tab0, ...)
  ft <- flextable::vline(x = ft, j = 1:ncol(tab0), part = "all")
  ft <- flextable::hline(x = ft, i = (which(!duplicated(tab0[,"Lab"]))-1)[-1])
  ft <- flextable::align(x = ft, j = 1:2, align = "center")
  ft <- flextable::border_outer(x = ft)
  return(ft)
}
