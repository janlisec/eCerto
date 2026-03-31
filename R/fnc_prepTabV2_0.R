#' @title fnc_prepTabV2_0.
#' @description \code{prepTabV2_0} will generate Tab.V2_0.
#' @details tabulate original data like in DIN Annex B.
#' @param inp The imported data from the validation2 module.
#' @param excl_ids Character vector of Lab IDs to be excluded from the table. Default is NULL.
#' @param ... Arguments passed to eCerto:::ft_default().
#' @return A flextable object.
#' @examples
#' inp <- eCerto:::init_V2_data()
#' eCerto:::prepTabV2_0(inp = inp)
#' eCerto:::prepTabV2_0(inp = inp, excl_ids = sample(inp[,"ID"], 2))
#' eCerto:::prepTabV2_0(inp = inp, id = "Tab1", caption = "caption")
#' @keywords internal
#' @noRd
prepTabV2_0 <- function(inp, excl_ids = NULL, ...) {
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
  if (!is.null(excl_ids) && any(excl_ids %in% inp[,"ID"])) {
    for (id in excl_ids[excl_ids %in% inp[,"ID"]]) {
      l <- which(inp[,"ID"] == id)
      i <- which(tab0[,"Lab"]==inp[l,"Lab"] & tab0[,"Rep"]==inp[l,"Replicate"])
      j <- which(colnames(tab0) %in% inp[l,"Level"])
      ft <- flextable::bg(x = ft, i = i, j = j, bg = "#FF000080")
    }
  }
  ft <- flextable::border_outer(x = ft)
  return(ft)
}
