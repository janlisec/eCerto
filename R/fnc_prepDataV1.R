#' @title fnc_prepDataV1.
#' @description \code{prepDataV1} will perform statistics on imported stability
#'    data.
#' @details tbd.
#' @param tab The imported V data.
#' @param a Analyte name.
#' @param l Level name.
#' @param fmt Export format of the data.
#' @examples
#' x <- eCerto:::test_Stability_Excel()
#' eCerto:::prepTabS1(x = x)
#' @return A data frame.
#' @keywords internal
#' @noRd
prepDataV1 <- function(tab = NULL, a = NULL, l = NULL, fmt = c("raw", "norm", "rel_norm")) {
  fmt <- match.arg(fmt)
  stopifnot(all(c("Analyte","Area_Analyte","Area_IS","Level") %in% colnames(tab)))
  if (is.null(a)) {
    a <- switch(
      fmt,
      "raw" = levels(tab[,"Analyte"])[1],
      "norm" = levels(tab[,"Analyte"])[1],
      "rel_norm" = levels(tab[,"Analyte"])
    )
  }
  stopifnot(all(a %in% levels(tab[,"Analyte"])))
  if (is.null(l)) {
    l <- switch(
      fmt,
      "raw" = levels(tab[,"Level"]),
      "norm" = levels(tab[,"Level"]),
      "rel_norm" = {
        l <- levels(tab[,"Level"])
        l[c(1,length(l))]
      }
    )
  }
  stopifnot(all(l %in% tab[,"Level"]))
  tab_analyte <- split(tab, tab[,"Analyte"])
  out <- lapply(tab_analyte[a], function(x) {
    tab_level <- split(x, x[,"Level"])
    lapply(tab_level[l], function (y) {
      switch(
        fmt,
        "raw" = y[,"Area_Analyte"],
        "norm" = y[,"Area_Analyte"]/y[,"Area_IS"],
        "rel_norm" = {
          ratio <- y[,"Area_Analyte"]/y[,"Area_IS"]
          ratio/mean(ratio, na.rm=TRUE)
        }
      )
    })
  })
  out <- unlist(out, recursive = FALSE)
  attr(out, "Analyte") <- factor(rep(a, each=length(l)), levels=levels(tab[,"Analyte"]))
  attr(out, "Level") <- factor(rep(l, times=length(a)), levels=levels(tab[,"Level"]))
  attr(out, "Concentration") <- as.vector(sapply(tab_analyte[a], function(x) { sapply(split(x, x[,"Level"])[l], function(y) { unique(y[,"Concentration"]) }) }))
  return(out)
}