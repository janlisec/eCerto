#'@title fnc_styleTabH1.
#'@description \code{styleTabH1} will style Tab.H1 for pretty output.
#'@details tbd.
#'@param x The Hom data from an session R6 object.
#'@param mt The mt from an session R6 object.
#'@param apm The apm from an session R6 object.
#'@examples
#'x <- eCerto:::prepTabH1(x = eCerto:::test_homog()$data)
#'x
#'eCerto:::styleTabH1(x = x)
#'mt <- data.frame("analyte"="Fe")
#'eCerto:::styleTabH1(x = x, mt = mt)
#'apm <- list("Fe"=list("precision"=2))
#'eCerto:::styleTabH1(x = x, apm = apm)
#'@return A data frame.
#'@keywords internal
styleTabH1 <- function(x, mt = NULL, apm = NULL) {
  message("[styleTabH1] styling Tab.H1")
  style_x <- x
  for (i in 1:nrow(style_x)) {
    an <- as.character(style_x[i,"analyte"])
    style_x[i,"mean"] <- pn(as.numeric(style_x[i,"mean"]), ifelse(an %in% names(apm), apm[[an]][["precision"]], 4))
  }
  # round the following columns with fixed precision of 4 digits
  for (cn in c("M_between","M_within","P","s_bb","s_bb_min")) {
    style_x[,cn] <- pn(style_x[,cn], 4)
  }
  # check if analyte is present in C modul
  if (!is.null(mt)) {
    style_x[,"style_analyte"] <- sapply(style_x[,"analyte"], function(x) {
      ifelse(x %in% mt[,"analyte"], "black", "red")
    })
  } else {
    style_x[,"style_analyte"] <- "red"
  }
  style_x[,"style_s_bb"] <- c("bold","normal")[1+as.numeric(style_x[,"s_bb"]<style_x[,"s_bb_min"])]
  style_x[,"style_s_bb_min"] <- c("bold","normal")[1+as.numeric(style_x[,"s_bb"]>=style_x[,"s_bb_min"])]
  return(style_x)
}