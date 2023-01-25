#'@title fnc_checkHdata.
#'@description \code{checkHdata} will check imported homogeneity data.
#'@details tbd.
#'@param x The Hom data from an session R6 object.
#'@examples
#'x <- eCerto:::test_homog()$data
#'eCerto:::checkHdata(x = x)
#'eCerto:::checkHdata(x = x[,-2])
#'@return A data frame with at least columns 'analyte', 'H_type', 'Flasche' and 'value'.
#'@keywords internal
checkHdata <- function(x) {
  message("[checkHdata] perform statistics on imported homogeneity data")
  # rename if if first column is not named 'analyte' and convert to factor
  colnames(x)[1] <- "analyte"
  x[,"analyte"] <- factor(x[,"analyte"])
  # ensure that there is a second column 'H_type' and convert to factor with at least one level
  if (colnames(x)[2]!="H_type" && colnames(x)[3]=="value") {
    x <- cbind(x[,"analyte",drop=FALSE], data.frame("H_type"=gl(n = 1, k = nrow(x), labels = "hom")), x[,2:ncol(x)])
  } else {
    x[,"H_type"] <- factor(x[,"H_type"])
    # ensure that H_type has one level (because downstream functions fail otherwise)
    if (length(levels(x[,"H_type"]))==0) {
      x[,"H_type"] <- gl(n = 1, k = nrow(x), labels = "hom")
    }
  }
  # ensure that there is a third column 'Flasche' and convert to factor
  colnames(x)[3] <- "Flasche"
  x[,"Flasche"] <- factor(x[,"Flasche"])
  # return checked data
  return(x)
}