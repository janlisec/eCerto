#'BAMTool
#'Modul: Zertifizierung
#'Scheffe's multiple t-test
#'data : data table
Scheffe <- function(data=NULL) {
  return(data.frame(
    "Scheffe_05"=agricolae::scheffe.test(y = lm(value~Lab, data=data), trt="Lab", alpha = 0.05)$group[levels(data$Lab),"groups"],
    "Scheffe_01"=agricolae::scheffe.test(y = lm(value~Lab, data=data), trt="Lab", alpha = 0.01)$group[levels(data$Lab),"groups"],
    row.names=levels(data$Lab))
  )
}
