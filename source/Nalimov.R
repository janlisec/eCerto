#'BAMTool
#'Modul: Zertifizierung
#'Nalimov test
#'lab_means : data table
Nalimov <- function(lab_means=NULL) {
  nalimov_crit <- structure(list(f = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 25L, 30L, 35L, 40L, 45L, 50L, 100L, 200L, 300L, 400L, 500L, 600L, 700L, 800L, 1000L), 
                                 a_05 = c(1.409, 1.645, 1.757, 1.814, 1.848, 1.87, 1.885, 1.895, 1.903, 1.91, 1.916, 1.92, 1.923, 1.926, 1.928, 1.931, 1.933, 1.935, 1.936, 1.937, 1.942, 1.945, 1.948, 1.949, 1.95, 1.951, 1.956, 1.958, 1.958, 1.959, 1.959, 1.959, 1.959, 1.959, 1.96), 
                                 a_01 = c(1.414, 1.715, 1.918, 2.051, 2.142, 2.208, 2.256, 2.294, 2.324, 2.348, 2.368, 2.385, 2.399, 2.412, 2.423, 2.432, 2.44, 2.447, 2.454, 2.46, 2.483, 2.498, 2.509, 2.518, 2.524, 2.529, 2.553, 2.564, 2.566, 2.568, 2.57, 2.571, 2.572, 2.573, 2.576), 
                                 a_001 = c(1.414, 1.73, 1.982, 2.178, 2.329, 2.447, 2.54, 2.616, 2.678, 2.73, 2.774, 2.812, 2.845, 2.874, 2.899, 2.921, 2.941, 2.959, 2.975, 2.99, 3.047, 3.085, 3.113, 3.134, 3.152, 3.166, 3.227, 3.265, 3.271, 3.275, 3.279, 3.281, 3.283, 3.285, 3.291)), 
                            class = "data.frame", row.names = c(NA, -35L))
  nalimov <- function(x, m, s, n) {
    abs((x-m)/s)*sqrt(n/(n-1))
  }
  cval <- sapply(lab_means$mean, function(x) {
    nalimov(x=x, m=mean(lab_means$mean), s=sd(lab_means$mean), n=nrow(lab_means))
  })
  
  return(data.frame(
    "Nalimov"=sapply(cval, function(x) {
      #l <- which.max(nalimov_crit[,"f"]<=(nrow(lab_means)-2))
      l <- max(which(nalimov_crit[,"f"]<=(nrow(lab_means)-2)))
      ifelse(x<nalimov_crit[l,"a_05"], ".", ifelse(x>=nalimov_crit[l,"a_01"], ".01", ".05"))
    }),
    row.names=rownames(lab_means)
    )
  )
}
