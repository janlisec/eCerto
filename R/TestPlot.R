#'BAMTool
#'Modul: Zertifizierung
#'Test Plot
#'x : data table
#'
#'
#' @param data data.frame with ID, Lab, analyte, replicate, value, unit, S_flt, L_flt
TestPlot <- function(data = NULL) {
  data[, "Lab"] <-
    factor(data[, "Lab"], levels = names(sort(sapply(
      split(data[, "value"], data[, "Lab"]), mean
    ))))
  graphics::plot(
    value ~ Lab,
    data = data,
    main = "",
    ylab = paste0(unique(data[, "analyte"])[1], " [", unique(data[, "unit"])[1], "]")
  )
  graphics::text(
    x = jitter(as.numeric(data[, "Lab"])),
    y = data[, "value"],
    labels = data[, "ID"],
    col = 4
  )
}
