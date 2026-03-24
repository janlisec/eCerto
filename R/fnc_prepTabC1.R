#' @title prepTabC1.
#' @description Compute lab-stats for Tab C1 of C Module.
#' @param dat Table with columns 'Lab' and 'value'.
#' @param precision Rounding precision.
#' @param excl_labs Exclude filtered Labs from table.
#' @param use_tests Character vector of Test names to be included in the output.
#' @param fmt Output value depicted in the table.
#' @examples
#' rv <- eCerto:::test_rv(type = "SR3")
#' shiny::isolate(dat <- rv$c_fltData())
#' shiny::isolate(lab_means <- rv$c_lab_means(data = dat))
#' eCerto:::prepTabC1(dat = dat, lab_means = lab_means)
#' eCerto:::prepTabC1(dat = dat, lab_means = lab_means, fmt = "cval")
#' dat[dat[, "Lab"] == "L13", "L_flt"] <- TRUE
#' shiny::isolate(lab_means <- rv$c_lab_means(data = dat))
#' eCerto:::prepTabC1(dat = dat, lab_means = lab_means, excl_labs = TRUE)
#' eCerto:::prepTabC1(dat = dat, lab_means = lab_means, use_tests = c("Grubbs"))
#' @return A data frame.
#' @keywords internal
#' @importFrom moments skewness kurtosis agostino.test anscombe.test
#' @noRd
prepTabC1 <- function(dat, lab_means, excl_labs = FALSE,
                      use_tests = c("Scheffe", "Dixon", "Grubbs", "Cochran"),
                      fmt = c("alpha", "pval", "cval", "cval05", "cval01")) {
  fmt <- match.arg(fmt)
  if (excl_labs) {
    # remove filtered labs and re-factor column 'Lab'
    L_flt <- unique(as.character(dat[dat[, "L_flt"], "Lab"]))
    dat <- dat[!dat[, "L_flt"], ]
    dat[, "Lab"] <- factor(dat[, "Lab"])
    lab_means <- lab_means[!(rownames(lab_means) %in% L_flt), ]
  }
  # out <- data.frame(
  #   lab_means,
  #   Scheffe(data = dat),
  #   Dixon(lab_means = lab_means, fmt = fmt),
  #   Grubbs(lab_means = lab_means, fmt = fmt),
  #   Cochran(data = dat, fmt = fmt),
  #   stringsAsFactors = FALSE
  # )
  out <- data.frame(
    lab_means,
    stringsAsFactors = FALSE
  )
  if ("Scheffe" %in% use_tests) out <- cbind(out, Scheffe(data = dat))
  if ("Dixon" %in% use_tests) out <- cbind(out, Dixon(lab_means = lab_means, fmt = fmt))
  if ("Grubbs" %in% use_tests) out <- cbind(out, Grubbs(lab_means = lab_means, fmt = fmt))
  if ("Cochran" %in% use_tests) out <- cbind(out, Cochran(data = dat, fmt = fmt))
  return(out[order(out[, "mean"]), ])
}
