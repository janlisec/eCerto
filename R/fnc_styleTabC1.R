#' @title styleTabC1.
#' @description Prepare Tab C1 for HTML.
#' @param x Object `overview_stats_pre()`.
#' @param n Rounding precision for mean/sd columns.
#' @param fmt Output value depicted in the table.
#' @param output Return either a formatted Datatable (DT) or flextable (ft) object.
#' @examples
#' rv <- eCerto:::test_rv(type = "SR3")
#' shiny::isolate(dat <- rv$c_fltData())
#' shiny::isolate(lab_means <- rv$c_lab_means(data = dat))
#' x <- eCerto:::prepTabC1(dat = dat, lab_means = lab_means)
#' eCerto:::styleTabC1(x = x)
#' eCerto:::styleTabC1(x = x, output="ft")
#' @return A data table object.
#' @keywords internal
#' @noRd
styleTabC1 <- function(x, n = 4, fmt = c("alpha", "pval", "cval", "cval05", "cval01"), output = c("DT", "ft")) {
  fmt <- match.arg(fmt)
  output <- match.arg(output)
  if (fmt %in% c("pval", "cval", "cval05", "cval01")) {
    cns <- c("Dixon", "Grubbs1", "Grubbs2", "Cochran")
    cns <- cns[cns %in% colnames(x)]
    if (length(cns) >= 1) x[, cns] <- round(x[, cns], 6)
  }
  nc <- ncol(x)
  colnames(x) <- gsub("_01", "<sub>.01</sub>", colnames(x))
  colnames(x) <- gsub("_05", "<sub>.05</sub>", colnames(x))
  colnames(x) <- gsub("1$", "<sub>1</sub>", colnames(x))
  colnames(x) <- gsub("2$", "<sub>2</sub>", colnames(x))
  if (output == "ft") {
    eCerto_flextable_defaults()
    x[,c("mean","sd")] <- round(x[,c("mean","sd")], n)
    ft <- flextable::flextable(x)
    for (j in grep("<.+>.+</.+>", colnames(x))) {
      ft <- flextable::compose(x = ft, j = j, value = HTML2ft(colnames(x)[j]), part = "header")
    }

    ft <- eCerto_flextable_defaults(ft = ft)
    ft <- flextable::set_caption(ft, caption = flextable::as_paragraph(flextable::as_b("Tab.C1"), " Statistics regarding lab means, lab variances and outlier detection"))
    return(ft)
  } else {
    dt <- DT::datatable(
      # add a fake column to the table to allow horizontal fill
      data = cbind(x, data.frame(" " = " ", check.names = FALSE)),
      options = list(
        dom = "t", pageLength = -1, scrollX = TRUE, ordering = FALSE,
        columnDefs = list(
          list("width" = "80px", targets = c(1:2, 4:(nc - 1))),
          list("width" = "30px", targets = c(0, 3)),
          list("orderable" = "true", targets = 0:2),
          list(className = "dt-right", targets = "_all")
        )
      ),
      selection = "none", rownames = NULL, escape = FALSE,
    )
    dt <- DT::formatCurrency(
      table = dt, columns = c(2, 3), currency = "", digits = n
    )
    return(dt)
  }
}
