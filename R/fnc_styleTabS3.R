#' @title fnc_styleTabS3.
#' @description \code{styleTabS3} will style Tab.S3 for pretty output.
#' @details tbd.
#' @param x Output of prepTabS2b.
#' @param output Return either a formatted Datatable (DT) or flextable (ft) object.
#' @examples
#' d_in <- eCerto:::test_Stability_Arrhenius()
#' tS2a <- eCerto:::prepTabS2a(x = d_in)
#' tS2b <- eCerto:::prepTabS2b(x = tS2a)
#' eCerto:::styleTabS3(x = tS2b, output = "DT")
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabS3 <- function(x, output = c("DT", "ft", "ft_HTML")) {
  e_msg("styling Tab.S3")
  output <- match.arg(output)
  # format sub text in header
  colnames(x)[1] <- "sum(x)"
  colnames(x)[2] <- "sum(x<sup>2</sup>)"
  if (output %in% c("ft", "ft_HTML")) {
    ft <- ft_default(df = x, output = output, HTML2ft = TRUE)
    for (i in c(1:2,4:7)) ft <- ft_set_formatter(ft, i, ft_formatter_fixed_digits, 6)
    return(ft)
  } else {
    # set up the DT object
    x[,-3] <- apply(x[,-3], 2, pn, 6)
    x <- cbind(x, " "="")
    dt <- DT::datatable(
      data = x,
      options = list(
        dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE,
        columnDefs = list(
          list("width" = "50px", "targets" = 0:(ncol(x)-2)),
          list(className = "dt-right", targets = 0:(ncol(x) - 1))
        )
      ),
      rownames = NULL, escape = FALSE
    )
    return(dt)
  }
}
