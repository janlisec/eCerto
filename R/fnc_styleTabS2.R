#' @title fnc_styleTabS2.
#' @description \code{styleTabS2} will style Tab.S2 for pretty output.
#' @details tbd.
#' @param x Output of prepTabS2c.
#' @param num_coef The coefficient to compute the shelf life in month.
#' @param output Return either a formatted Datatable (DT) or flextable (ft) object.
#' @examples
#' d_in <- eCerto:::test_Stability_Arrhenius()
#' tS2a <- eCerto:::prepTabS2a(x = d_in)
#' tS2b <- eCerto:::prepTabS2b(x = tS2a)
#' tS2c <- eCerto:::prepTabS2c(x = tS2a, y = tS2b)
#' eCerto:::styleTabS2(x = tS2c, num_coef = -1.9, output = "ft")
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabS2 <- function(x, num_coef = NULL, output = c("DT", "ft", "ft_HTML")) {
  e_msg("styling Tab.S2")
  output <- match.arg(output)
  # compute t_max
  x[, "t_max"] <- round(num_coef / (-1 * exp(x[, "CI_upper"])))
  # format sub text in header
  colnames(x)[5:10] <- endmod(colnames(x)[5:10], type = "~", fmt = "html")
  colnames(x)[6] <- paste0(gsub(")", "", colnames(x)[6]), ")")
  if (output %in% c("ft", "ft_HTML")) {
    ft <- ft_default(df = x, caption = "Calculation of possible storage time t_max", id = "Tab.S2", output = output, HTML2ft = TRUE)
    for (i in 4:9) ft <- ft_set_formatter(ft, i, ft_formatter_fixed_digits, 6)
    ft <- flextable::align(ft, j = which(!colnames(x) %in% c("")), align = "right", part = "all")
    return(ft)
  } else {
    # set up the DT object
    x[,4:9] <- apply(x[,4:9], 2, pn, 6)
    x <- cbind(x, " "="")
    dt <- DT::datatable(
      data = x,
      options = list(
        dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE,
        columnDefs = list(
          list("width" = "50px", "targets" = c(0,3:8)),
          list("width" = "30px", "targets" = c(1,2,9)),
          list(className = "dt-right", targets = 0:(ncol(x) - 1))
        )
      ),
      rownames = NULL, escape = FALSE
    )
    return(dt)
  }
}
