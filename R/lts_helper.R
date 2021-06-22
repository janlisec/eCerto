#' @title mondf.
#'
#' @description Calculation of number of month since start date.
#'
#' @param x A vector of dates or character in format 'yyyy-mm-dd'.
#' @param d_start A specific start date (if NULL the minimum of x will be used).
#' @param type You may specify 'year' or 'day' instead of month here.
#' @param origin The origin used.
#'
#' @return
#' @export
mondf <- function(x=NULL, d_start=NULL, type=c("year","mon","day")[2], origin="1900-01-01") {
  lt <- as.POSIXlt(as.Date(x, origin=origin))
  if (is.null(d_start)) d_start <- min(lt) else d_start <- as.POSIXlt(as.Date(d_start, origin=origin))
  out <- switch(type,
    "mon" = (lt$year*12 + lt$mon) - (d_start$year*12 + d_start$mon),
    "year" = lt$year - d_start$year,
    "day" = (lt - d_start)/(24*60*60)
  )
  return(out)
}

#' @title plot_lts_data.
#'
#' @description Plots for LTS module.
#'
#' @param x data.
#' @param type type of plot (see return).
#'
#' @return
#' The plot function can be used to return only the calculated LTS value (in month) with type=0. If type=1 or 2 the normal or adjusted LTS plot will be computed additionally.
#'
#' @export
plot_lts_data <- function(x=NULL, type=1) {
  # ensure that data is ordered after time
  x[["val"]] <- x[["val"]][order(x[["val"]][,"Date"]),]

  # get specific data
  vals <- x[["val"]][,"Value"]
  rt <- x[["val"]][,"Date"]
  mon <- mondf(rt)

  # establish linear model
  foo.lm <- lm(vals~mon)
  a <- coef(foo.lm)[1]
  b <- coef(foo.lm)[2]

  # extract relevant values from definition part
  U <- x[["def"]][,"U"]
  mn <- x[["def"]][,"CertVal"]
  ylab <- paste0(x[["def"]][,"KW_Def"]," (",x[["def"]][,"KW"],")"," [",x[["def"]][,"KW_Unit"],"]")
  main <- x[["def"]][,"KW"]
  sub <- x[["def"]][,"U_Def"]

  # correct values by coef estimate
  foo_adj <- vals-(a-mn)
  foo_lts <- ceiling(abs(U/b))

  # color comment datapoints differently
  if ("Comment" %in% colnames(x[["val"]])) com <- x[["val"]][,"Comment"] else com <- rep(NA, nrow(x[["val"]]))

  if (type==1) {
    # generate 'real time window' plot
    plot(
      vals~mon,
      type="n",
      ylim=range(c(vals,mn+c(-1,1)*U), na.rm=T),
      xlab="Month [n]",
      ylab=ylab,
      sub=sub,
      main=main
    )
    axis(side = 3, at = range(mon), labels = rt[c(1,length(rt))])
    abline(foo.lm, lty=2, col=4) # <-- slope
    abline(h=mn+c(-1,0,1)*U, lty=c(2,1,2), col=c(3,2,3))
    points(vals~mon, pch=24, bg=c(grey(0.6),2)[1+!is.na(com)])
  }

  if (type==2) {
    # generate 'fake time window' plot
    plot(
      c(foo_adj,mn+b*foo_lts)~c(mon,foo_lts),
      pch=21,
      bg=c(c(grey(0.6),2)[1+!is.na(com)],4),
      ylim=range(c(foo_adj,mn+b*foo_lts,mn+c(-1,1)*U)),
      xlab="Month [n]",
      ylab=paste(ylab, "adjusted"),
      sub=sub,
      main=paste(main, "(adjusted)")
    )
    # $$ToDo$$ end date estimation is only approximate (based on ~30d/month or precisely on 365/12=30.42)
    axis(side = 3, at = c(0, foo_lts), labels = c(rt[1], rt[1]+foo_lts*30.42))
    abline(lm(foo_adj~mon), lty=2, col=4)
    abline(h=mn+c(-1,0,1)*U, lty=c(2,1,2), col=c(3,2,3))
    text(x=foo_lts, y=mn+b*foo_lts, pos=2, labels = paste("n =",foo_lts))
  }

  names(foo_lts) <- as.character(as.POSIXlt(as.Date(rt[1]+foo_lts*30.42, origin="1900-01-01")))
  invisible(foo_lts)
}

#' @title read_lts_input.
#'
#' @description Read Excel function for Long-term-stability data.
#'
#' @param file Path to Excel File
#' @param simplify Try to simplify list of imported Excel files into data.frame
#'
#' @return
#' @export
read_lts_input <- function(file=NULL, simplify=FALSE) {
  sheets <- openxlsx::getSheetNames(file = file)
  out <- vector("list", length(sheets))
  for (i in 1:length(sheets)) {
    out[[i]][["def"]] <- openxlsx::read.xlsx(xlsxFile = file, sheet = i, startRow = 1, rows = 1:2)
    out[[i]][["val"]] <- openxlsx::read.xlsx(xlsxFile = file, sheet = i, startRow = 4, detectDates=TRUE)
    out[[i]][["val"]] <- cbind(out[[i]][["val"]], "Comment"=NA)
  }
  if (simplify) {
    out <- plyr::ldply(out, function(x) {
      cbind(x[["val"]], x[["def"]])
    })
  }
  return(out)
}