#' calculation month
#'
#' @param d_start
#' @param d_end
#' @noRd
#'
#' @return
#' @export
#'
#' @examples
mondf <- function(d_start, d_end) {
  lt <- as.POSIXlt(as.Date(d_start, origin="1900-01-01"))
  d_start <- lt$year*12 + lt$mon
  lt <- as.POSIXlt(as.Date(d_end, origin="1900-01-01"))
  d_end <- lt$year*12 + lt$mon
  return(d_end - d_start )
}


#' Plots for LTS module
#'
#' @param x data
#' @param type type of plot (either 1 oder 2)
#'
#' @return
#' @export
#'
#' @examples
#' @noRd
plot_lts_data <- function(x=NULL, type=1) {
  # helper function
  mondf <- function(d_start, d_end) {
    lt <- as.POSIXlt(as.Date(d_start, origin="1900-01-01"))
    d_start <- lt$year*12 + lt$mon
    lt <- as.POSIXlt(as.Date(d_end, origin="1900-01-01"))
    d_end <- lt$year*12 + lt$mon
    return(d_end - d_start )
  }

  # get specific data
  vals <- x[["val"]][,"Value"]
  rt <- x[["val"]][,"Date"]
  mon <- sapply(rt, function(x) { mondf(d_start = rt[1], d_end = x) })
  com <- x[["val"]][,"Comment"]

  U <- x[["def"]][,"U"]
  mn <- x[["def"]][,"CertVal"]
  ylab <- paste0(x[["def"]][,"KW_Def"]," (",x[["def"]][,"KW"],")"," [",x[["def"]][,"KW_Unit"],"]")
  main <- x[["def"]][,"KW"]
  sub <- x[["def"]][,"U_Def"]

  # establish linear model
  foo.lm <- lm(vals~mon)
  a <- coef(foo.lm)[1]
  b <- coef(foo.lm)[2]

  # correct values by coef estimate
  foo_adj <- vals-(a-mn)
  foo_lts <- ceiling(abs(U/b))

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
      ylab=paste(ylab,"adjusted"),
      sub=sub,
      main=main
    )
    # $$ToDo$$ end date estimation is only approximate (based on 30d/month)
    axis(side = 3, at = c(0, foo_lts), labels = c(rt[1],rt[1]+foo_lts*30))
    abline(lm(foo_adj~mon), lty=2, col=4)
    abline(h=mn+c(-1,0,1)*U, lty=c(2,1,2), col=c(3,2,3))
    text(x=foo_lts, y=mn+b*foo_lts, pos=2, labels = paste("n =",foo_lts))
  }

  invisible(foo_lts)
}

#' Read Excel for Long term stability
#'
#' @param file
#' @param simplify
#'
#' @return
#' @export
#'
#' @examples
#' @noRd
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