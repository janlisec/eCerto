# format a number by rounding to a precision 
pn <- function(n=NULL, p=4L) {
# n : numeric vector
# p : precision after the decimal sign
# output : numbers formatted in same width as character using scientific notation for numbers < precision and rounding to precision otherwise
  if (any(is.finite(n))) {
    w <- max(nchar(round(n)))+p+1 # determine maximum width required
    o <- sprintf(paste0("%*.", p, "f"), w, n)
    s <- round(n,p)==0 # requires scientific notation
    if (any(s)) o[which(s)] <- sprintf(paste0("%*.", max(c(p-4,1)), "E"), w, n[which(s)])
    return(o)
  } else {
    return(n)
  }
}
# pn(n=pi*10^(-1*(-3:6)))