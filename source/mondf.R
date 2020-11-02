# helper function
mondf <- function(d_start, d_end) { 
  lt <- as.POSIXlt(as.Date(d_start, origin="1900-01-01"))
  d_start <- lt$year*12 + lt$mon 
  lt <- as.POSIXlt(as.Date(d_end, origin="1900-01-01"))
  d_end <- lt$year*12 + lt$mon 
  return(d_end - d_start )
}