
#' Title
#'
#' @param l 
#'
#' @return
#' @export
#'
#' @examples
list2dataframe = function(l) {
  if (requireNamespace("plyr", quietly = TRUE)) {
    plyr::ldply(l, function(x) {
      x[param$start_row():param$end_row(), param$start_col():param$end_col()]
    })
  } else {
    as.data.frame(do.call(cbind, l))
  }
}

#' creates long pivot table
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
laboratory_dataframe = function(x) {
  stopifnot(!is.reactive(x))
  x = as.data.frame(x)
  flt <- apply(x[,-c(1:2)], 1, function(y) {any(is.finite(as.numeric(y)))})
  if (any(flt)) x <- x[which(flt),,drop=F]
  #combine into data frame and return
  analyte <- x[,1]
  unit <- x[,2]
  dat <- x[,-c(1:2)]
  x <- data.frame("analyte"=factor(rep(analyte,times=ncol(dat)), levels=analyte),
                  "replicate"=factor(rep((1:ncol(dat)),each=nrow(dat))),
                  "value"=as.numeric(unlist(dat)),
                  #"value"=unlist(dat),
                  "unit"=as.character(rep(unit,times=ncol(dat))))
  
  
}


#' getter function for element of the main list
#'
#' @param c 
#' @param m 
#'
#' @return
#' @export
#'
#' @examples
get_listelem = function(c, m) {
  
  c[[m]]
}

#' setter function for an element in the list
#'
#' @param c 
#' @param m 
#' @param elem
#'
#' @return
#' @export
#'
#' @examples
set_listelem = function(c, m, elem) {
  if(!is.null(c[[m]])) stop(paste0(m, " in list is not null"))
  
  ?stop
  if(is.reactive(elem)) {
    c[[m]] = isolate(elem())
  } else {
    c[[m]] = elem
  }
  
}
