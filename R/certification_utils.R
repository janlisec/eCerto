
#' transforms a list to a data frame
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

#' creates long pivot table in laboratory style
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
#' @noRd
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



#' Returns the "data" element of the current "god list" element
#'
#' @param d 
#'
#' @return
#' @export
#'
#' @examples
data_of_godelement = function(d) {
  d[["data"]]
}

#' getter function for element of the "god list"
#'
#' @param c "god list
#' @param m element to be fetched (e.g. "Certifications")
#'
#' @return
#' @export
#'
#' @examples
get_listelem = function(c, m) {
  
  data_of_godelement(c[[m]])
}

#' setter function for an element in the "god list"
#'
#' @param c "god list
#' @param m element to be fed (e.g. "Certifications")
#' @param dat data
#'
#' @return
#' @export
#'
#' @examples
set_listelem = function(c, m, dat) {
  
  # if(!is.null(c[[m]])) 
  #   stop(paste0(m, " in list is not null"))
  # 
  # if(is.reactive(dat)) {
  #   c[[m]] = isolate(dat())
  # } else {
  #   c[[m]] = dat
  # }
  
  if(!is.null(get_listelem(c,m))) 
    stop(paste0(m, " in list is not null"))
  
  if(is.reactive(dat)) {
    c[[m]][["data"]] = isolate(dat())
  } else {
    c[[m]][["data"]] = dat
  }
  
}


#' set source of upload for an element
#' 
#'
#' @param c 
#' @param m 
#' @param uploadsource 
#'
#' @return
#' @export
#'
#' @examples
set_listUploadsource = function(c, m, uploadsource) {
  stopifnot(is.character(uploadsource)) # only character
  stopifnot(uploadsource %in% c("RData","Excel"))
  
  c[[m]][["uploadsource"]] = uploadsource
  
}


#' get source of upload for an element
#'
#' @param c 
#' @param m 
#'
#' @return
#' @export
#'
#' @examples
get_listUploadsource = function(c, m) {
  
  uploadsource_of_element(c[[m]])
  
}

#' Returns source of upload for an element
#'
#' @param d 
#'
#' @return
#' @export
#'
#' @examples
uploadsource_of_element = function(d) {
  d[["uploadsource"]]
}

