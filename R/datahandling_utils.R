
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
  flt <- apply(subset(x, select=-c(1,2,File)), 1, function(y) {any(is.finite(as.numeric(y)))})
  if (any(flt)) x <- x[which(flt),,drop=F]
  #combine into data frame and return
  analyte <- x[,1]
  unit <- x[,2]
  # drop first (analyte name), second (unit)
  # and File name column before continue
  dat <- subset(x, select=-c(1,2,File))
  # create new data frame
  x <- data.frame(
    "analyte"=factor(rep(analyte,times=ncol(dat)), levels=analyte),
    "replicate"=factor(rep((1:ncol(dat)),each=nrow(dat))),
    "value"=as.numeric(unlist(dat)),
    "unit"=as.character(rep(unit,times=ncol(dat)))
    )

  return(x)
}

load_sheetnames = function(filepath){
  a = lapply(shiny::isolate(filepath), function(x) {
    ext <- tools::file_ext(x)
    if(tolower(ext)=="xlsx"){
      openxlsx::getSheetNames(x)
    } else {
      shinyalert::shinyalert(title = "Wrong Filetype?", text = "Please select an Excel file.", type = "warning")
      return(NULL)
    }
  })
  
  if(length(unique(a))!=1) {
    shinyalert::shinyalert(
      title = "Different sheetnames", 
      text = "Sheet names are different", 
      type = "warning"
    )
  } 
  return(a[[1]])
}


load_excelfiles = function(filepath, sheet) {

  lapply(shiny::isolate(filepath), function(x) {
    ext <- tools::file_ext(x)
    if(ext == "xlsx"){
      tryCatch({
        a = openxlsx::read.xlsx(x, sheet)
        # a$File = rep(x,nrow(a))
      }, error = function(e) {
        stop(safeError(e))
      })
    } else {
      validate("Invalid file; Please upload a .xlsx file")
    }
  })
}

#' #' creates or delete unique sheet name appendix, 
#' #' so that an event is triggered any way even if
#' #' the sheet name has not changed.
#' #'
#' #' @param s 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' sheetname = function(s) {
#'   stopifnot(is.character(s))
#'   if(grepl("--.", s, fixed = TRUE)) {
#'     sub("--.*", "", s)
#'   } else {
#'     paste(s,
#'       floor(runif(1, min=1, max=500)),
#'       sep = "--."
#'     )
#'   }
#' }
  

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

