#' General access to data object (so data object can maybe get changed without that much code edit)
#'
#' @param df the data frame (e.g. a R6 object)
#' @param key key(s)
#' @param value value to set
#'
#' @return nothing
#' @export
#'
#' @examples
#' rv = reactiveClass$new(init_rv())
#' setValue(rv,c("Certifications","data"),5)
#' getValue(rv,c("Certifications","data")) # is 5?
setValue = function(df,key,value){
  if(R6::is.R6(df)){
    df$set(key,value)
  } else {
    stop("object of class ", class(df), " can't get set currently.")
  }
}


#' Returns element. If 'key' is used, reactivity not working correctly.
#' Preferable way for calling `getValue(df)$key`, see example
#'
#' @param df 
#' @param key key, see notes
#' @param react should
#'
#' @return
#' @export
#'
#' @examples
#' datreturn = test_datreturn()
#' isolate(getValue(datreturn)$t_H)
getValue = function(df, key=NULL, reactiveReturn = FALSE) {
  if(reactiveReturn==TRUE) {
    stop("reactiveReturn is still under testing")
  }
  if(R6::is.R6(df)){
    if(reactiveReturn){
      reactive({df$get(key)})
    } else {
      df$get(key)
    }
    return()
  } else {
    stop("object of class ", class(df), " can't get set currently.")
  }
}


#' creates long pivot table in laboratory style
#'
#' @param x data frame with uploaded excel table
#'
#' @return another data frame with extracted laboratory parameters
#' @export
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

#' Extracts values from nested list
#'
#' @param l the list object
#' @param keys a vector of keys
#'
#' @return the extracted value
#' @export
#'
#' @examples
#' lz = list(a1=list(b1 = "Streusalz",b2 = "Andreas Scheuer"), a2 = "Wurst")
#' lz = do.call(shiny::reactiveValues,lz)
#' access_nested_list(lz, c("a1","b2")) # should be "Andreas Scheuer"
access_nested_list = function(l,keys) {
  if(!is.null(keys)){
    if(shiny::is.reactivevalues(l)) {
      shiny::isolate(purrr::chuck(l, !!!keys))
    } else {
      purrr::chuck(l, !!!keys)
    }
  } else {
    l
  }
}

#' [similar to access_nested_list()]][ecerto::access_nested_list]
#' @export
set_nested_list = function(l,keys,value) {
  
  if(!is.null(keys)){
    if(shiny::is.reactivevalues(l)) {
      access_nested_list(l, keys)
      shiny::isolate(purrr::pluck(l, !!!keys) <- value)
    } else {
      purrr::pluck(l, !!!keys) <- value
    }
  } else {
    l
  }
  
}

#' Loads names of Excel sheets
#'
#' @param filepath the path to a single or multiple excel file(s)
#'
#' @return the names of sheets
#' @export
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


# load_excelfiles = function(filepath, sheet) {
#
#   lapply(filepath, function(x) {
#     ext <- tools::file_ext(x)
#     if(ext == "xlsx"){
#       tryCatch({
#         a = openxlsx::read.xlsx(x, sheet)
#         # a$File = rep(x,nrow(a))
#       }, error = function(e) {
#         stop(safeError(e))
#       }, warning = function(w){        # Specifying warning message
#         # message("There was a warning message.")
#       })
#     } else {
#       validate("Invalid file; Please upload a .xlsx file")
#     }
#   })
# }

#' Crops dataframe(s)
#'
#' @param dfs list of dataframe(s)
#' @param cols columns as array, e.g. 1:3
#' @param rows rows as array, e.g. 5:8
#'
#' @return cropped list of data frames(s)
#'
#' @examples crop_dataframes(iris,2:3,5:6)
crop_dataframes = function(dfs,cols,rows) {
  if(missing(dfs))
    stop("list of dataframes missing")
  if(missing(cols))
    stop("Need to specify columns")
  if(missing(rows))
    stop("Need to specify rows")
  if(length(cols) == 1 | length(rows) == 1)
    stop("length of rows and columns is one")
  if(cols[2]<cols[1] | rows[2] < rows[1])
    stop("order of elements wrong")
  if(!is.numeric(cols) | !is.numeric(rows))
    stop("rows and column index are not numerics")

  if(!inherits(dfs,"list")){
    browser()
    warning("data frame is not a list")
    dfs = list(dfs)
  }

  r = lapply(dfs, function(y) {
    y[rows,cols]
  })
  return(r)
}


#' Returns the "data" element of the current "god list" element
#'
#' @param l list, which contains "data", but also e.g. "source"
#'
#' @return
#' @export
data_of_godelement = function(l) {
  stopifnot(!is.reactive(l)) # d shouldn't be a reactive
  l[["data"]]
}

#' #' getter function for module element of the "god list"
#' #' modules so far are "Certifications, Homogeneity, Stability"
#' #'
#' #' @param c "god list
#' #' @param m element to be fetched (e.g. "Certifications")
#' #'
#' #' @return
#' #' @export
#' get_listelem = function(c, m) {
#'   .Deprecated("getValue")
#'   data_of_godelement(c[[m]])
#' }

#' #' setter function for an element in the "god list"
#' #'
#' #' @param rv "god list"
#' #' @param m element to be fed (e.g. "Certifications")
#' #' @param dat data to be inserted
#' #'
#' #' @return
#' #' @export
#' set_listelem = function(rv, m, dat) {
#'   # if(!is.null(get_listelem(c,m))) {
#'   #   warning(paste0(m, " in list is not null"))
#'   #   return(NULL)
#'   # }
#'   .Deprecated("setValue")
#' 
#'   if(is.reactive(dat)) {
#'     rv$set(c(m,"data"),isolate(dat()))
#'     # c[[m]][["data"]] = isolate(dat())
#'   } else {
#'     rv$set(c(m,"data"),dat)
#'     # c[[m]][["data"]] = dat
#'   }
#' 
#' }


#' set source of upload for an element
#'
#' @param rv the list
#' @param m one of "Certification","Homogeneity", "Stability"
#' @param uploadsource the source of upload
#'
#' @return nothing directly, but via rv
#' @export
#' @examples
#' ## Not run: 
#  # From within a reactive context, you can access values with:
#' rv = init_rv()
#' set_listUploadsource(rv,"Certification","Excel")

set_listUploadsource = function(rv, m, uploadsource) {
  stopifnot(is.character(uploadsource)) # only character
  stopifnot(uploadsource %in% c("RData","Excel"))
  setValue(rv,c(m,"uploadsource"),uploadsource)
  # rv$set(c(m,"uploadsource"),uploadsource)
  # rv[[m]][["uploadsource"]] = uploadsource

}


#' get source of upload for an element
#'
#' @param rv the list
#' @param m "Certification", etc.
#'
#' @return
#' @export

get_listUploadsource = function(rv, m) {
  getValue(rv,c(m,"uploadsource"))
  # rv$get(c(m,"uploadsource"))
  # uploadsource_of_element(c[[m]])

}

#' Returns source of upload for an element, is internally used by \code{get_listUploadsource}
#' (outdated)
#' @param d
#'
#' @return
#' @noRd
uploadsource_of_element = function(d) {
  d[["uploadsource"]]
}


#' Rounds material table.
#' 
#'
#' @param value the value to be rounded
#' @param precision precision value
#'
#' @return the rounded value
#' @export
#' @examples roundMT(34.3434,3)
roundMT = function(value,precision = NULL) {
  if(is.null(precision)) return(value)
  round(value,precision)
}


#' format a number by rounding to a precision in same width as character using
#' scientific notation for numbers < precision and rounding to precision
#' otherwise
#'
#' @param n numeric vector
#' @param p precision after the decimal sign
#'
#' @return numbers formatted
#' @export
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

#' Update single or multiple cells of the final materialtabelle (formerly cert_vals)
#' with new values
#'
#' @param r the reactive containing the data frame to be updated
#' @param colname name of the column
#' @param analyterow  name of the analyte-row. If NULL, whole column is updated
#' @param value value to be updated
#'
#' @return nothing directly
#' @export
update_reactivecell = function(r,colname,analyterow = NULL,value) {

  if(!is.data.frame(r()))
    stop("r is not a data frame")
  if(!colname %in% colnames(r()))
    stop("reactive data frame does not contain column ", colname)
  if(!is.null(analyterow) && nrow(merge(analyterow,r()))==0)
    stop("reactive data frame does not contain row ", analyterow)
  if(is.data.frame(value))
    stop("value is a dataframe, but should be a scalar")
  if(!is.null(analyterow) && length(value)>1) {
    warning("value to be inserted is not scalar, i.e. more than one. Take only first!")
    value = value[1]
  }

  # extract original row to be edit into variable (1/3)
  df = r()
  if(is.null(analyterow)){
    newRow = df
  } else {
    newRow = df[df[["analyte"]]==analyterow,]
  }

  # edit cell (2/3)
  newRow[[colname]] = value

  # update (3/3)
  if(is.null(analyterow)){
    df = newRow
  } else {
    df[df[["analyte"]]==analyterow,] = newRow
  }

  r(df)
}
