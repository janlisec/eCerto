#' @name datahandling_utils
#' @aliases setValue
#' @aliases getValue
#'
#' @title setValue.
#'
#' @description General access to data object (so data object can maybe get changed without that much code edit)
#'
#' @param df The data frame (an R6 object).
#' @param key A character vector specifying the key-chain to put the value in (see examples).
#' @param value Value to set.
#'
#' @return Nothing. The R6 object is updated automatically.
#'
#' @export
#'
#' @rdname datahandling_utils
#' @examples
#' rv <- reactiveClass$new(init_rv())
#' # Only run examples in interactive R sessions
#' if (interactive()) {
#'  setValue(rv, c("Certifications","data"), 5)
#'  getValue(rv, c("Certifications","data")) # is 5?
#'  setValue(rv, c("General","user"),"Franz")
#'  getValue(rv, c("General","user"))
#' }
setValue = function(df,key,value){

  if(R6::is.R6(df)){
    df$set(key, value) # in reactiveClass.R
  } else {
    stop("object of class ", class(df), " can't get set currently.")
  }
}


#' @title getValue.
#'
#' @description Returns element. If 'key' is used, reactivity not working correctly.
#' Preferable way for calling `getValue(df, key)`, see example
#'
#' @param df An object of class R6.
#' @param key Key value within R6 object 'df'.
#'
#' @return Value of 'key' from 'df'.
#'
#' @export
#'
#' @rdname datahandling_utils
#' @examples
#' datreturn <- ecerto:::test_datreturn()
#' shiny::isolate(ecerto::getValue(datreturn, "t_H"))
getValue = function(df, key=NULL) {
  if(R6::is.R6(df)){
    return(df$get(key))
  } else if(is.list(df)){
    return(df[[key]])
  } else {
    stop("object of class ", class(df), " can't get/set currently.")
  }
}


#' @title creates long pivot table in laboratory style after load
#'
#' @param x data frame with uploaded excel table
#'
#' @return another data frame with extracted laboratory parameters
#' @rdname datahandling_utils
#' @export
laboratory_dataframe = function(x) {
  stopifnot(!shiny::is.reactive(x))

  x2 = as.data.frame(x)
  x2_sub =  x2[,!names(x2) %in% c(names(x2[,c(1,2)]),"Species","File")]
  flt <- apply(x2_sub, 1, function(y) {any(is.finite(as.numeric(y)))})
  if (any(flt)) x2 <- x2[which(flt),,drop=F]
  #combine into data frame and return
  analyte <- x2[,1]
  unit <- x2[,2]
  dat = x2[,!names(x2) %in% c(names(x2[,c(1,2)]),"Species","File")]
  # drop first (analyte name), second (unit)
  # and File name column before continue;
  # create new data frame
  x2 <- data.frame(
    "analyte"=factor(rep(analyte,times=ncol(dat)),levels=analyte),
    "replicate"=factor(rep((1:ncol(dat)),each=nrow(dat))),
    "value"=as.numeric(unlist(dat)),
    "unit"=as.character(rep(unit,times=ncol(dat)))
    )

  return(x2)
  
  
}

#' Loads names of Excel sheets
#'
#' @param filepath the path to a single or multiple excel file(s)
#'
#' @return the names of sheets
#' @rdname datahandling_utils
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

#' Crops dataframe(s)
#'
#' @param dfs list of dataframe(s)
#' @param cols columns as array, e.g. 1:3
#' @param rows rows as array, e.g. 5:8
#'
#' @return cropped list of data frames(s)
#'
#' @rdname datahandling_utils
#' @examples ecerto:::crop_dataframes(iris,2:3,5:6)
crop_dataframes <- function(dfs, cols, rows) {
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
    warning("data frame is not a list")
    dfs <- list(dfs)
  }

  r <- lapply(dfs, function(y) {
    y[rows,cols]
  })
  return(r)
}



#' set source of upload for an element
#'
#' @param rv the list
#' @param m one of "Certification","Homogeneity", "Stability"
#' @param uploadsource the source of upload
#'
#' @return nothing directly, but via rv
#' @export
#' @rdname datahandling_utils
#' @examples
#' if (interactive()) {
#'   rv = init_rv()
#'   set_uploadsource(rv,"Certification","Excel")
#' }

set_uploadsource = function(rv, m, uploadsource) {
  stopifnot(is.character(uploadsource)) # only character
  stopifnot(uploadsource %in% c("RData","Excel"))
  setValue(rv,c(m,"uploadsource"),uploadsource)
  # rv$set(c(m,"uploadsource"),uploadsource)
  # rv[[m]][["uploadsource"]] = uploadsource

}



#' Rounds material table.
#'
#'
#' @param value the value to be rounded
#' @param precision precision value
#'
#' @return the rounded value
#'
#' @rdname datahandling_utils
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
#' @rdname datahandling_utils
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
#' @rdname datahandling_utils
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

#' @keywords internal
#' to switch to Start Page
to_startPage = function(session, value="Certification") {
  shiny::updateNavbarPage(
    session = session,
    inputId = "navbarpage",
    selected = "Start"
  )
  shiny::updateSelectInput(
    session = session,
    inputId = "moduleSelect",
    selected = value
  )
}
