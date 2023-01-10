#' @name app_utils
#' @aliases setValue
#' @aliases getValue
#'
#' @title setValue.
#' @description General access to data object (so data object can maybe get
#'     changed without that much code edit)
#' @param df The data frame (an R6 object).
#' @param key A character vector specifying the key-chain to put the value in (see examples).
#' @param value Value to set.
#' @return Nothing. The R6 object is updated automatically.
#' @export
#' @rdname app_utils
#' @examples
#' # Only run examples in interactive R sessions
#' if (interactive()) {
#'   rv <- eCerto$new(init_rv())
#'   setValue(rv, c("Certification","data"), 5)
#'   getValue(rv, c("Certification","data")) # is 5?
#'   setValue(rv, c("General","user"),"Franz")
#'   getValue(rv, c("General","user"))
#' }
setValue <- function(df, key, value){
  if(R6::is.R6(df)){
    df$set(key, value) # in eCerto.R
  } else {
    stop("Object of class ", class(df), " can't set value currently.")
  }
}

#' @title getValue.
#' @description Returns element. If 'key' is used, reactivity not working correctly.
#' Preferable way for calling `getValue(df, key)`, see example
#' @param df An object of class R6.
#' @param key Key value within R6 object 'df'.
#' @return Value of 'key' from 'df'.
#' @export
#' @rdname app_utils
getValue <- function(df, key = NULL) {
  if (R6::is.R6(df)) {
    return(df$get(key))
  } else if (is.list(df)) {
    return(df[[key]])
  } else {
    stop("Object of class ", class(df), " can't get value currently.")
  }
}

#' @title xlsxSheetNames.
#' @description Loads names of Excel sheets.
#' @param filepath the path to a single or multiple excel file(s)
#' @return the names of sheets
#' @importFrom openxlsx getSheetNames
#' @importFrom shinyalert shinyalert
#' @importFrom tools file_ext
#' @noRd
#' @keywords internal
xlsxSheetNames <- function(filepath) {
  a <- lapply(shiny::isolate(filepath), function(x) {
    ext <- tools::file_ext(x)
    if (tolower(ext) == "xlsx") {
      openxlsx::getSheetNames(x)
    } else {
      if (is.null(shiny::getDefaultReactiveDomain())) {
        stop("Please select only Excel (.xlsx) files.")
      } else {
        shiny::showModal(
          shiny::modalDialog("Please select only Excel (.xlsx) files.", title = "Wrong Filetype?")
        )
      }
      return(NULL)
    }
  })
  if (length(unique(a)) != 1) {
    if (is.null(shiny::getDefaultReactiveDomain())) {
      stop("Sheet names are different within files.")
    } else {
      shiny::showModal(
        shiny::modalDialog("Sheet names are different within files.", title = "Different sheetnames?")
      )
    }
  }
  return(a[[1]])
}

#' @title set_uploadsource.
#' @description Set source of upload for an element. This is kept to trigger a re-focus of the App
#'     to a specific subpage should new data be uploaded for this module.#'
#' @param rv The eCerto object.
#' @param m Name of a module (one of "Certification", "Homogeneity", "Stability").
#' @param uploadsource The source of upload (either "RData" or "Excel").#'
#' @return Nothing. Will update the eCerto object field 'uploadsource'.
#' @keywords internal
#' @noRd
#' @examples
#' if (interactive()) {
#'   rv <- eCerto:::init_rv()
#'   set_uploadsource(rv, "Certification", "Excel")
#' }
set_uploadsource <- function(rv, m, uploadsource) {
  stopifnot(is.character(uploadsource)) # only character
  stopifnot(uploadsource %in% c("RData", "Excel"))
  # Has RData been uploaded before?
  if (uploadsource == "RData") {
    u <- getValue(rv, c(m, "uploadsource"))
    if (!is.null(u) && startsWith(u, "RData")) {
      # what number was last upload
      idx <- as.numeric(gsub("RData-", "", u))
    } else {
      # if previous was 'Excel', restart counter
      idx <- 0
    }
    uploadsource <- paste0("RData-", idx + 1)
  }
  setValue(rv, c(m, "uploadsource"), uploadsource)
}

#' @title pn.
#' @description Format a number by rounding to a precision in same width as
#'   character using scientific notation for numbers < precision and rounding
#'   to precision otherwise.
#' @param n numeric vector
#' @param p requested precision after the decimal sign
#' @return numbers formatted
#' @examples
#' pn(n=c(1.23456, NA, 0, 0.00001, -0.00001), p=8)
#' @noRd
#' @keywords internal
pn <- function(n=NULL, p=4L) {
  # n : numeric vector
  # p : precision after the decimal sign
  # output : numbers formatted in identical width as character using scientific notation for numbers < p and rounding to p otherwise
  if (any(is.finite(n))) {
    w <- max(nchar(round(n)), na.rm=TRUE)+p+1 # determine maximum width required
    o <- rep(paste(rep(" ", w), collapse=""), length(n))
    o[is.finite(n)] <- sprintf(paste0("%*.", p, "f"), w, n[is.finite(n)])
    s <- is.finite(n) & round(n,p)==0 & abs(n)>0 # requires scientific notation
    if (any(s)) o[which(s)] <- sprintf(paste0("%*.", max(c(p-4,1)), "E"), w, n[which(s)])
    return(o)
  } else {
    return(n)
  }
}

#' @title update_reactivecell.
#' @description Update single or multiple cells of the final `materialtabelle` with new values.
#' @param r Reactive containing the data frame to be updated.
#' @param colname Name of the column.
#' @param analyterow Name of the analyte-row. If NULL, whole column is updated.
#' @param value New value to be updated.
#' @return Nothing. Will update the data frame in the reactive `r`.
#' @noRd
#' @keywords internal
update_reactivecell = function(r, colname, analyterow = NULL, value) {

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

  # message("reactivecell: Update ",  deparse(substitute(r())), "; column: ", colname)
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

#' @title to_startPage.
#' @description If called, function will redirect the user to page `Start`.
#' @param session session.
#' @param  value value.
#' @keywords internal
#' @noRd
to_startPage = function(session, value="Certification") {
  # this function will break if shiny input IDs get changed
  # ToDo: implement in testthat that 'Start' and 'Start-moduleSelect' are present
  shiny::updateNavbarPage(
    session = session,
    inputId = "navbarpage",
    selected = "Start"
  )
  shiny::updateSelectInput(
    session = session,
    inputId = "Start-excelfile-moduleSelect",
    selected = value
  )
}

#' @title listNames
#' @description Provides names of nested list elements, but ignores data frame column names.
#'     Refer to  <https://stackoverflow.com/q/68453593/6946122> for details.
#' @param l Nested list or R6 containing reactiveValues or reactiveValues
#' @param maxDepth the maximum depth, the names of list should be returned
#' @param split should the returning list be returned as nested  (TRUE) or as point-separated list (FALSE)?#'
#' @return A list if split == TRUE, otherwise a character vector of names.
#' @keywords internal
#' @noRd
#' @examples
#' test <- list(
#'   "b" = list(
#'     "df1" = data.frame(col = c(1, 2)),
#'     "e" = list(z = NULL)
#'    ),
#'   "c" = NULL,
#'   "df2" = data.frame(c12 = c(1, 2), c34 = c(3, 4))
#' )
#' listNames(test)
#' listNames(test, 3)
#' listNames(test, 3, TRUE)
listNames <- function(l, maxDepth = 2, split = FALSE) {
  if(R6::is.R6(l) | shiny::is.reactivevalues(l)){
    # decompose first if it is a R6 object
    l = sapply(l$get(), function(x) {
        if(shiny::is.reactivevalues(x)) shiny::reactiveValuesToList(x)
      })
  }
  depth = 0
  listNames_rec = function(l, depth) {
    if(!is.list(l) | is.data.frame(l) | depth >= maxDepth) TRUE
    else {
      depth = depth + 1
      lapply(l, listNames_rec, depth)
    }
  }
  nms = names(unlist(listNames_rec(l, depth)))
  if(split==TRUE){
    nms = strsplit(nms,split = ".", fixed = TRUE)
  }
  return(nms)
}

#' @title show_view
#' @description Show View Returns a list of panels, which are marked to be shown in the
#'   accordingly used RData from previous analysis. This is currently not evaluated
#'   but could be useful in the future. Keep for now and don't delete,
#' @param rv The eCerto R6 object.
#' @return A character vector of panels to be shown or more precisely the parent list names
#'   which contain a sub item 'show' that is set to TRUE.
#' @keywords internal
#' @noRd
#' @examples
#' rv <- eCerto::eCerto$new(eCerto:::init_rv()) # initiate persistent variables
#' shiny::isolate({setValue(rv, c("Certification_processing","CertValPlot","show"),TRUE) })
#' print(eCerto:::show_view(rv))
#' shiny::isolate({setValue(rv, c("Certification_processing","mstats","show"),TRUE) })
#' print(eCerto:::show_view(rv))
show_view <- function(rv){
  nms <- shiny::isolate(listNames(rv, maxDepth = 3, split = TRUE))
  visible = c()
  for (n in nms) {
    i = any(n %in% "show")
    if(i && !is.null(shiny::isolate(getValue(rv, n))) && shiny::isolate(getValue(rv, n))) {
      visible = c(visible,n[2])
    }
  }
  return(visible)
}

#' @title sub_header.
#' @description Format a text as bold paragraph and with specific left/bottom margin.
#' @param txt The sub_header text to format.
#' @param l Left margin in percent.
#' @param b Bottom margin left in percent.
#' @return HTML to include in UI.
#' @keywords internal
#' @noRd
#' @examples
#' eCerto:::sub_header("test")
#' eCerto:::sub_header("test", l=5, b=0)
sub_header <- function(txt="test", l=0, b=5, unit=c("px", "%")) {
  u <- match.arg(unit)
  shiny::div(
    style = paste0("margin-left: ", l, u, "; margin-bottom: ", b, u, "; font-weight: 700"),
    txt
  )
}
