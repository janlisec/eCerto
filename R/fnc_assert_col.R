#'@title fnc_assert_col.
#'@description \code{assert_col} will check in a data.frame for name, position,
#'    type of a specific column and ensure that the return value (data frame)
#'    contains a respective column.
#'@details tbd.
#'@param df Input data frame.
#'@param name Name of the column to ensure (and to search for).
#'@param pos Position of this column. NULL to keep position where found in df.
#'@param type Desired data type of this column.
#'@param fuzzy_name Allow fuzzy matching (additional blanks and case insensitive search allowed).
#'@param default_value Default value if column needs to be created or can not be converted to specified type. Keep NULL to use pre defined
#'
#'@examples
#'x <- eCerto:::test_homog()$data
#'assert_col <- eCerto:::assert_col
#'str(assert_col(df = x, name = "analyte", pos = 1, type = "factor"))
#'str(assert_col(df = x, name = "Analyte", pos = 3, type = "character"))
#'str(assert_col(df = x, name = " Analyte", pos = 2, type = "factor"))
#'str(assert_col(df = x, name = "Analyte", pos = 2, type = "factor", fuzzy_name = FALSE))
#'str(assert_col(df = x, name = "test", type = "factor", default_value = "test"))
#'str(assert_col(df = x, name = "unit", type = "numeric", default_value = "test"))
#'str(assert_col(df = x, name = "unit", type = "numeric", default_value = 10))
#'str(assert_col(df = x, name = "unit", type = "Date"))
#'str(assert_col(df = data.frame("test"="2022-03-31"), name = "test", type = "Date"))
#'# show type and class of internal default values
#'x <- data.frame("character" = "", "integer" = 0L, "numeric" = 0, "factor" = factor(NA),
#'    "logical" = NA, "date" = Sys.Date(), NA)
#'sapply(1:ncol(x), function(i) { typeof(x[,i]) })
#'sapply(1:ncol(x), function(i) { class(x[,i]) })
#'

#'@return A data frame with a column of the specified name and type at the specified position.
#'@keywords internal
assert_col <- function(df, name, pos = NULL, type = c("character", "integer", "numeric", "factor", "logical", "Date"), fuzzy_name = TRUE, default_value = NULL) {

  type <- match.arg(type)

  # keep previous messages or set to NULL if non exist
  msg <- attr(df, "msg")

  # helper function
  convert_type <- function(x, type) {
    switch(
      type,
      "character" = as.character(x),
      "integer" = as.integer(x),
      "numeric" = as.numeric(x),
      "factor" = factor(x, levels=unique(as.character(x))),
      "logical" = as.logical(x),
      "Date" = as.Date.character(x, tryFormats = c("%Y-%m-%d","%d.%m.%Y","%Y/%m/%d")),
      NA
    )
  }

  # check default value
  if (is.null(default_value)) {
    default_value <- switch(
      type,
      "character" = "",
      "integer" = 0L,
      "numeric" = 0,
      "factor" = factor(NA),
      "logical" = NA,
      "Date" = Sys.Date(),
      NA
    )
  } else {
    default_value <- convert_type(x = default_value[1], type = type)
  }

  # find column position
  if (fuzzy_name) {
    cp <- which(gsub(" ", "", tolower(colnames(df))) == gsub(" ", "", tolower(name)))
  } else {
    cp <- which(colnames(df) == name)
  }
  if (length(cp)==0) {
    msg <- c(msg, paste0("Column '", name, "' not found. Create new column with default values."))
    cp <- NULL
  }
  if (length(cp)>1) {
    msg <- c(msg, "Found >=1 matching columns. Processing first.")
    cp <- cp[1]
  }

  # check pos
  if (!is.null(pos)) {
    pos <- as.numeric(pos[1])
    if (!pos %in% 1:ncol(df)) {
      msg <- c(msg, "Specified column position outside data frame range.")
      pos <- ncol(df)+1
    }
  } else {
    pos <- ifelse(is.null(cp), ncol(df)+1, cp)
  }

  # create new values
  if (is.null(cp)) {
    new_vals <- rep(default_value, nrow(df))
  } else {
    if (class(df[,cp])!=type) {
      new_vals <- try(convert_type(x = df[,cp], type = type))
      if (inherits(new_vals, "try-error")) {
        msg <- c(msg,  paste0("Could not convert column '", name, "' to specified type."))
        new_vals <- rep(default_value, nrow(df))
      }
    } else {
      new_vals <- df[,cp]
    }
  }
  new_vals <- stats::setNames(data.frame(new_vals), name)

  # put dataframe together
  if (!is.null(cp)) df <- df[,-cp, drop=FALSE]
  if (pos>ncol(df)) {
    df <- cbind(df, new_vals)
  } else if (pos==1) {
    df <- cbind(new_vals, df)
  } else {
    df <- cbind(df[,1:(pos-1),drop=FALSE], new_vals, df[,pos:ncol(df),drop=FALSE])
  }

  if (!is.null(msg)) attr(df, "msg") <- msg

  return(df)
}









