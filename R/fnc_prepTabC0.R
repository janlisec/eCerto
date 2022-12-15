#' @title fnc_prepTabC0.
#' @description \code{prepTabC0} will post process data uploaded from Excel files
#'     of a certification trial.
#' @details not yet
#' @param df_list list of already imported Excel tables.
#' @examples
#' x <- list(
#'   data.frame(
#'     "A"=paste0("A",1:4),
#'     "U"=rep("U",4),
#'     "R1"=1:4,
#'     "R2"=rnorm(4),
#'     "File"=rep("F1",4)
#'   ),
#'   data.frame(
#'     "A"=paste0("A",1:4),
#'     "U"=rep("U",4),
#'     "R1"=5:8,
#'     "R2"=rep(NA,4),
#'     "File"=rep("F2",4)
#'   )
#' )
#' prepTabC0(x)
#' @return A single data frame combining all data frames from input files.
#' @noRd
#' @keywords internal
prepTabC0 <- function(df_list = NULL) {
  helper_function <- function(x) {
    x2 <- as.data.frame(x)
    # # test if columns contain finite numeric values and remove if not
    # x2_sub <- x2[, !names(x2) %in% c(names(x2[, c(1, 2)]), "File")]
    # flt <- apply(x2_sub, 1, function(y) {
    #   any(is.finite(as.numeric(y)))
    # })
    # if (any(flt)) x2 <- x2[which(flt), , drop = F]
    # combine into data frame and return
    analyte <- x2[, 1]
    unit <- x2[, 2]
    # drop first (analyte name), second (unit) and File name column
    dat <- x2[, !names(x2) %in% c(names(x2[, c(1, 2)]), "File")]
    # create new data frame
    x3 <- data.frame(
      "analyte" = factor(rep(analyte, times = ncol(dat)), levels = analyte),
      "replicate" = factor(rep((1:ncol(dat)), each = nrow(dat))),
      "value" = as.numeric(unlist(dat)),
      "unit" = as.character(rep(unit, times = ncol(dat)))
    )
    # add File column again (is this redundant with being removed above?)
    if (any(names(x2) %in% "File")) {
      filecol <- x2[["File"]]
      x3["File"] <- as.character(rep(filecol, times = ncol(dat)))
    }
    return(x3)
  }

  if (!get_golem_config("silent")) message("[prepTabC0]: Prepare dataset after upload")

  # process tables from multiple files individually
  df_list_tmp <- lapply(df_list, function(x) {
    helper_function(x)
  })

  # combine into single data frame and attach filter columns
  df <- data.frame(
    "Lab" = rep(paste0("L", seq_along(df_list_tmp)), times = sapply(df_list_tmp, nrow)),
    as.data.frame(do.call(rbind, df_list_tmp)),
    "S_flt" = FALSE,
    "L_flt" = FALSE
  )

  # remove non-finite values
  df <- df[is.finite(df[, "value"]), ]

  # prepend data frame with index column
  df <- data.frame("ID" = 1:nrow(df), df)

  return(df)
}