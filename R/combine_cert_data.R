#'@title combine_cert_data.
#'
#'@description
#'\code{combine_cert_data} will post process data uploaded from Excel files of a certification trial.
#'
#'@details
#'not yet
#'
#'@param df_list list of imported Excel tables.
#'@param silent Option to print or omit status messages.
#'
#'@return
#'A dataframe.
#'
#'@export
#'
combine_cert_data <- function(df_list = NULL, silent = FALSE) {

    if (!silent) message("combine_cert_data: Prepare dataset after upload")

    # process tables from multiple files individually
    df_list <- lapply(df_list, function(x) {
      laboratory_dataframe(x)
    })

    # combine into single datafrafe
    df <- data.frame(
      "Lab" = rep(paste0("L", seq_along(df_list)), times = sapply(df_list, nrow)),
      as.data.frame(do.call(rbind, df_list)),
      "S_flt" = FALSE,
      "L_flt" = FALSE)

    # remove non-finite values
    df <- df[is.finite(df[, "value"]), ]

    # add index column
    df <- data.frame("ID" = 1:nrow(df), df)

    return(df)

}