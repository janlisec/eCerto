#' @title check_fmt_Vdata.
#' @description \code{check_fmt_Vdata} will read an xlsx file and try to determine
#'     if it fits an appropriate input format of `eCerto`.
#' @details tbd
#' @param file A valid filepath for an xlsx.
#' @return A character defining a valid format by keyword or NA.
#' @examples
#' # example code
#' inp <- system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx")
#' eCerto:::check_fmt_Vdata(file = inp)
#' @keywords internal
#' @noRd
check_fmt_Vdata <- function(file = NULL) {
    # default return value
    fmt <- as.character(NA)
    # excel data representation
    tab <- tidyxl::xlsx_cells(path = file, sheets = 1)
    # check if all entries in col 1 are empty to comfirm Agilent format
    x <- tab[tab$col==1 & tab$row %in% 2:nrow(tab),]
    if (all(x[,"is_blank"] | (!is.na(x[,"character"]) & x[,"character"]==""))) fmt <- "Agilent"
    # check if all expected eCerto names are in row 1
    x <- as.vector(unlist(tab[tab$row == 1, "character"]))
    if (all(c("ID", "Name", "Level", "Analyte", "Concentration", "Area_Analyte", "Area_IS") %in% x)) fmt <- "eCerto"
    return(fmt)
}
