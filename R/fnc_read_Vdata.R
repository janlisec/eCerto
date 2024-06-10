#' @title read_Vdata.
#' @description \code{read_Vdata} will read an xlsx file as exported from Mass
#'     Hunter Software.
#' @details `eCerto` allows to store imported data and user specified parameter
#'     values in RData files for backup. The files can be re-imported to `eCerto`
#'     at later time points. At this point values need to be put into the correct
#'     slots of an `eCerto`object. Such files can be stored in zenodo and imported
#'     via DOI.
#' @param file A valid filepath for an xlsx.
#' @param fmt The format of the input file. Currently only 'Agilent' type is supported.
#' @return A object 'res' from an RData file.
#' @keywords internal
#' @noRd
read_Vdata <- function(file = NULL, fmt = c("Agilent", "eCerto")) {
    fmt <- match.arg(fmt)
    tab <- openxlsx::read.xlsx(xlsxFile = file, sheet = 1, startRow = 2, check.names = FALSE)
    # check/prepare main table
    stopifnot(all(c("Name","Type","Level") %in% colnames(tab)))
    tab_main <- tab[,c("Name","Type","Level")]
    tab_main[,"Type"] <- "Cal"
    stopifnot(!is.na(tab_main[1,"Level"]))
    tab_main[,"Level"] <- auto_fill(tab_main[,"Level"])
    # check/prepare analyte table
    stopifnot(all(c("Exp..Conc.","Area") %in% colnames(tab)))
    tab_anal <- tab[,min(grep("Exp..Conc.", colnames(tab))):ncol(tab)]
    n <- length(grep("Exp..Conc.", colnames(tab_anal)))
    n_cols <- unique(diff(grep("Exp..Conc.", colnames(tab_anal))))
    stopifnot(length(n_cols)==1)
    stopifnot(n_cols %in% c(2,3))
    # get analyte names
    tab_hd <- unname(unlist(openxlsx::read.xlsx(xlsxFile = file, sheet = 1, rows = 1, colNames = FALSE)))[-1]
    a_names <- gsub(" Method", "", tab_hd[((1:n)-1)*n_cols+1])
    tab_out <- plyr::ldply(1:n, function(i) {
      tmp <- tab_anal[,(i-1)*n_cols+1:n_cols]
      colnames(tmp)[1:2] <- c("Concentration", "Area_Analyte")
      tmp[,1] <- auto_fill(tmp[,1])
      if (n_cols==2) tmp <- cbind(tmp, "Area_IS"=NA) else colnames(tmp)[3] <- "Area_IS"
      out <- cbind(tab_main, "Analyte"=a_names[i], tmp)
      return(out)
    })
    tab_out <- cbind("ID"=1:nrow(tab_out), tab_out)
    tab_out[,"Analyte"] <- factor(tab_out[,"Analyte"], levels=unique(tab_out[,"Analyte"]))
    tab_out[,"Level"] <- factor(tab_out[,"Level"])
    tab_out[,"norm"] <- tab_out[,"Area_Analyte"]/tab_out[,"Area_IS"]
    gr_mn <- lapply(split(tab_out, tab_out[,"Analyte"]), function(x) { lapply(split(x[,"norm"], x[,"Level"]), mean, na.rm=TRUE) })
    tab_out[,"rel_norm"] <- tab_out[,"norm"]/sapply(1:nrow(tab_out), function(i) {gr_mn[[tab_out[i,"Analyte"]]][[tab_out[i,"Level"]]]})
    tab_out[,"Exclude_Level"] <- FALSE
    tab_out[,"Exclude_Sample"] <- FALSE
    return(tab_out)
  }