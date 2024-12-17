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
#' @examples
#' # example code
#' inp <- system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx")
#' tab <- eCerto:::read_Vdata(file = inp)
#' head(tab)
#' @keywords internal
#' @noRd
read_Vdata <- function(file = NULL, fmt = c("Agilent", "eCerto")) {
    if (!any(is.na(fmt))) fmt <- match.arg(fmt)
    shiny::validate(shiny::need(file.exists(file), message = "Sorry, provided file path apparently invalid."))
    shiny::validate(shiny::need(!is.na(fmt), message = "Sorry, could not assign a known Excel import format to the selected file."))
    if (fmt == "Agilent") {
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
      n_cols <- unique(diff(grep("Exp..Conc.", c(colnames(tab_anal), "Exp..Conc."))))
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
    }
    if (fmt == "eCerto") {
      tab_out <- openxlsx::read.xlsx(xlsxFile = file, sheet = 1, colNames = TRUE, rowNames = FALSE)
      shiny::validate(shiny::need(!any(grep("xml:space", colnames(tab_out))), message = "Please re-save your Excel file and try again."))
    }
    if (!all(c("Name", "Type", "Level", "Analyte", "Concentration", "Area_Analyte", "Area_IS") %in% colnames(tab_out))) e_msg("Column in input Excel file missing")
    if (!is.factor(tab_out[,"Analyte"])) tab_out[,"Analyte"] <- factor(tab_out[,"Analyte"], levels=unique(tab_out[,"Analyte"]))
    if (!is.factor(tab_out[,"Level"])) {
      lev <- try(as.numeric(tab_out[,"Level"]))
      if (!inherits(lev, "try-error") && all(is.finite(lev))) {
        lev <- unique(as.character(sort(lev)))
      } else {
        lev <- unique(sort(as.character(tab_out[,"Level"])))
      }
      tab_out[,"Level"] <- factor(tab_out[,"Level"], levels = lev)
    }
    if (!"ID" %in% colnames(tab_out)) tab_out[,"ID"] <- 1:nrow(tab_out)
    if (!"norm" %in% colnames(tab_out)) tab_out[,"norm"] <- tab_out[,"Area_Analyte"]/tab_out[,"Area_IS"]
    gr_mn <- lapply(split(tab_out, tab_out[,"Analyte"]), function(x) { lapply(split(x[,"norm"], x[,"Level"]), mean, na.rm=TRUE) })
    if (!"rel_norm" %in% colnames(tab_out)) tab_out[,"rel_norm"] <- tab_out[,"norm"]/sapply(1:nrow(tab_out), function(i) {gr_mn[[tab_out[i,"Analyte"]]][[tab_out[i,"Level"]]]})
    if (!"Exclude_Level" %in% colnames(tab_out)) tab_out[,"Exclude_Level"] <- FALSE
    if (!"Exclude_Sample" %in% colnames(tab_out)) tab_out[,"Exclude_Sample"] <- FALSE
    # reorder columns
    tab_out <- tab_out[,c("ID", "Name", "Type", "Level", "Analyte", "Concentration", "Area_Analyte", "Area_IS", "norm", "rel_norm", "Exclude_Level", "Exclude_Sample")]
    return(tab_out)
  }