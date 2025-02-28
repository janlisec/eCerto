#' @title fnc_xml2df.
#'
#' @description \code{xml2df} will handle upload of a single XML file.
#'
#' @details Function will handle upload of a single XML file.
#'
#' @param xml_list An imported DRMD XML file converted to a list.
#' @param type Type of data to extract from the list.
#'
#' @return A tibble.
#'
#' @noRd
#' @keywords internal
xml2df <- function(xml_list, type = c("admin", "quant", "full")) {
  type <- match.arg(type)

  # helpers
  list2df <- function(x, upper_levels = NULL) {
    if (!is.list(x) && length(x)==1) {
      if (length(upper_levels)==0) nm <- NULL else nm <- paste0("L", 1:length(upper_levels))
      out <- as.data.frame(matrix(c(upper_levels, as.character(x[[1]])), nrow=1, dimnames=list(NULL, c(nm, "value"))))
    } else {
      out <- lapply(1:length(x), function(i) { list2df(x[[i]], upper_levels = c(upper_levels, names(x)[i])) })
    }
    return(out)
  }
  flatten_list <- function(x) {
    lst_idx <- sapply(x, is.list) & !sapply(x, is.data.frame)
    if (any(lst_idx)) {
      y <- list()
      for (i in 1:length(lst_idx)) {
        if (lst_idx[i]) {
          y <- c(y, unlist(x[i], recursive = FALSE))
        } else {
          y <- c(y, x[i])
        }
      }
      out <- flatten_list(x = y)
    } else {
      out <- dplyr::bind_rows(x)
    }
    return(out)
  }

  # checks
  stopifnot(names(xml_list)=="digitalReferenceMaterialDocument")

  if (type=="admin") {
    stopifnot("drmd:administrativeData" %in% names(purrr::pluck(xml_list, "digitalReferenceMaterialDocument")))
    ele <- c("digitalReferenceMaterialDocument", "drmd:administrativeData")
    #ele <- c("digitalReferenceMaterialDocument", "drmd:administrativeData", "drmd:items", "drmd:item")
    out <- list2df(x=purrr::pluck(xml_list, !!!ele), upper_levels = ele)
    out <- flatten_list(x = out)
  }

  if (type=="quant") {
    stopifnot("drmd:measurementResults" %in% names(purrr::pluck(xml_list, "digitalReferenceMaterialDocument")))
    ele1 <- c("digitalReferenceMaterialDocument", "drmd:measurementResults")
    # get the multiple measurement results
    idx1 <- which(names(purrr::pluck(xml_list, !!!ele1))=="drmd:measurementResult")
    out <- lapply(idx1, function(i) {
      ele2 <- c("dcc:results", "dcc:result", "dcc:data", "dcc:list")
      # get the multiple quantity entries
      idx2 <- which(names(purrr::pluck(xml_list, !!!ele1, i, !!!ele2))=="dcc:quantity")
      out <- lapply(idx2, function(j) {
        out <- flatten_list(list2df(x=purrr::pluck(xml_list, !!!ele1, i, !!!ele2, j), upper_levels = c(ele1, i, ele2, j)))
        out <- flatten_list(x = out)
        return(out)
      })
      out <- flatten_list(x = out)
      return(out)
    })
    out <- flatten_list(x = out)
  }

  if (type=="full") {
    browser()
    ele <- names(xml_list)
    out <- list2df(x=purrr::pluck(xml_list, !!!ele), upper_levels = ele)
    out <- flatten_list(x = out)
  }

  # sorting
  return(out[,order(as.numeric(gsub("[^[:digit:]]", "", colnames(out))), na.last = T),drop=FALSE])

}
