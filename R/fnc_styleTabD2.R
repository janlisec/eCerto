#' @title fnc_styleTabD2.
#' @description \code{styleTabD2} will style Tab.D2 for pretty output.
#' @details tbd.
#' @param df The data.frame of values.
#' @param selected Currently selected row.
#' @param interact_ele Show interactive elements (ordering and buttons), respectively use FALSE to hide them for Word export.
#' @examples
#' inp <- "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/DRMD/drmc-007.xml"
#' tab <- eCerto:::read_drmd_xml(inp)
#' tab <- eCerto:::xml2df(tab, type = "quant")
#' out_dt <- eCerto:::styleTabD2(df = tab, selected = NULL, interact_ele = FALSE)
#' out_dt
#' @return A datatable object.
#' @keywords internal
#' @noRd
styleTabD2 <- function(df, selected = 1, interact_ele = TRUE, L3 = NULL) {
  e_msg("Styling Tab.D2 for HTML output")

  #browser()
  # === new version
  if (all(c("idx", "path") %in% colnames(df))) {
    df <- df[df[,"value"]!="[ comment ]",]
    df <- df[grep("quantity", df[,"path"]),]
    df$L3 <- sapply(strsplit(df[,"idx"],"_"),function(x){x[3]})
    df$L8 <- sapply(strsplit(df[,"idx"],"_"),function(x){x[8]})
    df$name <- sapply(strsplit(df[,"path"],"_"),function(x){x[length(x)]})
    df <- cbind(df[,!colnames(df)%in%"value"], "value" = df[,"value"])
  }
  # === new version

  if (is.null(L3)) {
    result_idx <- unique(df$L3)
    L3 <- result_idx[1]
  } else {
    req(L3 %in% unique(df$L3))
  }

  tmp <- df[df$L3==L3,]
  df <- dplyr::bind_rows(lapply(split(tmp, tmp$L8), function(x) {
    stats::setNames(x$value, apply(x[,-ncol(x),drop=FALSE], 1, function(y) {
      rev(stats::na.omit(y))[1]
    }))
  }))

  #if (selected %in% 1:now(df))

  # modify column names, using HTML formatting
  #colnames(df) <- gsub("^value$", "value", colnames(df))

  # create DT object
  dt <- DT::datatable(
    data = df, rownames = FALSE, extensions = "Buttons", escape = FALSE,
    options = list(
      dom = ifelse(interact_ele, "Bt", "t"), pageLength = -1, ordering = FALSE,
      buttons = if (interact_ele)  { list(list(extend = "excel", text = "Excel", title = NULL)) }
    ),
    selection = list(mode = "single", selected = selected, target = 'row')
  )

  return(dt)
}
