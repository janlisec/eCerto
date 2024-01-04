#'@title fnc_styleTabH1.
#'@description \code{styleTabH1} will style Tab.H1 for pretty output.
#'@details tbd.
#'@param x The Hom data from an session R6 object.
#'@param mt The mt from an session R6 object.
#'@param prec The precision of all analytes from x (names vector).
#'@param output Return either the dataframe with styling information in columns or the corresponding datatable object.
#'@param cr Current row selected (relevant if output = 'dt').
#'@examples
#'x <- eCerto:::prepTabH1(x = eCerto:::test_homog()$data)
#'x
#'eCerto:::styleTabH1(x = x)
#'mt <- data.frame("analyte"="Fe")
#'eCerto:::styleTabH1(x = x, mt = mt)
#'prec <- unlist(list("Fe"=2))
#'eCerto:::styleTabH1(x = x, prec = prec)
#'eCerto:::styleTabH1(x = x, output = "dt", prec = prec)
#'@return A data frame or a datatable object depending on parameter 'output'.
#'@keywords internal
styleTabH1 <- function(x, mt = NULL, prec = NULL, output = c("df", "dt")[1], cr = 1) {
  message("[styleTabH1] styling Tab.H1")
  style_x <- x
  for (i in 1:nrow(style_x)) {
    an <- as.character(style_x[i,"analyte"])
    #style_x[i,"mean"] <- pn(as.numeric(style_x[i,"mean"]), ifelse(an %in% names(apm), apm[[an]][["precision"]], 4))
    style_x[i,"mean"] <- pn(as.numeric(style_x[i,"mean"]), ifelse(an %in% names(prec), prec[an], 4))
  }
  # round the following columns with fixed precision of 4 digits
  for (cn in c("M_between","M_within","P","s_bb","s_bb_min")) {
    style_x[,cn] <- pn(style_x[,cn], 4)
  }
  # check if analyte is present in C module
  if (!is.null(mt)) {
    style_x[,"style_analyte"] <- sapply(style_x[,"analyte"], function(x) {
      ifelse(x %in% mt[,"analyte"], "", "red")
    })
  } else {
    style_x[,"style_analyte"] <- "red"
  }
  style_x[,"style_s_bb"] <- c("bold","normal")[1+as.numeric(style_x[,"s_bb"]<style_x[,"s_bb_min"])]
  style_x[,"style_s_bb_min"] <- c("bold","normal")[1+as.numeric(style_x[,"s_bb"]>=style_x[,"s_bb_min"])]

  if (output=="df") {
    return(style_x)
  } else {
    x <- style_x
    # set invisible cols
    inv_cols <- grep("style_", colnames(x))-1
    if (length(unique(x[,"H_type"]))==1) inv_cols <- c(1, inv_cols)
    # format substring column header
    colnames(x) <- gsub("_type", "<sub>type</sub>", colnames(x))
    colnames(x) <- gsub("_between", "<sub>between</sub>", colnames(x))
    colnames(x) <- gsub("_within", "<sub>within</sub>", colnames(x))
    colnames(x) <- gsub("^s_bb$", "s<sub>bb</sub>", colnames(x))
    colnames(x) <- gsub("^s_bb_min$", "s<sub>bb,min</sub>", colnames(x))
    # attach a blank column at the end
    x <- cbind(x, data.frame(" "=" ", check.names = FALSE))
    # prepare DT
    dt <- DT::datatable(
      data = x,
      options = list(
        dom = "t", paging = FALSE, searching = FALSE, ordering = FALSE,
        columnDefs = list(
          list("width"= paste0(max(c(60, nchar(as.character(x[,"analyte"]))*9)), "px"), "targets" = which(colnames(x) %in% c("analyte"))-1),
          list("width"= "60px", "targets" = which(!(colnames(x) %in% c("analyte", " ", "n", "N")))-1),
          list("width"= "30px", "targets" = which(colnames(x) %in% c("n", "N"))-1),
          list(visible = FALSE, targets = inv_cols),
          list(className = 'dt-right', targets = which(!(colnames(x) %in% c("analyte")))-1),
          list(className = 'dt-left', targets = which(colnames(x) %in% c("analyte"))-1)
        )
      ),
      rownames=NULL, escape = FALSE, selection = list(mode="single", target="row", selected=cr)
    )
    # style different DT columns
    dt <- DT::formatStyle(
      table = dt,
      columns = "analyte",
      valueColumns = "style_analyte",
      target = "cell",
      color = DT::styleValue()
    )
    dt <- DT::formatStyle(
      table = dt,
      columns = "s<sub>bb</sub>",
      valueColumns = "style_s_bb",
      target = "cell",
      fontWeight = DT::styleValue()
    )
    dt <- DT::formatStyle(
      table = dt,
      columns = "s<sub>bb,min</sub>",
      valueColumns = "style_s_bb_min",
      target = "cell",
      fontWeight = DT::styleValue()
    )
    dt <- DT::formatStyle(
      table = dt,
      columns = "P",
      target = "cell",
      color = DT::styleInterval(cuts = 0.05, values = c("red","")),
      fontWeight = DT::styleInterval(cuts = 0.05, values = c("bold","normal"))
    )
    return(dt)
  }
}