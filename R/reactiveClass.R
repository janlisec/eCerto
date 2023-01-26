#' @title A reactive class based on an R6 object.
#' @description Builds a class, which allows only restricted access to the
#'    contained 'reactiveValues'. Elements should be accessed via [getValue()].
#'    Possible advantages are that (1) structure of 'reactiveValues' is clear
#'    from the beginning (no function like "addVariable" should exist!) and that
#'    (2) functions to calculate the mean or plot current data can be implemented
#'    here directly.
#' @name eCerto_R6Class
#' @examples
#' if (interactive()) {
#' rv <- shiny::reactiveValues(a=1)
#' shiny::observeEvent(rv$a, { message("rv$a changed:", rv$a) })
#' shiny:::flushReact()
#' rv$a <- 2
#' shiny:::flushReact()
#' test <- eCerto$new(eCerto:::init_rv())
#' shiny::isolate(eCerto::getValue(test, c("Certification","data")))
#' shiny::observeEvent(eCerto::getValue(test, c("Certification", "data")), {
#'   message("Certification$data changed:", eCerto::getValue(test, "Certification")$data)
#' })
#' shiny::isolate(eCerto::setValue(test, c("Certification","data"), 5))
#' shiny::isolate(eCerto::getValue(test, c("Certification","data")))
#' shiny:::flushReact()
#' }
#' tmp <- eCerto$new()
#' shiny::isolate(tmp$c_plot())
#' tmp$c_lab_means()
#' tmp$c_analyte
#' tmp$c_analytes()
#' tmp$c_lab_codes()
#' tmp$a_p()
#' tmp$a_p("pooling")
#' tmp$a_p("pooling")[tmp$c_analyte]
#' shiny::isolate(tmp$e_present())
#' shiny::isolate(tmp$c_analyte <- "Cu")
#' tmp$c_lab_means()
#' tmp$c_fltData()
#' x <- shiny::isolate(eCerto::getValue(tmp, c("General","apm")))
#' x[[tmp$c_analyte]][["lab_filter"]] <- "L2"
#' shiny::isolate(eCerto::setValue(tmp, c("General","apm"), x))
#' tmp$c_fltData(recalc = TRUE)
#' @importFrom purrr chuck pluck
#' @export
eCerto <- R6::R6Class(
  classname = "eCerto",
  private = list(
    ..eData = NULL,
    ..cAnalyte = NULL,
    ..cFltData = NULL
  ),
  public = list(
    # R6 object fields
    #' @description
    #' Write the (reactive) value of element 'keys' from list 'l'.
    #' @param rv 'reactiveValues' object.
    #' @return A new 'eCerto' object.
    initialize = function(rv) {
      # message("Initiate R6 object")
      if (missing(rv)) {
        # the 'reactiveValues' object parsed on initialize.
        # set up internal data structure and fill with test data
        rv <- init_rv()
        testdata <- test_Certification_Excel()
        rv[["Certification"]][["data"]] <- testdata
        rv[["General"]][["apm"]] <- init_apm(testdata)
        rv[["General"]][["materialtabelle"]] <- init_materialtabelle(sapply(init_apm(testdata),function(x){x[["name"]]}))
        private$..cAnalyte <- shiny::isolate( rv[["General"]][["apm"]][[1]][["name"]] )
        private$..cFltData <- shiny::isolate( eCerto:::c_filter_data(x = rv[["Certification"]][["data"]], c_apm = rv[["General"]][["apm"]][[1]]) )
        private$..eData <- rv
      } else {
        # [ToDo] implement testing (copy from RData upload module)
        private$..eData <- rv
        if (!is.null(shiny::isolate( rv[["General"]][["apm"]][[1]][["name"]] ))) {
          private$..cAnalyte <- shiny::isolate( rv[["General"]][["apm"]][[1]][["name"]] )
        }
      }
    },
    # R6 object functions
    #' @description Read the value of field element of R6 object.
    #' @param keys Name of list element.
    #' @return Current value of field.
    get = function(keys=NULL) {
      purrr::chuck(private$..eData, !!!keys)
    },
    #' @description Set element of R6 object defined by 'keys' to new value.
    #' @param keys Name(s) of list element.
    #' @param value New value.
    #' @return New value of element (invisible).
    set = function(keys=NULL, value){
      if(!is.null(value)) {
        purrr::pluck(private$..eData, !!!keys) <- value
      } else {
        # assigning NULL to a normal list element will delete it (and not replace its content by NULL)
        # however, most elements of an eCerto R6 object are reactiveValue items --> here,
        # the behavior is different: one can assign NULL to an reactiveValue-Item BUT (!!!)
        # assigning NULL to the reactiveValue-List itself would delete it
        # this case needs to be taken care of here
        if (length(keys)>=2 && is.reactivevalues(purrr::pluck(private$..eData, !!!keys[-length(keys)]))) {
          #browser()
          # [JL20230118_the out commented version stopped working after purr update and...]
          #purrr::pluck(private$..eData, !!!keys) <- NULL
          # [...was replaced by this version]
          purrr::pluck(private$..eData, !!!keys[-length(keys)])[[keys[length(keys)]]] <- NULL
        } else {
          warning(paste("Can't assign NULL to a standard list element: ", paste(keys, collapse=", ")))
        }
      }
      invisible(purrr::chuck(private$..eData, !!!keys))
    },
    #' @description Plot the certification data either provided by the user or from the private slot of self.
    #' @param data data.frame containing columns 'value', 'Lab' and 'L_flt' for a specific analyte.
    #' @param annotate_id T/F to overlay the plot with ID as text if column 'ID' is present.
    #' @param filename_labels T/F to use imported file names as labels on x-axes.
    #' @return A plot.
    c_plot = function(data, annotate_id = FALSE, filename_labels = FALSE) {
      if (missing(data)) {
        data <- private$..eData[["Certification"]][["data"]]
        data <- data[data[,"analyte"]==private$..cAnalyte,,drop=FALSE]
      }
      eCerto:::CertValPlot(data = data, annotate_id = annotate_id, filename_labels = filename_labels)
    },
    #' @description Compute the analyte means for a specific analyte.
    #' @param data data.frame containing columns 'analyte', 'value', 'Lab', 'S_flt' and 'L_flt'.
    #' @param analyte_name Specify the analyte you want the lab mean statistics for.
    #' @return A data.frame of lab means.
    c_lab_means = function(data, analyte_name) {
      if (missing(analyte_name)) {
        analyte_name <- private$..cAnalyte
      }
      flt_data <- private$..cFltData
      out <- plyr::ldply(split(flt_data$value, flt_data$Lab), function(x) {
        data.frame(
          "mean" = mean(x, na.rm = T),
          "sd" = stats::sd(x, na.rm = T),
          "n" = sum(is.finite(x)),
          stringsAsFactors = FALSE
        )
      }, .id = "Lab")
      rownames(out) <- out$Lab
      return(out)
    },
    #' @description Return analyte names currently in apm.
    #' @return A named character vector.
    c_analytes = function() {
      shiny::isolate(sapply(private$..eData[["General"]][["apm"]], function(x) {x[["name"]]}))
    },
    #' @description Return lab codes currently in C data.
    #' @return A named character vector.
    c_lab_codes = function() {
      fn <- shiny::isolate(private$..eData[["Certification"]][["data"]])
      fn <- fn[!duplicated(fn[,"Lab"]),c("Lab","File")]
      out <- as.character(fn[,"File"]); names(out) <- fn[,"Lab"]
      return(out)
    },
    #' @description Return currently specified values of a type for all analytes.
    #' @param val A character value indicating the item of the apm list to be extracted
    #' @return A named vector.
    a_p = function(val = c("precision", "precision_export", "pooling", "confirmed", "unit")) {
      val <- match.arg(val)
      as <- shiny::isolate(private$..eData[["General"]][["apm"]])
      out <- sapply(names(as), function(x) { as[[x]][[val]] })
      return(out)
    },
    #' @description Return modules with existing data.
    #' @return A named logical vector.
    e_present = function() {
      sapply(private$..eData[["modules"]], function(x) { !is.null(private$..eData[[x]][["data"]]) })
    },
    #' @description Filter the full data set for a specific analyte and remove all 'S_flt' but keep 'L_flt'.
    #' @param recalc If TRUE triggers a recalculation and returns current object if FALSE..
    #' @return A data.frame with filtered data of a single analyte.
    c_fltData = function(recalc = FALSE) {
      if (recalc) {
        private$..cFltData <- shiny::isolate(eCerto:::c_filter_data(
          x = private$..eData[["Certification"]][["data"]],
          c_apm = private$..eData[["General"]][["apm"]][[private$..cAnalyte]]
        ))
        return(private$..cFltData)
      } else {
        return(private$..cFltData)
      }
    }
  ),
  # R6 active bindings
  active = list(
    #' @field c_analyte Set or return the current analyte via an active binding.
    c_analyte = function(an) {
      if (missing(an)) {
        # simply return current analyte on focus in C Module
        if (is.null(private$..cAnalyte)) {
          # set value to first available in apm if not set previously
          private$..cAnalyte <- names(private$..eData[["General"]][["apm"]])[1]
        }
        return(private$..cAnalyte)
      } else {
        if (!identical(an, private$..cAnalyte)) {
          # set current analyte on focus in C Module and recalculate dependent reactive variables
          private$..cFltData <- eCerto:::c_filter_data(
            x = private$..eData[["Certification"]][["data"]],
            c_apm = private$..eData[["General"]][["apm"]][[an]]
          )
          #private$..eData[["Certification_processing"]][["cert_mean"]] <- 3
          private$..cAnalyte <- an
        }
      }
    }
  )
)
