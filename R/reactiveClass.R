#' @name eCerto_R6Class
#' @aliases eCerto
#' @aliases setValue
#' @aliases getValue
#' @title A reactive class based on an R6 object.
#' @description Builds a class, which allows only restricted access to the
#'    contained 'reactiveValues'. Elements should be accessed via [getValue()].
#'    Possible advantages are that (1) structure of 'reactiveValues' is clear
#'    from the beginning (no function like "addVariable" should exist!) and that
#'    (2) functions to calculate the mean or plot current data can be implemented
#'    here directly.
#' @rdname eCerto_R6Class
#' @examples
#' \donttest{
#' if (interactive()) {
#'   test <- eCerto$new()
#'   shiny::isolate(eCerto::getValue(test, c("Certification", "data")))
#'   shiny::observeEvent(eCerto::getValue(test, c("Certification", "data")), {
#'     message("Certification$data changed:", eCerto::getValue(test, "Certification")$data)
#'   })
#'   shiny::isolate(eCerto::setValue(test, c("Certification", "data"), 5))
#'   shiny::isolate(eCerto::getValue(test, c("Certification", "data")))
#'   shiny:::flushReact()
#' }
#' tmp <- eCerto$new()
#' shiny::isolate(tmp$c_plot())
#' shiny::isolate(tmp$c_lab_means())
#' tmp$c_analytes()
#' tmp$c_lab_codes()
#' tmp$a_p()
#' tmp$a_p("pooling")
#' ca <- shiny::isolate(tmp$cur_an)
#' tmp$a_p("pooling")[ca]
#' shiny::isolate(tmp$e_present())
#' tmp$c_fltData()
#' shiny::isolate(tmp$cur_an <- "Fe")
#' shiny::isolate(tmp$cur_an)
#' tmp$c_fltData()
#' x <- shiny::isolate(eCerto::getValue(tmp, c("General", "apm")))
#' x[[shiny::isolate(tmp$cur_an)]][["lab_filter"]] <- "L2"
#' shiny::isolate(eCerto::setValue(tmp, c("General", "apm"), x))
#' tmp$c_fltData()
#' tmp$c_fltData(recalc = TRUE)
#' }
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
      e_msg("Initiate eCerto R6 object")
      if (missing(rv)) {
        # the 'reactiveValues' object parsed on initialize.
        # set up internal data structure and fill with test data
        rv <- init_rv()
        testdata <- test_Certification_Excel()
        rv[["Certification"]][["data"]] <- testdata
        rv[["General"]][["apm"]] <- init_apm(testdata)
        rv[["General"]][["materialtabelle"]] <- init_materialtabelle(sapply(init_apm(testdata), function(x) {
          x[["name"]]
        }))
        an <- shiny::isolate(rv[["General"]][["apm"]][[1]][["name"]])
        private$..cAnalyte <- shiny::reactiveVal(an)
        private$..cFltData <- shiny::isolate(c_filter_data(x = rv[["Certification"]][["data"]], c_apm = rv[["General"]][["apm"]][[an]]))
        private$..eData <- rv
      } else {
        # [ToDo] implement testing (copy from RData upload module)
        private$..eData <- rv
        an <- shiny::isolate(rv[["General"]][["apm"]][[1]][["name"]])
        if (!is.null(an)) {
          private$..cAnalyte <- shiny::reactiveVal(shiny::isolate(rv[["General"]][["apm"]][[1]][["name"]]))
        } else {
          private$..cAnalyte <- shiny::reactiveVal(NULL)
        }
      }
    },
    # Standard print output for R6 object
    # print = function() {
    #   print("I am an eCerto object")
    # },
    # R6 object functions
    #' @description Read the value of field element of R6 object.
    #' @param keys Name of list element.
    #' @return Current value of field.
    get = function(keys = NULL) {
      purrr::chuck(private$..eData, !!!keys)
    },
    #' @description Set element of R6 object defined by 'keys' to new value.
    #' @param keys Name(s) of list element.
    #' @param value New value.
    #' @return New value of element (invisible).
    set = function(keys = NULL, value) {
      if (!is.null(value)) {
        purrr::pluck(private$..eData, !!!keys) <- value
      } else {
        # assigning NULL to a normal list element will delete it (and not replace its content by NULL)
        # however, most elements of an eCerto R6 object are reactiveValue items --> here,
        # the behavior is different: one can assign NULL to an reactiveValue-Item BUT (!!!)
        # assigning NULL to the reactiveValue-List itself would delete it
        # this case needs to be taken care of here
        if (length(keys) >= 2 && is.reactivevalues(purrr::pluck(private$..eData, !!!keys[-length(keys)]))) {
          # [JL20230118_the out commented version stopped working after purrr update and...]
          # purrr::pluck(private$..eData, !!!keys) <- NULL
          # [...was replaced by this version]
          purrr::pluck(private$..eData, !!!keys[-length(keys)])[[keys[length(keys)]]] <- NULL
        } else {
          warning(paste("Can't assign NULL to a standard list element: ", paste(keys, collapse = ", ")))
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
        data <- data[data[, "analyte"] == private$..cAnalyte(), , drop = FALSE]
      }
      CertValPlot(data = data, annotate_id = annotate_id, filename_labels = filename_labels)
    },
    #' @description Compute the analyte means for a data set filtered for a specific analyte.
    #' @param data data.frame containing columns 'analyte', 'value', 'Lab', 'S_flt' and 'L_flt'.
    #' @return A data.frame of lab means.
    c_lab_means = function(data) {
      if (missing(data)) {
        data <- private$..cFltData
      }
      out <- plyr::ldply(split(data$value, data$Lab), function(x) {
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
      shiny::isolate(sapply(private$..eData[["General"]][["apm"]], function(x) {
        x[["name"]]
      }))
    },
    #' @description Return lab codes currently in C data.
    #' @return A named character vector.
    c_lab_codes = function() {
      fn <- shiny::isolate(private$..eData[["Certification"]][["data"]])
      fn <- fn[!duplicated(fn[, "Lab"]), c("Lab", "File")]
      out <- as.character(fn[, "File"])
      names(out) <- fn[, "Lab"]
      return(out)
    },
    #' @description Return currently specified values of a type for all analytes.
    #' @param val A character value indicating the item of the apm list to be extracted
    #' @return A named vector.
    a_p = function(val = c("precision", "precision_export", "pooling", "confirmed", "unit", "name")) {
      val <- match.arg(val)
      as <- shiny::isolate(private$..eData[["General"]][["apm"]])
      out <- sapply(names(as), function(x) {
        as[[x]][[val]]
      })
      return(out)
    },
    #' @description Return modules with existing data.
    #' @return A named logical vector.
    e_present = function() {
      sapply(private$..eData[["modules"]], function(x) {
        !is.null(private$..eData[[x]][["data"]])
      })
    },
    #' @description Filter the full data set for a specific analyte and remove all 'S_flt' but keep 'L_flt'.
    #' @param recalc If TRUE triggers a recalculation and returns current object if FALSE..
    #' @return A data.frame with filtered data of a single analyte.
    c_fltData = function(recalc = FALSE) {
      if (recalc) {
        private$..cFltData <- shiny::isolate(c_filter_data(
          x = private$..eData[["Certification"]][["data"]],
          c_apm = private$..eData[["General"]][["apm"]][[private$..cAnalyte()]]
        ))
        return(private$..cFltData)
      } else {
        return(private$..cFltData)
      }
    }
  ),
  # R6 active bindings
  active = list(
    #' @field cur_an Set or return the current analyte (reactiveVal) via an active binding.
    cur_an = function(a) {
      if (missing(a)) {
        if (is.null(private$..cAnalyte())) {
          private$..cAnalyte(names(private$..eData[["General"]][["apm"]])[1])
        }
        return(private$..cAnalyte())
      } else {
        if (!identical(a, private$..cAnalyte())) {
          if (self$e_present()["Certification"] & a %in% self$a_p("name")) {
            # set current analyte on focus in C Module and recalculate dependent reactive variables if C data are present and a is valid
            private$..cFltData <- c_filter_data(
              x = private$..eData[["Certification"]][["data"]],
              c_apm = private$..eData[["General"]][["apm"]][[a]]
            )
          }
          private$..cAnalyte(a)
        }
      }
    }
  )
)

#' @title setValue.
#' @description General access to data object (so data object can maybe get
#'     changed without that much code edit)
#' @rdname eCerto_R6Class
#' @param df The data frame (an R6 object).
#' @param key A character vector specifying the key-chain to put the value in (see examples).
#' @param value Value to set.
#' @return Nothing. The R6 object is updated automatically.
#' @examples
#' # Only run examples in interactive R sessions
#' if (interactive()) {
#'   rv <- eCerto$new(init_rv())
#'   setValue(rv, c("Certification", "data"), 5)
#'   getValue(rv, c("Certification", "data")) # is 5?
#'   setValue(rv, c("General", "user"), "Franz")
#'   getValue(rv, c("General", "user"))
#' }
#' @export
setValue <- function(df, key, value) {
  if (R6::is.R6(df)) {
    df$set(key, value) # in eCerto.R
  } else {
    stop("Object of class ", class(df), " can't set value currently.")
  }
}

#' @title getValue.
#' @description Returns element. If 'key' is used, reactivity not working correctly.
#' Preferable way for calling `getValue(df, key)`, see example
#' @rdname eCerto_R6Class
#' @param df An object of class R6.
#' @param key Key value within R6 object 'df'.
#' @return Value of 'key' from 'df'.
#' @export
getValue <- function(df, key = NULL) {
  if (R6::is.R6(df)) {
    return(df$get(key))
  } else if (is.list(df)) {
    return(df[[key]])
  } else {
    stop("Object of class ", class(df), " can't get value currently.")
  }
}
