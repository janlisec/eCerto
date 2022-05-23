#'@title A reactive class based on an R6 object.
#'@description Builds a class, which allows only restricted access to the
#'    contained 'reactiveValues'. Elements should be accessed via [getValue()].
#'    Possible advantages are that (1) structure of 'reactiveValues' is clear
#'    from the beginning (no function like "addVariable" should exist!) and that
#'    (2) functions to calculate the mean or plot current data can be implemented
#'    here directly.#'
#'@param rv ReactiveValues object.
#'@param field Field name of R6 object for get and set methods.
#'@param value Value for set method.
#'@name eCerto_R6Class
#'@examples
#' if(interactive()) {
#' rv <- shiny::reactiveValues(a=1)
#' shiny::observeEvent(rv$a, { message("rv$a changed:", rv$a) })
#' shiny:::flushReact()
#' rv$a <- 2
#' shiny:::flushReact()
#' test <- eCerto$new(eCerto:::init_rv())
#' eCerto::getValue(test, c("Certification","data"))
#' shiny::observeEvent(eCerto::getValue(test, "Certification")$data, {
#'   message("Certification$data changed:", eCerto::getValue(test, "Certification")$data)
#' })
#' eCerto::setValue(test, c("Certification","data"), 5)
#' eCerto::getValue(test, c("Certification","data"))
#' shiny:::flushReact()
#' }
#'@export
eCerto <- R6::R6Class(
  classname = "eCerto",
  private = list(
    # #' @field reactive_data The 'reactiveValues' object parsed on initialize.
    ..eData = NULL
  ),
  public = list(

    #' @description
    #' Write the (reactive) value of element 'keys' from list 'l'.
    #' @param rv 'reactiveValues' object.
    #' @return A new 'eCerto' object.
    initialize = function(rv) {
      # message("Initiate R6 object")
      # stopifnot(shiny::is.reactivevalues(rv))
      if (missing(rv)) {
        # set up internal data structure and fill with test data
        rv <- init_rv()
        testdata <- test_Certification_Excel()
        rv[["Certification"]][["data"]] <- testdata
        rv[["General"]][["apm"]] <- init_apm(testdata)
        rv[["General"]][["materialtabelle"]] <- init_materialtabelle(sapply(init_apm(testdata),function(x){x[["name"]]}))
        private$..eData = rv
      } else {
        # [ToDo] implement testing (copy from RData upload module)
        private$..eData = rv
      }
    },

    #' @description Read the value of field element of R6 object.
    #' @param keys Name of list element.
    #' @return Current value of field.
    get = function(keys=NULL) {
      purrr::chuck(private$..eData, !!!keys)
    },

    #' @description Write the value to field element of R6 object.
    #' @param keys Name of list element.
    #' @param value New value.
    #' @return New value of field (invisible).
    set = function(keys=NULL, value){
      if(!is.null(value)) {
        purrr::pluck(private$..eData, !!!keys) <- value
      } else {
        # assigning NULL to a normal list element will delete it (and not replace its content by NULL)
        # however, most elements of an eCerto R6 object are reactiveValue items --> here,
        # the behavior is different: one can assign NULL to an reactiveValue-Item BUT (!!!)
        # assigning NULL to the reactiveValue-List itself would delete it
        # this case needs to be taken care of here
        if (length(keys)>=2 && is.reactivevalues(purrr::pluck(private$reactive_data, !!!keys[-length(keys)]))) {
          purrr::pluck(private$reactive_data, !!!keys) <- NULL
        } else {
          warning(paste("Can't assign NULL to a standard list element: ", paste(keys, collapse=", ")))
        }
      }
      invisible(purrr::chuck(private$..eData, !!!keys))
    },

    #' @description Plot the certification data either provided by the user or from the private slot of self.
    #' @param data data.frame containing columns 'value', 'Lab' and 'L_flt'.
    #' @param annotate_id T/F to overlay the plot with ID as text if column 'ID' is present.
    #' @param filename_labels T/F to use imported file names as labels on x-axes.
    #' @return A plot.
    c_plot = function(data, annotate_id = FALSE, filename_labels = FALSE) {
      if (missing(data)) {
        data <- isolate(private$..eData[["Certification"]][["data"]])
      }
      eCerto::CertValPlot(data = data, annotate_id = annotate_id, filename_labels = filename_labels)
    }
  )
)
