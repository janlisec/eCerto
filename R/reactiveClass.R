#' @title A reactive class based on an R6 object.
#'
#' @description
#' Builds a class, which allows only restricted access to the contained 'reactiveValues'. Elements should be accessed via [getValue()].
#' Possible advantages are that (1) structure of 'reactiveValues' is clear from the beginning (no function like "addVariable" should exist!)
#' and that (2) functions to calculate the mean or plot current data can be implemented here directly.
#'
#' @param rv ReactiveValues object.
#' @param field Field name of R6 object for get and set methods.
#' @param value Value for set method.
#'
#' @name reactiveClass
#'
#' @examples
#' if(interactive()) {
#' rv <- shiny::reactiveValues(a=1)
#' shiny::observeEvent(rv$a, { message("rv$a changed:", rv$a) })
#' shiny:::flushReact()
#' rv$a <- 2
#' shiny:::flushReact()
#' test <- reactiveClass$new(ecerto:::init_rv())
#' ecerto::getValue(test, c("Certifications","data"))
#' shiny::observeEvent(ecerto::getValue(test, "Certifications")$data, {
#'   message("Certifications$data changed:", ecerto::getValue(test, "Certifications")$data)
#' })
#' ecerto::setValue(test, c("Certifications","data"), 5)
#' ecerto::getValue(test, c("Certifications","data"))
#' shiny:::flushReact()
#' }
#'
#' @export

reactiveClass = R6::R6Class(
  classname = "reactiveValuesClass",
  private = list(
    # #' @field reactive_data The 'reactiveValues' object parsed on initialize.
    reactive_data = NULL
  ),
  public = list(
    #' @description
    #' Write the (reactive) value of element 'keys' from list 'l'.
    #' @param rv 'reactiveValues' object.
    #' @return A new 'reactiveClass' object.
    initialize = function(rv){
      # message("Initiate R6 object")
      stopifnot(shiny::is.reactivevalues(rv))
      private$reactive_data = rv
    },
    #' @description
    #' Read the value of field element of R6 object.
    #' @param keys Name of list element.
    #' @return Current value of field.
    get = function(keys=NULL) {

      purrr::chuck(private$reactive_data, !!!keys)
      
    },
    #' @description
    #' Write the value to field element of R6 object.
    #' @param keys Name of list element.
    #' @param value New value.
    #' @return A new 'reactiveClass' object.
    set = function(keys=NULL, value){
      # value needs to be NULL, otherwise pluck() is going to delete the
      # list entry
      if(!is.null(value)) {
        purrr::pluck(private$reactive_data, !!!keys) <- value
      } 
      # if(!is.null(self$get(field))) {
      #   warning(paste0(field, " was ", self$get(field),"; overwritten now with ", x))
      # }
    },
    #' @description
    #' Returns names of all elements of the reactiveValues list stored in the R6 object, except "General"
    #' @return A new 'reactiveClass' object.
    names = function() {
      n = isolate(names(private$reactive_data))
      n[!n == "General"]
    }
  )
)