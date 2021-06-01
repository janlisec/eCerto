# builds a class, which allows only restricted access to the contained
# reactiveValues. 
# elements should be accessed via [getValue()]
# 1) structure of reactiveValues is clear from the beginning (no
#   function like "addVariable" should exist!) 
# 2) functions to calculate, e.g., the mean can be implemented
reactiveClass = R6::R6Class(
  classname = "reactiveValuesClass",
  private = list(
    reactive_data = NULL
  ),
  public = list(
    initialize = function(rv){
      message("Initiate R6 object")
      stopifnot(shiny::is.reactivevalues(rv))
      private$reactive_data = rv
    },
    set = function(field=NULL,value){
      set_nested_list(private$reactive_data,field,value)
      # if(!is.null(self$get(field))) {
      #   warning(paste0(field, " was ", self$get(field),"; overwritten now with ", x))
      # }
    },
    get = function(field=NULL) {
      access_nested_list(private$reactive_data,field)
    },
    names = function() {
      isolate(names(private$reactive_data))
    }
  )
)
