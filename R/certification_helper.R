analyte_parameter_list = function(d = NULL) {
  if(!is.null(d)){
    stopifnot(is.factor(d[, "analyte"]))
  }
  
  #### create the parameter list for the analytes ###
  # it will contain information about he selected analyte tab, and for
  # each analyte the wanted precision, the filtered sample id, which
  # sample ids are available to be filtered at all and, for completion,
  # the analyte name in case the list name fails
  param_template = list(
    "precision" = NULL, 
    "sample_filter" = NULL, # saving which samples where selected for filter
    "sample_ids" = NULL, # which samples are available for the filter
    "lab_filter" = NULL, # filter of laboratories (e.g. L1)
    "analytename" = NULL
  )
  
  analytes = levels(d[, "analyte"])
  # create list with lists of all analytes (i.e. a nested list)
  a_param_list = rep(list(param_template), length(analytes))
  if(!is.null(d)){
    for (i in 1:length(a_param_list)) {
      # add analyte name to list
      a_param_list[[i]]$analytename = as.list(analytes)[[i]]
      # add available id's of samples to list
      tmp = d
      ids = tmp[tmp[["analyte"]] == as.list(analytes)[[i]], "ID"]
      a_param_list[[i]]$sample_ids = ids[!is.na(ids)] # fill available ids
    }
  }

  # set names of sublists to analyte names
  a_param_list = setNames(a_param_list, analytes)
  l = list("selected_tab" = NULL)
  l$analytes = a_param_list
  apm = do.call("reactiveValues", l) # finally, create reactiveValues
  # end param list
}
