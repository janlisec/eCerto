#' Title
#'
#' @param id 
#' @param d 
#'
#' @return
#' @export
#'
#' @examples
.CertificiationServer = function(id, d) {
  #stopifnot(is.reactivevalues(d))
  moduleServer(id, function(input, output, session) {
    observeEvent(d(), ignoreInit = TRUE,
                 {
                   #if loaded (successfully)
                   if (!is.null(d())) {
                     updateTabsetPanel(session = session,
                                       "certificationPanel",
                                       selected = "loaded")
                     filterServer("cert_filter", d)
                     
                   } else {
                     updateTabsetPanel(session = session,
                                       "certificationPanel",
                                       selected = "standBy")
                   }
                   
                 })
    
  })
}

#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
.CertificationUI = function(id) {
  tabsetPanel(
    id = NS(id, "certificationPanel"),
    type = "hidden",
    # when nothing is loaded
    tabPanel(title = "standby-Panel", value  = "standby", "empty panel content"),
    # when something is loaded
    tabPanel(title = "active-Panel", value = "loaded",
             filterUI(NS(id, "cert_filter")))
    
  )
  
}
