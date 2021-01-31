#' Title
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
xlsxload_ImportCntrlUI = function(id) {
  shiny::tagList(
    shiny::selectInput(
      inputId = shiny::NS(id, "moduleSelect"),
      choices = NULL,
      label = "module",
      width = "50%"
    ),
    xlsxload_uploadTabsetsUI(shiny::NS(id, "test")),
    shiny::actionButton(inputId = shiny::NS(id, "go"),
                        label = "LOAD"#, disabled = FALSE
    ),
  )
}

#' Title
#'
#' @param id 
#' @param c 
#'
#' @return
#' @export
#'
#' @examples
xlsxload_ImportCntrlServer = function(id, c) {
  stopifnot(is.reactivevalues(c))
  shiny::moduleServer(id, function(input, output, session) {
    shiny::updateSelectInput(inputId = "moduleSelect",
                             session = session,
                             choices =  shiny::isolate(names(c)))
    
    
    choosen = shiny::eventReactive(input$moduleSelect,
                                   {
                                     get_listelem(c, input$moduleSelect)
                                   }, ignoreInit = TRUE)
    
    t = xlsxload_uploadTabsetsServer("test", shiny::reactive({input$moduleSelect}), choosen) 
    
    # must be extra disabled after loading, since is in parent module of upload panel
    shiny::observeEvent(input$moduleSelect, {
      #if(is.null(c[[input$moduleSelect]])){
      if(is.null(get_listelem(c,input$moduleSelect))){  
        shinyjs::enable("go")
      } else {
        shinyjs::disable("go")
      }
    }, ignoreInit = TRUE)
    
    # update list ater pushing upload button
    shiny::observeEvent(input$go, {
      
      set_listelem(c, input$moduleSelect,t)
      #c[[input$moduleSelect]] = isolate(t())
      shinyjs::disable("go")
      
      # TODO "choice" grün färben
    })
    
    # builds a reactive expression that only invalidates
    # when the value of input$go becomes out of date
    # (i.e., when the button is pressed)
    # eventReactive(input$go, {
    #   # --> ... 3) return the data list
    #   c
    # })
    
  })
}
