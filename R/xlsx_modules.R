#' Title
#'
#' @param id 
#'
#' @return
#' @export
.xlsxinputUI = function(id) {
  shiny::fileInput(
    inputId = NS(id, "file"),
    multiple = TRUE, 
    label = "Test-Upload (.xlsx format)",
    accept = "xlsx"
    )
}

#' XLSX INPUT MODULE SERVER
#' Returns only the input from fileInput
#'
#' @param id 
#'
#' @return
#' @export
.xlsxinputServer = function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # The selected file, if any
    shiny::reactive({
      # If no file is selected, don't do anything
      shiny::req(input$file)
      input$file
    })
  })
}

#' SHEET MODULE UI
#'
#' @param id 
#'
#' @return
#' @export
.sheetUI = function(id) {
  shiny::selectInput(shiny::NS(id, "sheet_sel"), choices = NULL, label = "Sheet")
}

#' SHEET MODULE SERVER
#'
#' @param id 
#' @param datafile 
#'
#' @return
#' @export
.sheetServer = function(id, datafile) {
  stopifnot(is.reactive(datafile))
  # TODO check if datafile is really an excel
  shiny::moduleServer(id, function(input, output, session) {
    #excel-file is uploaded --> update selectInput of available sheets
    shiny::observeEvent(datafile(), { 
      choices_list = load_sheetnames(datafile()$datapath) 
      shiny::updateSelectInput(session = session,
                               inputId = "sheet_sel",
                               choices = choices_list,
                               selected = choices_list[1]
                                )
    })
    s = shiny::reactive(input$sheet_sel)
    # observeEvent(s(),print(paste0("reactive: ", s())))
    return(s)
  })
}


#' Title
#'
#' @param id 
#'
#' @return
#' @export
.ExcelServer = function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    datafile = .xlsxinputServer("xlsxfile")
    sh = .sheetServer("sheet", datafile)
    
    df = debounce(datafile,1000)
    # when sheet is selected, upload Excel and enable button
    t = shiny::eventReactive(df(),{
      #shiny::req(sh())
      # shinyjs::delay(50,{
        l = load_excelfiles(df()$datapath, sh())
        # add file name to data frame
        
        for (i in 1:length(l)) {
          l[[i]][["File"]] = rep(isolate(df()$name[i]), nrow(l[[i]]))
        }
        return(l)
      # })

    })
    # t = debounce(t_tmp,50)
    return(t)
    # observe({print(paste0("t: ", t()))})
  })
}

#' Title
#'
#' @param id 
#'
#' @return
#' @export
.ExcelUI = function(id) {
  shiny::tagList(.xlsxinputUI(id = shiny::NS(id, "xlsxfile")), # upload input
                 .sheetUI(id = shiny::NS(id, "sheet"))) # sheet select
  
  
}

#' Title
#'
#' @param id 
#'
#' @return
#' @export
.parameterUI = function(id) {
  shiny::tagList(shiny::tabsetPanel(
    id = shiny::NS(id, "params"),
    type = "hidden",
    shiny::tabPanel(
      "Certifications",
      shiny::sliderInput(
        shiny::NS(id, "rowslider"),
        "Rows",
        value = c(0, 20),
        min = 0,
        max = 100
      ),
      shiny::sliderInput(
        shiny::NS(id, "colslider"),
        "Columns",
        value = c(0, 1),
        min = 0,
        max = 2
      )
    ),
    shiny::tabPanel("Homogeneity"),
    shiny::tabPanel("Stability")
  ))
}

#' Title
#'
#' @param id 
#' @param dat 
#' @param excelformat 
#'
#' @return
#' @export
.parameterServer = function(id, dat, excelformat) {
  stopifnot(is.reactive(dat))
  shiny::moduleServer(id, function(input, output, session) {
    
    # cd creates a random number everytime data, excel format or rowsliders change
    # so that reactive gets invalidated even with unchanged
    cd = reactiveVal() 
    
    observeEvent(excelformat(), {
      updateTabsetPanel(session = session,
                        inputId = "params",
                        selected = excelformat())
      cd(rnorm(1))
      sliderupdate(session, dat)
    })
    
    # update slider when new data set
    observeEvent(dat(), {
      sliderupdate(session, dat)
      shinyjs::delay(50,cd(rnorm(1))) # this could cause errors
       
    })
    
    observeEvent({
      input$rowslider
      input$colslider
    },{
      cd(rnorm(1))
    })
    
    # TODO validation part here
    
    # returns list with selected additional parameters (if any)
    list(
      change_detector = cd, # generate random number to trigger event even with unchanged inputs
      param_format = reactive(excelformat()),
      start_row = reactive(input$rowslider[1]),
      end_row = reactive(input$rowslider[2]),
      start_col = reactive(input$colslider[1]),
      end_col = reactive(input$colslider[2])
    )
  })
}


#' Title
#'
#' @param id 
#'
#' @return
#' @export
.uploadTabsetsUI = function(id) {
  shiny::fluidRow(shiny::column(id = shiny::NS(id,"leftcol"),width = 4,
                                .ExcelUI(shiny::NS(id, "upld")),
                                .parameterUI(shiny::NS(id, "pam")), ),
                  shiny::column(width = 8,
                                shiny::p("Preview (first 6 lines)"),
                                shiny::verbatimTextOutput(shiny::NS(id, "preview_out"))))
}


#' Title
#'
#' @param id 
#' @param excelformat 
#' @param dat 
#'
#' @return
#' @export
.uploadTabsetsServer = function(id, excelformat, dat) {
  # stopifnot(!is.reactivevalues(dat))
  moduleServer(id, function(input, output, session) {
    t = .ExcelServer("upld") # call module that gives initial table
    # take only the first object of the uploaded excel to create the parameter
    # modules
    param =  .parameterServer("pam", reactive ({t()[[1]]}) ,  excelformat)
    
    # disable upload Panel after upload the corresponding excel file
    observeEvent(excelformat(),{
      if(is.null(dat())){
        shinyjs::enable(id = "leftcol")
      } else if(!is.null(dat())) {
        shinyjs::disable(id = "leftcol")
      } else {
        shinyjs::enable(id = "leftcol")
      }
    })
    
    # selects chosen rows and columns
    a = .computation_preview_data("a", param, t)
    
    # perform minimal validation checks
    prevw = reactive({
      if(excelformat()=="Homogeneity") {
        validate(
          need("analyte" %in% colnames(a()[[1]]), "No column 'analyte' found in input file."),
          need("value" %in% colnames(a()[[1]]), "No column 'value' found in input file.")
        )
        validate(need(is.numeric(a()[[1]][,"value"]), "Column 'value' in input file contains non-numeric values."))
      }  else if(excelformat() == "Certifications"){
        # perform minimal validation tests
        validate(
          #need(is.numeric(a()[[1]][, "value"]), message = "measurement values seem not to be numeric"),
          need(length(a()) >=2, message = "less than 2 laboratory files uploaded. Upload more!")
        )
      }
        a()
      })
      ex = .computation_final_data(id, prevw)
      
      # Preview: when something was uploaded before already, i.e. is in dat(), then 
      # load. Otherwise show head of first excel file
      output$preview_out = renderPrint(
        if(is.null(dat())){
          head(prevw()[[1]])
        } else {
          head(dat())
        })
      
      reactive({
        switch (excelformat(),
                Certifications = ex(),
                Homogeneity = prevw()[[1]]
        )
      })
    })

}

.computation_preview_data = function(id, param, t){
  shiny::moduleServer(id, function(input, output, session){
    # if one parameter gets updated, subset all data frames
     a = eventReactive(param$change_detector(),{
      datlist = isolate(t())
      lapply(datlist, function(x) {
        a = x[as.numeric(param$start_row()):as.numeric(param$end_row()),
          as.numeric(param$start_col()):as.numeric(param$end_col())]
        filename = x$File[as.numeric(param$start_row()):as.numeric(param$end_row())]
        a = cbind(a,File = filename)
        return(a)
      })
    }, ignoreInit = TRUE)
 })
}

.computation_final_data = function(id, a) {
  shiny::moduleServer(id, function(input, output, session){

    reactive({
      b1  = lapply(a(), function(x) {
        laboratory_dataframe(isolate(x))
      })
      c = data.frame(
        "Lab" = rep(paste0("L", seq_along(b1)), times = sapply(b1, nrow)),
        as.data.frame(do.call(rbind, b1)),
        "S_flt" = FALSE,
        "L_flt" = FALSE)
      c <- c[is.finite(c[, "value"]), ] # remove non-finite values

      c <- data.frame("ID" = 1:nrow(c), c)
      return(c)
    })
  })
}


#' Title
#'
#' @param id 
#'
#' @return
#' @export
.ImportCntrlUI = function(id) {
  shiny::tagList(
    shiny::selectInput(
      inputId = shiny::NS(id, "moduleSelect"),
      choices = NULL,
      label = "module",
      width = "50%"
    ),
    .uploadTabsetsUI(shiny::NS(id, "test")),
    shinyjs::disabled(
      shiny::actionButton(inputId = shiny::NS(id, "go"),
                          label = "LOAD"
      )
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
.ImportCntrlServer = function(id, c) {
  stopifnot(is.reactivevalues(c))
  shiny::moduleServer(id, function(input, output, session) {
    shiny::updateSelectInput(inputId = "moduleSelect",
                             session = session,
                             choices =  shiny::isolate(names(c)))
    
    
    # change the reactive if Cert, Homog oder Stab was choosen
    choosen = shiny::eventReactive(input$moduleSelect,
                                   {
                                     get_listelem(c, input$moduleSelect)
                                   }, ignoreInit = TRUE)
    
    t = .uploadTabsetsServer("test", shiny::reactive({input$moduleSelect}), choosen) 
    
    # must be extra disabled after loading, since is in parent module of upload panel
    shiny::observe({
      req(input$moduleSelect)
      # enable upload button when a data frame was uploaded via the Upload menu
      # but only as long c hasn't been filled so far
      # !is.null(data_of_godelement(t())) &&
      if(is.null(get_listelem(c,input$moduleSelect))){ 
        print("go enabled")
        shinyjs::enable("go")
      } else {
        print("go disabled")
        shinyjs::disable("go")
      }
    })
    
    # update list after pushing upload button
    shiny::observeEvent(input$go, {
      shinyjs::disable("go")
      set_listelem(c, input$moduleSelect, t)
      set_listUploadsource(c, input$moduleSelect, uploadsource = "Excel")
      
      
      # TODO "choice" grün färben
      
    })
  })
}


