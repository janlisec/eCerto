#' #' @rdname .xlsxinputServer
#' .xlsxinputUI = function(id) {
#'   shiny::fileInput(
#'     inputId = NS(id, "file"),
#'     multiple = TRUE,
#'     label = "Test-Upload (.xlsx format)",
#'     accept = "xlsx"
#'     )
#' }
#'
#' #' XLSX INPUT MODULE SERVER
#' #' Returns only the input from fileInput, which contains information about
#' #' the file path and their names
#' #'
#' #' @param id
#' #'
#' #' @return
#' #' @export
#' .xlsxinputServer = function(id) {
#'   shiny::moduleServer(id, function(input, output, session) {
#'
#'     # The selected file, if any
#'     r = shiny::reactive({
#'       # If no file is selected, don't do anything
#'       shiny::req(input$file)
#'       input$file
#'     })
#'   })
#' }
#'
#' #' @rdname .sheetServer
#' .sheetUI = function(id) {
#'   shiny::selectInput(shiny::NS(id, "sheet_sel"), choices = NULL, label = "Sheet")
#' }
#'
#' #' SHEET MODULE SERVER
#' #'
#' #' @param id
#' #' @param datafile
#' #'
#' #' @return
#' #' @export
#' .sheetServer = function(id, datafile) {
#'   stopifnot(is.reactive(datafile))
#'   # TODO check if datafile is really an excel
#'   shiny::moduleServer(id, function(input, output, session) {
#'     #excel-file is uploaded --> update selectInput of available sheets
#'     shiny::observeEvent(datafile(), {
#'
#'       choices_list = load_sheetnames(datafile()$datapath)
#'       shiny::updateSelectInput(session = session,
#'                                inputId = "sheet_sel",
#'                                choices = choices_list,
#'                                selected = choices_list[1]
#'                                 )
#'     })
#'     s = shiny::reactive(input$sheet_sel)
#'
#'     # data file should wait until sheetnames are loaded properly
#'     # This should be only temporarily, since it slows down the uploading
#'     # mechanism. Alternatively, (1) experiment with 'priority' argument of
#'     # observe() or (2) extra argument
#'     s2 = debounce(s, 500)
#'     return(s2)
#'   })
#' }
#'
#' #' @rdname .ExcelServer
#' .ExcelUI = function(id) {
#'   shiny::tagList(
#'     .xlsxinputUI(id = shiny::NS(id, "xlsxfile")), # upload input
#'                  .sheetUI(id = shiny::NS(id, "sheet"))
#'     ) # sheet select
#' }
#'
#' #' Excel Module
#' #' Receives sheet and path/name of datafiles and finally uploads
#' #' the Excel file with load_excelfiles(). Returns only raw uploaded
#' #' Excel tables.
#' #'
#' #' @param id
#' #'
#' #' @return the raw and not-yet-cropped preview file
#' .ExcelServer = function(id) {
#'   shiny::moduleServer(id, function(input, output, session) {
#'     rv <- reactiveValues(v = 0)
#'
#'     datafile = .xlsxinputServer("xlsxfile")
#'     sh = .sheetServer("sheet", datafile)
#'
#'     # when sheet gets uploaded
#'     observeEvent(sh(),{
#'       print("sheet updated")
#'       rv$v <- rv$v + 1 # invalidate 'df' reactive
#'     }, ignoreNULL = TRUE, ignoreInit = TRUE)
#'
#'     df = reactive({
#'       req(sh())
#'       rv$v # invalidates 'df' when sheet was uploaded
#'       isolate(datafile())
#'     })
#'
#'     # when sheet is selected, upload Excel and enable button
#'     t = shiny::eventReactive(df(),{
#'       message("uploading excel ",df()$name, " and sheet ", isolate(sh()))
#'         l = load_excelfiles(df()$datapath, sh())
#'         # add file name to data frame
#'         for (i in 1:length(l)) {
#'           l[[i]][["File"]] = rep(isolate(df()$name[i]), nrow(l[[i]]))
#'         }
#'         return(l)
#'     })
#'
#'     return(t)
#'   })
#' }
#'
#' #' @rdname .parameterServer
#' .parameterUI = function(id) {
#'   shiny::tagList(shiny::tabsetPanel(
#'     id = shiny::NS(id, "params"),
#'     type = "hidden",
#'     shiny::tabPanel(
#'       "Certifications",
#'       shiny::sliderInput(
#'         shiny::NS(id, "rowslider"),
#'         "Rows",
#'         value = c(0, 20),
#'         min = 0,
#'         max = 100
#'       ),
#'       shiny::sliderInput(
#'         shiny::NS(id, "colslider"),
#'         "Columns",
#'         value = c(0, 1),
#'         min = 0,
#'         max = 2
#'       )
#'     ),
#'     shiny::tabPanel("Homogeneity"),
#'     shiny::tabPanel("Stability")
#'   ))
#' }
#'
#' #' PARAMETER MODULE
#' #' contains and returns the selected rows and columns
#' #'
#' #' @param id
#' #' @param dat
#' #' @param excelformat
#' #'
#' #' @return
#' #' @export
#' .parameterServer = function(id, dat, excelformat) {
#'   stopifnot(is.reactive(dat))
#'   shiny::moduleServer(id, function(input, output, session) {
#'
#'     # cd creates a random number everytime data, excel format or rowsliders change
#'     # so that reactive gets invalidated even with unchanged
#'     cd = reactiveVal()
#'
#'     observeEvent(excelformat(), {
#'       updateTabsetPanel(session = session,
#'                         inputId = "params",
#'                         selected = excelformat())
#'       cd(rnorm(1))
#'       sliderupdate(session, dat)
#'     })
#'
#'     # update slider when new data set
#'     observeEvent(dat(), {
#'       sliderupdate(session, dat)
#'       shinyjs::delay(50,cd(rnorm(1))) # this could cause errors
#'
#'     })
#'
#'     observeEvent({
#'       input$rowslider
#'       input$colslider
#'     },{
#'       cd(rnorm(1))
#'     })
#'
#'     # TODO validation part here?
#'
#'     # returns list with selected additional parameters (if any)
#'     list(
#'       change_detector = cd, # generate random number to trigger event even with unchanged inputs
#'       param_format = reactive(excelformat()),
#'       start_row = reactive(input$rowslider[1]),
#'       end_row = reactive(input$rowslider[2]),
#'       start_col = reactive(input$colslider[1]),
#'       end_col = reactive(input$colslider[2])
#'     )
#'   })
#' }

#'
#' #' @rdname .uploadTabsetsServer
#' .uploadTabsetsUI = function(id) {
#'   tagList(
#'     fileInput(multiple = TRUE, inputId = shiny::NS(id,"excel_file"), label = "excel_file (xlsx)", accept = "xlsx"),
#'     numericInput(inputId = shiny::NS(id,"sheet_number"), label = "sheet_number", value = 1),
#'     hr(),
#'     xlsx_range_select_UI(shiny::NS(id,"test"))
#'   )
#' }
#'
#'
#' #' Module that receives and prints the preview, tests for invalidates and
#' #' also computes the final formatted dataset
#' #'
#' #' @param id
#' #' @param excelformat
#' #' @param dat
#' #'
#' #' @return
#' #' @export
#' .uploadTabsetsServer = function(id, excelformat) {
#'   # stopifnot(!is.reactivevalues(dat))
#'   moduleServer(id, function(input, output, session) {
#'
#'     out <- xlsx_range_select_Server(id = "test",
#'                                     x = reactive({ input$excel_file }),
#'                                     sheet = reactive({ input$sheet_number })
#'     )
#'
#'     a = reactive({out$tab_flt})
#'
#'
#'     # # disable upload Panel after upload the corresponding excel file
#'     # observeEvent(excelformat(),{
#'     #   if(is.null(dat())){
#'     #     shinyjs::enable(id = "leftcol")
#'     #   } else if(!is.null(dat())) {
#'     #     shinyjs::disable(id = "leftcol")
#'     #   } else {
#'     #     shinyjs::enable(id = "leftcol")
#'     #   }
#'     # })
#'
#'     # perform minimal validation checks
#'     prevw = reactive({
#'       if(excelformat()=="Homogeneity") {
#'         validate(
#'           need("analyte" %in% colnames(a()[[1]]), "No column 'analyte' found in input file."),
#'           need("value" %in% colnames(a()[[1]]), "No column 'value' found in input file.")
#'         )
#'         validate(need(is.numeric(a()[[1]][,"value"]), "Column 'value' in input file contains non-numeric values."))
#'       }  else if(excelformat() == "Certifications"){
#'         # perform minimal validation tests
#'         validate(
#'           # TODO
#'           #need(is.numeric(a()[[1]][, "value"]), message = "measurement values seem not to be numeric"),
#'           need(length(a()) >=2, message = "less than 2 laboratory files uploaded. Upload more!")
#'         )
#'       }
#'       a()
#'     })
#'     ex = .computation_final_data(id, prevw)
#'
#'     reactive({
#'       switch (excelformat(),
#'               Certifications = ex(),
#'               Homogeneity = prevw()[[1]]
#'       )
#'     })
#'   })
#'
#' }

# .computation_preview_data = function(id, param, t){
#
#   shiny::moduleServer(id, function(input, output, session){
#     # if one parameter gets updated, subset all data frames
#      # a = eventReactive(param$change_detector(),{
#      # a = reactive({
#       # datlist = isolate(t())
#     if(is.reactive(t)){
#       datlist = isolate(t())
#     } else {
#       datlist = t
#     }
#
#       lapply(datlist, function(x) {
#         a = x[as.numeric(param$start_row):as.numeric(param$end_row),
#           as.numeric(param$start_col):as.numeric(param$end_col)]
#         # in case column "File" has been excluded by the row and column
#         # selection add it now again
#
#
#         return(a)
#       })
#     # })
#  })
# }

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


#' @rdname .ExcelUploadControllServer
.ExcelUploadControllUI = function(id) {
  shiny::tagList(
    # --- --- --- --- --- --- --- --- ---
    # .uploadTabsetsUI(shiny::NS(id, "uploadTabset")),
    fluidRow(
      column(3, uiOutput(outputId = shiny::NS(id,"excel_file"))),
      #column(3, fileInput(multiple = TRUE, inputId = shiny::NS(id,"excel_file"), label = "Excel (xlsx)", accept = "xlsx")),
      column(3, numericInput(inputId = shiny::NS(id,"sheet_number"), label = "sheet_number", value = 1)),
      column(6, align="right", uiOutput(outputId = shiny::NS(id,"btn_load")))
    ),
    xlsx_range_select_UI(shiny::NS(id,"Upload")),
  )
}

#' Here you can select the Ecerto-Mode (Certification, Homogeneity, Stability)
#' and, until now, contains the Upload button
#'
#' @param id
#' @param rv reactiveValues
#'
#' @return
#' @export
.ExcelUploadControllServer = function(id, excelformat, dat) {
  # stopifnot(is.reactivevalues(rv))
  stopifnot(is.reactive(excelformat))
  shiny::moduleServer(id, function(input, output, session) {

    output$btn_load <- renderUI({
      fluidRow(
        strong("Click to load"),
        br(),
        shiny::actionButton(inputId = shiny::NS(id, "go"), label = "LOAD")
      )
    })

    output$excel_file <- renderUI({
      #browser()
      if (is.null(dat())) {
        updateNumericInput(session = session, inputId = "sheet_number", value=1)
        shinyjs::hideElement(id = "sheet_number")
        shinyjs::hideElement(id = "btn_load")
        x(NULL)
        fileInput(multiple = excelformat()=="Certifications", inputId = shiny::NS(id,"excel_file"), label = "Select Excel (xlsx)", accept = "xlsx")
      } else {
        HTML("Already uploaded")
      }
    })

    x <- reactiveVal(NULL)

    observeEvent(input$excel_file, {
      sheetnames <- load_sheetnames(input$excel_file$datapath)
      if (length(sheetnames)>1) {
        updateNumericInput(session = session, inputId = "sheet_number", value=1, min=1, max=length(sheetnames), step=1)
        shinyjs::showElement(id = "sheet_number")
      }
      shinyjs::showElement(id = "btn_load")
      x(input$excel_file)
    })

    out <- xlsx_range_select_Server(
      id = "Upload",
      #x = reactive({ input$excel_file }),
      x = x,
      sheet = reactive({ input$sheet_number })
    )

    a = reactive({ out$tab_flt })


    # # disable upload Panel after upload the corresponding excel file
    # observeEvent(excelformat(),{
    #   if(is.null(dat())){
    #     shinyjs::enable(id = "leftcol")
    #   } else if(!is.null(dat())) {
    #     shinyjs::disable(id = "leftcol")
    #   } else {
    #     shinyjs::enable(id = "leftcol")
    #   }
    # })

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
          # TODO
          #need(is.numeric(a()[[1]][, "value"]), message = "measurement values seem not to be numeric"),
          need(length(a()) >=2, message = "less than 2 laboratory files uploaded. Upload more!")
        )
      }
      a()
    })
    fd = .computation_final_data(id, prevw)

    t = reactive({
      switch (excelformat(),
              Certifications = fd(),
              Homogeneity = prevw()[[1]]
      )
    })
    # --- --- --- --- --- --- --- --- ---

    # must be extra disabled after loading, since is in parent module of upload panel
    shiny::observe({
      req(excelformat())
      # enable upload button when a data frame was uploaded via the Upload menu
      # but only as long dat hasn't been filled so far !is.null((dat()))
      # NOTE: In the future, this and dat() parameter could be obsolete, when whole
      # panel gets deactivated
      if(is.null(dat())){
        message("go enabled")
        shinyjs::enable("go")
      } else {
        message("go disabled")
        shinyjs::disable("go")
      }
    })

    # update list after pushing upload button
    ex = eventReactive(input$go, {
      req(t())
      validate(need(!is.null(t()), "Load-Button disabled, data has not been uploaded"))
      shinyjs::disable("go")
      message("go clicked")

      t()
      # TODO "choice" grün färben

    })

    return(ex)
  })
}


