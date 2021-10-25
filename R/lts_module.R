#' @name longtermstability
#' @aliases longtermstabilityUI
#' @aliases longtermstabilityServer
#'
#' @title longtermstabilityServer.
#'
#' @description Modul for LongTermStability Monitoring as currently (2021) used by Carsten Prinz.
#'
#' @param id Name when called as a module in a shiny app.
#' @return nothing
#'
#' @rdname longtermstability
#' @export
.longtermstabilityUI = function(id) {

  # set up shinyAlert and ns function
  shinyalert::useShinyalert()
  ns <- shiny::NS(id)

  shiny::wellPanel(
    shiny::conditionalPanel(
      condition="output.LTS_fileUploaded == false",
      ns = ns, # namespace of current module
      shiny::fileInput(
        inputId = ns("LTS_input_file"),
        label = shiny::actionLink(inputId = ns("InputHelp"),"Import Excel/RData File"),
        multiple = FALSE,
        accept = c("xls","xlsx","RData")
      ),
      shiny::helpText("Example Table"),
      shiny::img(src = "www/Import_ltsData_FixedFormat.png")
    ),
    shiny::conditionalPanel(
      condition="output.LTS_fileUploaded == true",
      ns = ns, # namespace of current module
      shiny::fluidRow(
        shiny::column(width = 4, DT::dataTableOutput(ns("LTS_vals"))),
        shiny::column(
          width = 8,
          shiny::fluidRow(
            shiny::column(12, DT::dataTableOutput(ns("LTS_def"))),
            style = "margin-bottom:15px;"
          ),
          shiny::fluidRow(
            shiny::column(2, shiny::uiOutput(ns("LTS_sel_KW"))),
            shiny::tags$style(type="text/css", "#lts-Report {margin-top:-1%;}"),
            shiny::column(8, DT::dataTableOutput(ns("LTS_NewVal"))),
            shiny::tags$style(type="text/css", "#lts-LTS_NewVal {margin-top:-2%;}"),
            shiny::column(2, shiny::strong("New Entry"), shiny::p(), shiny::actionButton(inputId = ns("LTS_ApplyNewValue"), label = "Add data")),
            shiny::tags$style(type="text/css", "#lts-LTS_ApplyNewValue {margin-top:-1%;}")
          ),
          shiny::fluidRow(
            shiny::column(2, shiny::strong("Download Report"), shiny::p(), shiny::downloadButton(ns("Report"))),
            shiny::column(2, shiny::strong("Save LTS Data"), shiny::p(), shiny::downloadButton(ns("LTS_Save"), label="Backup")),
            shiny::column(
              width = 6,
              shinyjs::disabled(
                shiny::textInput(
                  inputId = ns("datacomment"),
                  label = "data comment",
                  value = "",
                  placeholder = "select point or row to comment"
                )
              )
            ),
            shiny::column(2, shiny::strong("New Comment"), shiny::p(), shiny::actionButton(inputId = ns("LTS_ApplyNewComment"), label = "Add comment"))
          ),
          shiny::fluidRow(shiny::column(12, shiny::plotOutput(ns("LTS_plot1_1"), height = "450px", click = ns("plot1_click")))),
          shiny::fluidRow(shiny::column(12, shiny::plotOutput(ns("LTS_plot1_2"), height = "450px"))),
          shiny::fluidRow(shiny::column(12, shiny::plotOutput(ns("LTS_plot2"), height = "450px")))
        )
      )
    ) # conditionalPanel
  ) # wellPanel
}

#' @rdname longtermstability
#' @export
.longtermstabilityServer = function(id) {
  shiny::moduleServer(id, function(input, output, session) {

    datalist <- shiny::reactiveValues("lts_data" = NULL, "comment"= NULL)

    LTS_Data <- shiny::reactive({
      if (!is.null(input$LTS_input_file)) {
        file.type <- tools::file_ext(input$LTS_input_file$datapath)
        if (!tolower(file.type) %in% c("rdata","xls","xlsx")) {
          shinyalert::shinyalert(title = "Wrong Filetype?", text = "Please select an RData file or an Excel file.", type = "warning")
          return(NULL)
        }
        if (tolower(file.type)=="rdata") {
            tryCatch({ load(input$LTS_input_file$datapath[1]) }, error = function(e) { stop(shiny::safeError(e)) })
            if (!exists("LTS_dat")) {
              warning("Did load RData backup but could not find object 'LTS_dat' inside.")
              LTS_dat <- NULL
            }
        } else {
          LTS_dat <- read_lts_input(file = input$LTS_input_file$datapath[1])
          # plot(LTS_dat)
          check_validity <- TRUE; i <- 0
          while (check_validity & i < length(LTS_dat)) {
            for (i in 1:length(LTS_dat)) {
              def_cols <- c("RM","KW","KW_Def","KW_Unit","CertVal","U","U_Def","Device","Method","Coef_of_Var","acc_Datasets")
              check_def_cols <- def_cols %in% colnames(LTS_dat[[i]][["def"]])
              val_cols <- c("Value","Date","File")
              check_val_cols <- val_cols %in% colnames(LTS_dat[[i]][["val"]])
              if (!all(check_def_cols)) {
                warn_txt <- paste0("Can't find the following columns in input file", i, " 'definition' part: ", paste(def_cols[!check_def_cols], collapse=", "))
                shinyalert::shinyalert(title = "Warning", text = warn_txt, type = "warning")
                LTS_dat[[i]][["def"]] <- cbind(LTS_dat[[i]][["def"]], as.data.frame(matrix(NA, ncol=sum(!check_def_cols), nrow=1, dimnames=list(NULL,def_cols[!check_def_cols]))))
              }
              if (!all(check_val_cols)) {
                warn_txt <- paste0("Can't find the following columns in input file", i, " 'definition' part: ", paste(val_cols[!check_val_cols], collapse=", "))
                shinyalert::shinyalert(title = "Warning", text = warn_txt, type = "warning")
              }
              if (!"Comment" %in% colnames(LTS_dat[[i]][["val"]])) LTS_dat[[i]][["val"]] <- cbind(LTS_dat[[i]][["val"]], "Comment"=as.character(rep(NA, nrow(LTS_dat[[i]][["val"]]))))
              if (class(LTS_dat[[i]][["val"]][,"Date"])!="Date") {
                LTS_dat[[i]][["val"]][,"Date"] <- as.Date.character(LTS_dat[[i]][["val"]][,"Date"],tryFormats = c("%Y-%m-%d","%d.%m.%Y","%Y/%m/%d"))
              }
              shiny::validate(shiny::need(class(LTS_dat[[i]][["val"]][,"Date"])=="Date", "Sorry, could not convert column 'Date' into correct format."))
              shiny::validate(shiny::need(LTS_dat[[i]][["def"]][,"U_Def"] %in% c("1s","2s","CI","1sx","2sx"), "Sorry, unexpected value in 'U_Def'. Allowed: '1s', '2s', 'CI', '1sx' and '2sx'. Please check."))
              LTS_dat[[i]][["def"]] <- LTS_dat[[i]][["def"]][,def_cols]
              LTS_dat[[i]][["val"]] <- LTS_dat[[i]][["val"]][,c(val_cols,"Comment")]
            }
          }
        }
        return(LTS_dat)
      }
    })

    shiny::observeEvent(LTS_Data(),{
      datalist[["lts_data"]] <- LTS_Data()
    })

    # upload info used in UI part
    output$LTS_fileUploaded <- shiny::reactive({
      return(!is.null(datalist$lts_data))
    })
    shiny::outputOptions(output, 'LTS_fileUploaded', suspendWhenHidden=FALSE)

    LTS_KWs <- shiny::reactive({
      shiny::req(datalist$lts_data)
      sapply(datalist$lts_data, function(x) { x[["def"]][,"KW"] })
    })

    # i() will provide the currently selected KW from the list as a numeric index throughout Server
    i <- shiny::reactiveVal(1)

    output$LTS_sel_KW <- shiny::renderUI({
      shiny::req(LTS_KWs())
      shiny::selectInput(inputId=shiny::NS(id, "LTS_sel_KW"), label="Property", choices=LTS_KWs(), selected=shiny::isolate(i()))
    })

    shiny::observeEvent(input$LTS_sel_KW, {
      i(which(LTS_KWs() %in% input$LTS_sel_KW))
    })

    LTS_new_val <- data.frame("Value"=0.0,
                              "Date"=as.Date(format(Sys.time(), "%Y-%m-%d")),
                              "File"=as.character("filename"),
                              "Comment"=as.character(NA), stringsAsFactors = FALSE)
    LTS_tmp_val <- shiny::reactiveVal(LTS_new_val)

    # Data Tables
    # current LTS values
    tab_LTSvals <- shiny::reactiveVal(shiny::isolate(datalist[["lts_data"]][[i()]][["val"]][,1:3]))
    shiny::observeEvent(i(), {
      # select the current set of values based on i() without showing the comments (to save screen space)
      tab_LTSvals(datalist[["lts_data"]][[i()]][["val"]][,1:3])
    })
    output$LTS_vals <- DT::renderDataTable({
      shiny::req(i())
      # trigger redraw on new value and update reactive Value to this end
      input$LTS_ApplyNewValue
      tab_LTSvals(shiny::isolate(datalist[["lts_data"]][[i()]][["val"]][,1:3]))
      tab_LTSvals()
    }, options = list(paging = TRUE, pageLength = 25, searching = FALSE, stateSave = TRUE), rownames=NULL, server = FALSE, selection = 'single')


    # output$LTS_vals <- DT::renderDataTable({
    #   req(datalist$lts_data, i())
    #   # trigger redraw on new value and update reactive Value to this end
    #   #input$LTS_ApplyNewValue
    #   datalist$lts_data[[i()]][["val"]][,1:3]
    # }, options = list(paging = TRUE, pageLength = 25, searching = FALSE, stateSave = TRUE), rownames=NULL, server = FALSE, selection = 'single')



    # current LTS definition
    output$LTS_def <- DT::renderDataTable({
      shiny::req(datalist$lts_data)
      out <- datalist$lts_data[[i()]][["def"]]
      out[,"Coef_of_Var"] <- formatC(round(out[,"Coef_of_Var"], 4), digits=4, format="f")
      # reorder and rename columns according to wish of Carsten Prinz
      out <- out[,c("RM","KW","KW_Def","KW_Unit","CertVal","U","U_Def","Coef_of_Var","acc_Datasets","Device","Method")]
      colnames(out) <- c("Reference Material","Property","Name","Unit","Certified value","Uncertainty","Uncertainty unit","Coeff. of Variance","accepted Datasets","Device","Method")
      return(out)
    }, options = list(paging = FALSE, searching = FALSE, ordering=FALSE, dom='t'), rownames=NULL)

    # entry table for new datapoint
    output$LTS_NewVal <- DT::renderDataTable({
      LTS_new_val
    }, options = list(paging = FALSE, searching = FALSE, ordering=FALSE, dom='t'), rownames=NULL, editable=TRUE)

    # helper data.frame containing only Month and Value information of current KW
    d <- shiny::reactive({
      x = datalist$lts_data[[i()]]
      vals <- x[["val"]][,"Value"]
      rt <- x[["val"]][,"Date"]
      mon <- mondf(rt)
      data.frame(mon, vals)
    })

    # when a row in table was selected (either by user clicking the table or clicking in the plot)
    shiny::observeEvent(input$LTS_vals_rows_selected, {
      shiny::req(datalist$lts_data)
      if(!is.null(input$LTS_vals_rows_selected)){
        # when a row is selected in table or plot change title and value
        sr <- input$LTS_vals_rows_selected # selected row
        shinyjs::enable(id = "datacomment")
        shiny::updateTextInput(
          session = session,
          inputId = "datacomment",
          label = paste0("Comment for month ", d()[sr,"mon"], " and value ", d()[sr,"vals"]),
          value = datalist[["lts_data"]][[i()]][["val"]][[sr, "Comment"]]
        )
      } else {
        # when row gets deselected/ no row is selected
        shinyjs::disable(id = "datacomment")
        shiny::updateTextInput(
          session = session,
          inputId = "datacomment",
          label  = "Comment",
          value = NA
        )
      }
    }, ignoreNULL = FALSE)

    # Data Figures
    output$LTS_plot1_1 <- shiny::renderPlot({
      shiny::req(datalist[["lts_data"]], i(), d())
      input$LTS_ApplyNewValue
      # $$minor ToDo$$ plot does not need to be updated if only comment was edited --> test for this situation
      # Careful! If user goes to next comment plot needs to be updated again
      plot_lts_data(x = datalist$lts_data[[i()]], type=1)
      ### if a point in data table is selected --> mark in plot 1
      sr <- input$LTS_vals_rows_selected
      tmp <- datalist$lts_data[[i()]][["val"]]
      if (length(sr)) {
       graphics::points(x = rep(mondf(tmp[,"Date"])[sr],2), y = rep(tmp[sr,"Value"],2), pch = c(21,4), cex = 2, col = 5)
      }
    })

    output$LTS_plot1_2 = shiny::renderPlot({
      shiny::req(datalist[["lts_data"]], i())
      input$LTS_ApplyNewValue
      plot_lts_data(x = datalist$lts_data[[i()]], type=2)
    })

    output$LTS_plot2 <- shiny::renderPlot({
      shiny::req(datalist[["lts_data"]], i())
      input$LTS_ApplyNewValue
      tmp <- datalist$lts_data[[i()]]
      if(nrow(tmp[["val"]])>=6) {
        est <- sapply(6:nrow(tmp[["val"]]), function(i) {
          x <- tmp
          x[["val"]] <- x[["val"]][1:i,]
          plot_lts_data(x = x, type=0)
        })
        x <- mondf(tmp[["val"]][,"Date"])
        plot(x=x[-c(1:5)], y=est, xlab="Measurement Point", ylab="LTS month estimate (excluding initital 5 values)", xlim=range(x), pch=24)
      }
    })

    # proxy for changing the table
    proxy = DT::dataTableProxy("LTS_vals")

    #  when clicking on a point in the plot, select Rows in data table proxy
    shiny::observeEvent(input$plot1_click, {
      # 1/3 nearest point to click location
      a <- shiny::nearPoints(d(), input$plot1_click, xvar = "mon", yvar = "vals", addDist = TRUE)
      # 2/3 index in table
      if (nrow(a)>=2) {
        shinyalert::shinyalert(title = "Warning", text = "More than one data point in proximity to click event. Please cross check with table entry if correct data point is selected.", type = "warning")
        a <- a[which.min(a[,"dist_"])[1],]
      }
      idx <- which(d()$mon==a$mon & d()$vals==a$vals)
      # 3/3
      DT::selectRows(proxy = proxy, selected =  idx)
      DT::selectPage(proxy = proxy, page = (idx - 1) %/% input$LTS_vals_state$length + 1)
    })


    # Edit Value/Information of new datapoint
    shiny::observeEvent(input[["LTS_NewVal_cell_edit"]], {
      cell <- input[["LTS_NewVal_cell_edit"]]
      i <- cell$row
      j <- 1+cell$col
      tmp <- LTS_tmp_val()
      tmp[i, j] <- DT::coerceValue(val = cell$value, old = tmp[i, j])
      LTS_tmp_val(tmp)
    })

    # add new value
    shiny::observeEvent(input$LTS_ApplyNewValue, ignoreNULL = TRUE, ignoreInit = TRUE, {
      if (input$LTS_ApplyNewValue>=1) {
        tmp <- datalist$lts_data[[i()]][["val"]]
        nval <- LTS_tmp_val()
        if (nval$Date < max(tmp$Date)) {
          shinyalert::shinyalert(title = "Warning", text = "You added a data point for an earlier date. Resorting the table accordingly.", type = "warning")
          ord <- order(c(tmp$Date, nval$Date))
        } else {
          ord <- 1:(nrow(tmp)+1)
        }
        datalist$lts_data[[i()]][["val"]] <- rbind(tmp, nval)[ord,]
      }
    })

    # add new comment
    shiny::observeEvent(input$LTS_ApplyNewComment, ignoreNULL = TRUE, ignoreInit = TRUE, {
      shiny::req(input$LTS_vals_rows_selected)
      if (input$LTS_ApplyNewComment>=1) {
        # comment input is explicitly triggered now to allow deletion of comments as well as to avoid unnecessary update of the plots
        if (input$datacomment=="") {
          datalist[["lts_data"]][[i()]][["val"]][input$LTS_vals_rows_selected,"Comment"] <- NA
        } else {
          datalist[["lts_data"]][[i()]][["val"]][input$LTS_vals_rows_selected,"Comment"] <- input$datacomment
        }
      }
    })

    # REPORT LTS
    output$Report <- shiny::downloadHandler(
      filename = paste0("LTS_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf" ),
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        rmdfile <- fnc_get_local_file("report_vorlage_lts.Rmd")
        logofile <- fnc_get_local_file("BAMLogo2015.svg")
        font_file1 <- fnc_get_local_file("BAMKlavika-Light.ttf")
        font_file2 <- fnc_get_local_file("BAMKlavika-Medium.ttf")
        font_file3 <- fnc_get_local_file("BAMKlavika-LightItalic.ttf")
        font_file4 <- fnc_get_local_file("BAMKlavika-MediumItalic.ttf")
        
        # Set up parameters to pass to Rmd document
        dat <- datalist[["lts_data"]]
        if (length(dat)>=2 & i()>=2) for (j in rev(1:(i()-1))) dat[j] <- NULL
        params <- list(
          "dat" = dat,
          "logo_file" = logofile,
          "fnc"=list("plot_lts_data"=plot_lts_data)
        )

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(
          input = rmdfile,
          output_file = file,
          output_format = "pdf_document",
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )

    # BACKUP
    output$LTS_Save <- shiny::downloadHandler(
      filename = function() { paste0(datalist$lts_data[[i()]][["def"]][,"RM"], '.RData') },
      content = function(file) {
        # !! save cant handle reactiveVal object properly. has to be written to local variable first
        LTS_dat <- datalist[["lts_data"]]
        save(LTS_dat, file = file)
      },
      contentType = "RData"
    )

    shiny::observeEvent(input$InputHelp, {
      help_the_user("lts_dataupload", modal = TRUE)
    })

  })
}

