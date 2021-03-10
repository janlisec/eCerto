#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
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
          tryCatch({ load(input$LTS_input_file$datapath[1]) }, error = function(e) { stop(safeError(e)) })
          if (!exists("LTS_dat")) {
            warning("Did load RData backup but could not find object 'LTS_dat' inside.")
            LTS_dat <- NULL
          }
        # showModal(modalDialog(
        #   title = "Somewhat important message",
        #   "Currently not implemented for RData",
        #   easyClose = TRUE,
        #   footer = NULL
        # ))
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
            # validate(
            #   need(all(check_def_cols), paste0("Can't find the following columns in input file", i, " 'definition' part:", paste(names(check_def_cols)[!check_def_cols],collapse=", "))),
            #   need(all(check_val_cols), paste0("Can't find the following columns in input file", i, " 'data' part:", paste(names(check_val_cols)[!check_val_cols],collapse=", ")))
            # )
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
            validate(need(class(LTS_dat[[i]][["val"]][,"Date"])=="Date", "Sorry, could not convert column 'Date' into correct format."))
            validate(need(LTS_dat[[i]][["def"]][,"U_Def"] %in% c("1s","2s","CI","1sx","2sx"), "Sorry, unexpected value in 'U_Def'. Allowed: '1s', '2s', 'CI', '1sx' and '2sx'. Please check."))
            LTS_dat[[i]][["def"]] <- LTS_dat[[i]][["def"]][,def_cols]
            LTS_dat[[i]][["val"]] <- LTS_dat[[i]][["val"]][,c(val_cols,"Comment")]
          }
        }
       }
      #        assign("LTS_dat", value=LTS_dat, envir = env_perm)

      return(LTS_dat)
      #      } else {
      #        return(getData("LTS_dat"))
      }
    })

    shiny::observeEvent(LTS_Data(),{
      datalist[["lts_data"]] <- LTS_Data()
    })

    output$LTS_fileUploaded <- shiny::reactive({
      return(!is.null(datalist$lts_data))
    })
    shiny::outputOptions(output, 'LTS_fileUploaded', suspendWhenHidden=FALSE)

    LTS_KWs <- shiny::reactive({
      #req(LTS_Data())
      # hier isolate verwendet, damit das input nicht nach Eintrag eines neuen Wertes neu befüllt wird
      # müsste ok sein, da wir den Tabellen upload sowieso deaktivieren sobald erfolgreich
      sapply(isolate(datalist$lts_data), function(x) { x[["def"]][,"KW"] })
    })

    output$LTS_sel_KW <- shiny::renderUI({
      shiny::req(LTS_KWs())
      ns <- session$ns
      shiny::selectInput(inputId=ns("LTS_sel_KW"), label="Property", choices=LTS_KWs(), selected=shiny::isolate(i()))
    })

    # i <- reactive({
    #   req(input$LTS_sel_KW)
    #   which(LTS_KWs() %in% input$LTS_sel_KW)
    # })

    i <- shiny::reactiveVal(1)
    shiny::observeEvent(input$LTS_sel_KW, {
      i(which(LTS_KWs() %in% input$LTS_sel_KW))
    })

    LTS_new_val <- data.frame("Value"=0.0, "Date"=as.Date(format(Sys.time(), "%Y-%m-%d")), "File"=as.character("filename"), "Comment"=as.character(NA), stringsAsFactors = FALSE)
    LTS_tmp_val <- shiny::reactiveVal(LTS_new_val)

    # Data Tables
    tab_LTSvals <- shiny::reactiveVal(isolate(datalist[["lts_data"]][[i()]][["val"]][,1:3]))
    shiny::observeEvent(i(), {tab_LTSvals(datalist[["lts_data"]][[i()]][["val"]][,1:3])})
    #observeEvent(input$LTS_ApplyNewValue, {tab_LTSvals(datalist[["lts_data"]][[i()]][["val"]][,1:3])})
    output$LTS_vals <- DT::renderDataTable({
      # req(LTS_Data())
      req(i())
      input$LTS_ApplyNewValue
      tab_LTSvals(isolate(datalist[["lts_data"]][[i()]][["val"]][,1:3]))
      tab_LTSvals()
      # getData("LTS_dat")[[i()]][["val"]]
    }, options = list(paging = TRUE, pageLength = 25, searching = FALSE, stateSave = TRUE), rownames=NULL, server = FALSE, selection = 'single')

    output$LTS_def <- DT::renderDataTable({
      req(datalist$lts_data)
      out <- datalist$lts_data[[i()]][["def"]]
      out[,"Coef_of_Var"] <- formatC(round(out[,"Coef_of_Var"], 4), digits=4, format="f")
      # reorder and rename columns according to wish of Carsten Prinz
      out <- out[,c("RM","KW","KW_Def","KW_Unit","CertVal","U","U_Def","Coef_of_Var","acc_Datasets","Device","Method")]
      colnames(out) <- c("Reference Material","Property","Name","Unit","Certified value","Uncertainty","Uncertainty unit","Coeff. of Variance","accepted Datasets","Device","Method")
      return(out)
    }, options = list(paging = FALSE, searching = FALSE, ordering=FALSE, dom='t'), rownames=NULL)

    output$LTS_NewVal <- DT::renderDataTable({
      LTS_new_val
    }, options = list(paging = FALSE, searching = FALSE, ordering=FALSE, dom='t'), rownames=NULL, editable=TRUE)

    d = shiny::reactive({
      x = datalist$lts_data[[i()]]
      # x = getData("LTS_dat")[[i()]]
      vals <- x[["val"]][,"Value"]
      rt <- x[["val"]][,"Date"]
      mon <- sapply(rt, function(x) { mondf(d_start = rt[1], d_end = x) })
      data.frame(mon, vals)
    })

    # when a row in table was selected
    shiny::observeEvent(input$LTS_vals_rows_selected, {
      shiny::req(datalist$lts_data, datalist)
      if(!is.null(input$LTS_vals_rows_selected)){
        # when a row is selected
        e = datalist[["lts_data"]][[i()]][["val"]][[input$LTS_vals_rows_selected,"Comment"]]
        s = input$LTS_vals_rows_selected # selected row
        shinyjs::enable(id = "datacomment")
        # change title when value was selected in table or plot
        shiny::updateTextInput(
          session = session,
          inputId = "datacomment",
          label = paste0("data comment for month ",
            d()[s,"mon"],
            " and value ",
            d()[s,"vals"]
            )
        )
      } else {
        # when row gets deselected/ no row is selected
        e = NA
        shinyjs::disable(id = "datacomment")
        shiny::updateTextInput(
          session = session,
          inputId = "datacomment",
          label  = paste0(
            "data comment"
          )
        )
      }
      # when a value was entered before already
      shiny::updateTextInput(session,"datacomment", value = e) # clear textInput when deselected
    }, ignoreNULL = FALSE)


    # create list for populating each KW with comments (2 steps)
    # d_NAvec = reactive({
    #   req(datalist$lts_data)
    #   commentlist = list()
    #   # of length of list [[]]
    #   for (i in 1:length(datalist$lts_data)) {
    #     #if (datalist$lts_data[[i]]["val"])
    #     commentlist[[i]] = rep(NA, nrow(d()))
    #   }
    #   return(commentlist)
    # })
    # observe({ datalist$comment = d_NAvec() })

    # when some comment was entered, save in reactivevalues
    shiny::observeEvent(input$datacomment, {
      shiny::req(input$LTS_vals_rows_selected)
      if(input$datacomment != "" ){
        # for some reason, after switching from commented row to another, shiny gives ""
        # therefore this "if" condition is necessary to check
        datalist[["lts_data"]][[i()]][["val"]][input$LTS_vals_rows_selected,"Comment"] <- input$datacomment
      }
    })

    # Data Figures
    output$LTS_plot1_1 <- shiny::renderPlot({
      shiny::req(datalist[["lts_data"]], i(), d())
      input$LTS_ApplyNewValue
      s <- input$LTS_vals_rows_selected
      #c <- datalist[["lts_data"]][[i()]][["val"]][,"Comment"]
      plot_lts_data(x = datalist$lts_data[[i()]], type=1)
      ### select a point in data table and mark in plot -- plot 1 --
      if (length(s)) points(x = d()[s,"mon"], y = d()[s,"vals"], pch = 19, cex = 2)
      ### select a point in data table and mark in plot -- plot 1 -- end
      ### change color of triangle for commented points
      #if(sum(!is.na(c))>=1) points(x = d()[!is.na(c),"mon"],y = d()[!is.na(c),"vals"], pch=24, bg="red")
    })

    output$LTS_plot1_2 = shiny::renderPlot({
      shiny::req(datalist[["lts_data"]], i())
      input$LTS_ApplyNewValue
      #c <- datalist[["lts_data"]][[i()]][["val"]][,"Comment"]
      plot_lts_data(x = datalist$lts_data[[i()]], type=2)
      # I outcommented this one because y-data get adjusted in the plotting function such that d() is not correct anymore
      # $ToDo --> modify plotting functions to react on comments if present
      # if(sum(!is.na(c))>=1) points(x = d()[!is.na(c),"mon"],y = d()[!is.na(c),"vals"], pch=21, bg="red")
    })

    output$LTS_plot2 <- shiny::renderPlot({
      shiny::req(datalist[["lts_data"]], i(),input$LTS_ApplyNewValue)
      input$LTS_ApplyNewValue
      #c <- datalist[["lts_data"]][[i()]][["val"]][,"Comment"]
      # c = c[-c(1:3)]
      input$LTS_ApplyNewValue
      tmp <- datalist$lts_data[[i()]]
      if(nrow(tmp[["val"]])>4) {
        est <- sapply(4:nrow(tmp[["val"]]), function(i) {
          x <- tmp; x[["val"]] <- x[["val"]][1:i,]
          plot_lts_data(x = x, type=0)
        })
        plot(x=tmp[["val"]][-c(1:3),"Date"], y=est, xlab="Measurement Point", ylab="LTS month estimate")
        #if(sum(!is.na(c))>=1) points(x = tmp[["val"]][!is.na(c),"Date"],y = est[!is.na(c)], pch=24, bg="red")
      }
    })

    # proxy for changing the table
    proxy = DT::dataTableProxy("LTS_vals")

    #  when clicking on a point in the plot, select Rows in data table proxy
    shiny::observeEvent(input$plot1_click, {
      # 1/3 nearest point to click location
      a = shiny::nearPoints(d(), input$plot1_click, xvar = "mon", yvar = "vals", addDist = TRUE)
      # 2/3 index in table
      #print(a)
      if (nrow(a)>=2) {
        shinyalert::shinyalert(title = "Warning", text = "More than one data point in proximity to click event. Please cross check with table entry if correct data point is selected.", type = "warning")
        a <- a[which.min(a[,"dist_"])[1],]
      }
      ind = which(d()$mon==a$mon & d()$vals==a$vals)
      # 3/3
      DT::selectRows(proxy = proxy, selected =  ind)
      DT::selectPage(
        proxy = proxy,
        page = (ind - 1) %/% input$LTS_vals_state$length + 1)
    })


    #Add/Remove Value
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
        datalist$lts_data[[i()]][["val"]] <- rbind(datalist$lts_data[[i()]][["val"]], LTS_tmp_val())
      }
    })

    # REPORT LTS
    output$Report <- shiny::downloadHandler(
      filename = paste0("LTS_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf" ),
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        #browser()
        rmdfile = system.file("rmd", "LTSreport.Rmd", package = "ecerto")[1]
        # keep default option for no package/online version
        if (rmdfile=="") rmdfile <- "R/LTSreport.Rmd"
        logo_file = system.file("rmd", "BAMLogo2015.svg", package = "ecerto")[1]
        # keep default option for no package/online version
        #if (logo_file=="") logo_file <- "R/BAMLogo2015.svg"
        if (logo_file=="") logo_file <- "C:/Users/jlisec/Rpackages/Rpackage_eCerto/ecerto/inst/rmd/BAMLogo2015.svg"

        tempReport <- file.path(tempdir(), "LTSreport.Rmd")
        file.copy(rmdfile, tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list("dat" = datalist[["lts_data"]], "logo_file" = logo_file)

        # das hat bei mir zum download (und der nicht erfolgreichen Installation) von tinytech geführt
        # für das online tool brauchen wir das nicht (shiny server kümmert sich)
        # für die app würde ich der installation eine MsgBox vorschalten ob der Nutzer das möchte (ich habe z.B. bereits ein MikTeX drauf und kann auf tiny verzichten)
        #if (tinytex::tinytex_root() == "") tinytex::install_tinytex()

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )

    # BACKUP
    output$LTS_Save <- shiny::downloadHandler(
      filename = function() { paste0(datalist$lts_data[[i()]][["def"]][,"RM"], '.RData') },
      content = function(file) {
       # LTS_dat <- getData("LTS_dat")
        LTS_dat <- datalist[["lts_data"]]
        # !! save cant handle reactiveVal object properly. has to be written to local variable first
        save(LTS_dat, file = file)
      },
      contentType = "RData"
    )


  })
}

#' User Interface of LTS module
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
.longtermstabilityUI = function(id) {

  # set up shinyAlert
  shinyalert::useShinyalert()

  wellPanel(
     conditionalPanel(
       condition="output.LTS_fileUploaded == false",
       ns = NS(id), # namespace of current module
      fileInput(
        inputId = NS(id,"LTS_input_file"),
        label = "Import Excel/RData File",
        multiple = FALSE,
        accept = c("xls","xlsx","RData")
        ),
       helpText("Example Table"),
       imageOutput(NS(id,"myImage11a"), inline = TRUE),
     ),
    conditionalPanel(
      condition="output.LTS_fileUploaded == true",
      ns = NS(id), # namespace of current module
      fluidRow(
        column(4, DT::dataTableOutput(NS(id,"LTS_vals"))),
        column(
          8,
          fluidRow(
            column(12, DT::dataTableOutput(NS(id,"LTS_def"))),
            style = "margin-bottom:15px;"
            ),
          fluidRow(
            column(2, uiOutput(NS(id,"LTS_sel_KW"))),
            column(2, strong("Save LTS Data"), p(),downloadButton(NS(id,"LTS_Save"), label="Backup")),
            tags$style(type="text/css", "#LTS_Save {margin-top:-3%;}"),
            column(2, strong("Download Report"), p(), downloadButton(NS(id,"Report"))),
            tags$style(type="text/css", "#Report {margin-top:-1%;}"),
            column(4, DT::dataTableOutput(NS(id,"LTS_NewVal"))),
            tags$style(type="text/css", "#LTS_NewVal {margin-top:-3%;}"),
            column(2, strong("New Entry"), p(), actionButton(inputId = NS(id,"LTS_ApplyNewValue"), label = "Add data")),
            tags$style(type="text/css", "#LTS_ApplyNewValue {margin-top:-1%;}")
          ),
          #verbatimTextOutput(NS(id,"click_info")),
          fluidRow(
            column(
              6,
              shinyjs::disabled(
                textInput(
                  inputId = NS(id,"datacomment"),
                  label = "data comment",
                  value = "",
                  placeholder = "select point or row to comment"
                  )
                )
            )
          ),
          fluidRow(column(12, plotOutput(NS(id,"LTS_plot1_1"), height = "450px", click = NS(id,"plot1_click")))),
          fluidRow(column(12, plotOutput(NS(id,"LTS_plot1_2"), height = "450px"))),
          fluidRow(column(12, plotOutput(NS(id,"LTS_plot2"), height = "450px")))
        )
      )
    ) # conditionalPanel
  ) # wellPanel
}
