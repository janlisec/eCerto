shinyServer(function(input, output, session) {
  # helper functions (statistics, plotting and report_dummy)
  source("BAMTool_functions.R", local = TRUE)
  
  # permanent environment to store imported and modified data to allow ...
  # processing of multiple analytes
  env_perm <- new.env()
  
  # function checks if object exists in environment and returns it
  getData <- function(x = " ", e = env_perm) {
    if (exists(x, envir = e)) {
      return(get(x, envir = e))
    } else {
      return(NULL)
    }
  }
  
  # =============================================================
  # DATENIMPORT
  # =============================================================
  
  # TODO outsource server code for module certification to a separate file ####
  # import all selected files and update/make reactive
  c_Data <- reactive({
    #cat(file=stderr(), "c_Data called\n")
    
    # if both input options (.RData and Excel) are NULL ...
    # remove permanent Certification objects ...
    # from memory if in backup file no Certification data is found
    if (is.null(input$c_input_files) & is.null(input$in_file_ecerto_backup)) {
      if (exists("dat", envir = env_perm))
        rm("dat", envir = env_perm)
      if (exists("cert_vals", envir = env_perm))
        rm("cert_vals", envir = env_perm)
      if (exists("normality_statement", envir = env_perm))
        rm("normality_statement", envir = env_perm)
      if (exists("c_upload_statement", envir = env_perm))
        rm("c_upload_statement", envir = env_perm)
      return(NULL)
    }
    # test if backup file contains certification data
    cert_data_in_backup_file <- FALSE
    if (!is.null(input$in_file_ecerto_backup)) {
      # load from RData backup
      file.type <-
        tools::file_ext(input$in_file_ecerto_backup$datapath)
      validate(need(
        length(file.type) == 1 &
          tolower(file.type) == "rdata",
        "Please select only one RData file."
      ))
      file.type <- "RData"
      tryCatch({
        load(input$in_file_ecerto_backup$datapath[1])
      }, error = function(e) {
        stop(safeError(e))
      })
      if (!is.null(res[["Certification"]]))
        cert_data_in_backup_file <- TRUE
    }
    # if backup RData is present AND contains a previous Certification list ...
    # than proceed with these data
    if (cert_data_in_backup_file) {
      out <- res[["Certification"]][["data_input"]]
      # update GUI elements
      updateTextInput(session, inputId = "user", value = res[["Certification"]]$user)
      updateTextInput(session,
                      inputId = "study_id",
                      value = res[["Certification"]]$study_id)
      # initialize permanent values
      assign("dat", value = out, envir = env_perm)
      assign(
        "normality_statement",
        value = res[["Certification"]]$normality_statement,
        envir = env_perm
      )
      assign(
        "c_upload_statement",
        value = paste0(
          "Data upload completed for ",
          length(unique(out[, "Lab"])),
          " Labs from a previous analysis (",
          input$in_file_ecerto_backup$name,
          ")."
        ),
        envir = env_perm
      )
      assign("cert_vals",
             value = res[["Certification"]]$cert_vals,
             envir = env_perm)
      # set global parameter to acknowledge initial loading
      reload_finished <<- FALSE
      # switch on material report table after cert_vals was established
      shinyjs::click(id = "show_table")
      return(out)
      # otherwise check if there are excel files available for reading as input
    } else if (!is.null(input$c_input_files)) {
        #read from excel files
        file.type <- tools::file_ext(input$c_input_files$datapath)
        validate(need(
          length(unique(file.type)) == 1,
          "Not all selected Files have the same format."
        ))
        file.type <- "xlsx"
        #for (id in c("user","study_id","start_row","end_row","start_col","end_col","Excel_Sheet_Name")) shinyjs::enable(id=id)
        validate(
          need(
            is.numeric(input$start_row) &&
              input$start_row >= 1 &&
              input$start_row < input$end_row,
            message = "check row definition"
          ),
          need(
            is.numeric(input$end_row) &&
              input$end_row > input$start_row &&
              input$end_row <= 16000,
            message = "check row definition"
          ),
          need(
            is.numeric(input$start_col) &&
              input$start_col >= 1 &&
              input$start_col < (input$end_col - 2),
            message = "check col definition"
          ),
          need(
            is.numeric(input$end_col) &&
              input$end_col > input$start_col &&
              input$end_col <= 160,
            message = "check col definition"
          )
        )
        # read Excel tables
        sheet <- NA
        if (input$Excel_Sheet_Name %in% openxlsx::getSheetNames(input$c_input_files$datapath[1])) {
          sheet <- input$Excel_Sheet_Name
        } else {
          if (nchar(input$Excel_Sheet_Name) <= 2 &&
              all(strsplit(input$Excel_Sheet_Name, "")[[1]] %in% 0:9))
            sheet <- as.numeric(input$Excel_Sheet_Name)
        }
        validate(need(!is.na(sheet), message = "check sheet name"))
        tryCatch({
          data <- lapply(input$c_input_files$datapath, function(x) {
            read_excel_input(
              xlsxFile = x,
              sheet = sheet,
              rows = input$start_row:input$end_row,
              cols = input$start_col:input$end_col
            )
          })
        }, error = function(e) {
          stop(safeError(e))
        })
        out <-
          data.frame(
            "Lab" = rep(paste0(
              "L", formatC(
                1:length(data),
                width = nchar(as.character(length(data))),
                flag = 0
              )
            ), times = sapply(data, nrow)),
            plyr::ldply(data, function(x) {
              x
            }),
            "File" = rep(input$c_input_files$name, times =
                           sapply(data, nrow)),
            "S_flt" = FALSE,
            "L_flt" = FALSE
          )
        # remove non-finite values
        out <- out[is.finite(out[, "value"]), ]
        # perform minimal validation tests
        validate(
          need(is.numeric(out[, "value"]), message = "measurement values seem not to be numeric"),
          need(length(levels(as.factor(out[, "Lab"]))) >= 2, message = "less than 2 Labs imported")
        )
        out <- data.frame("ID" = 1:nrow(out), out)
        updateNumericInput(session,
                           inputId = "Fig01_width",
                           value = 150 + 40 * length(levels(out$Lab)))
        attr(out, "file.type") <- file.type
        # initialize permanent values
        assign("dat", value = out, envir = env_perm)
        assign("normality_statement",
               value = "",
               envir = env_perm)
        assign(
          "c_upload_statement",
          value = paste(
            "Data upload completed for",
            length(input$c_input_files$datapath),
            "Excel files representing Labs."
          ),
          envir = env_perm
        )
        #assign("cert_vals", value = data.frame("analyte"=levels(out[,"analyte"]), "mean"=NA, "F1"=1, "F2"=1, "F3"=1, "cert_val"=NA, "sd"=NA, "n"=NA, "ubb_r"=1, "U"=NA), envir = env_perm)
        cert_vals <-
          data.frame(
            "analyte" = levels(out[, "analyte"]),
            "mean" = NA,
            "F1" = 1,
            "F2" = 1,
            "F3" = 1,
            "cert_val" = NA,
            "sd" = NA,
            "n" = NA,
            "char" = 0,
            "U2" = 0,
            "U3" = 0,
            "U4" = 0,
            "U5" = 0,
            "U6" = 0,
            "U7" = 0,
            "com" = NA,
            "k" = 2,
            "U" = NA
          )
        attr(cert_vals, "col_code") <-
          data.frame(
            "ID" = c(paste0("F", 1:3), paste0("U", 2:7)),
            "Name" = c(paste0("F", 1:3), paste0("U", 2:7)),
            stringsAsFactors = FALSE
          )
        attr(cert_vals, "disable") <- c(1, 5:8, 15, 17)
        assign("cert_vals", value = cert_vals, envir = env_perm)
        # switch on material report table after cert_vals was established
        shinyjs::click(id = "show_table")
        return(out)
      } else {
        return(NULL)
      }
    
  })
  
  # renderUI() captures the upload HTML control, which here can be hidden
  # TODO is this necessary to have on server? ####
  output$c_input_files_ui <- renderUI({
    wellPanel(
      # create a file upload control
      fileInput(
        inputId = "c_input_files",
        label = "Import Excel Files",
        multiple = TRUE,
        accept = c("xls", "xlsx")
      )
    )
  })
  # observe({ shinyjs::click(id="opt_input_files", condition=length(input$in_file_ecerto_backup)>0 | length(input$c_input_files)>0) })
  
  output$c_fileUploaded_message <- reactive({
    if (is.null(c_Data())) {
      return("")
    } else {
      return(getData("c_upload_statement"))
    }
  })
  #outputOptions(output, 'c_fileUploaded_message', suspendWhenHidden=FALSE)
  
  # output$c_fileUploaded <- reactive({
  #   return(!is.null(c_Data()) && isTruthy(c_Data()))
  # })
  # outputOptions(output, 'c_fileUploaded', suspendWhenHidden=FALSE)
  
  # filter data for particular measurements specified by user
  dataset_flt <- reactive({
    req(c_Data()) # this is required in case that Certification=NULL data is reloaded from backup into an active session with present Certification data
    out <- getData("dat")
    # in case of reloaded old data switch off some functionality (analyte/sample selection)
    if (!is.null(out) && attr(out, "file.type") == "RData") {
      load(input$in_file_ecerto_backup$datapath[1])
      if (!is.null(input$sel_analyt) &&
          input$sel_analyt == res$Certification$opt$sel_analyt &&
          !reload_finished) {
        # at first round of reload fill all stored parameters for the last analyte processed
        updateSelectizeInput(
          session,
          inputId = "sel_analyt",
          selected = res$Certification$opt$sel_analyt
        )
        updateSelectizeInput(
          session,
          inputId = "flt_samples",
          selected = res$Certification$opt$flt_samples
        )
        updateSelectizeInput(
          session,
          inputId = "flt_labs",
          selected = res$Certification$opt$flt_labs
        )
        updateNumericInput(session,
                           inputId = "precision",
                           value = res$Certification$precision)
        updateCheckboxInput(session,
                            inputId = "pooling",
                            value = res$Certification$opt$pooling)
        updateNumericInput(
          session,
          inputId = "Fig01_width",
          value = res$Certification$CertValPlot$Fig01_width
        )
        updateNumericInput(
          session,
          inputId = "Fig01_height",
          value = res$Certification$CertValPlot$Fig01_height
        )
        # set reload_finished to TRUE to avoid overwriting parameters in next round
        reload_finished <<- TRUE
      }
    }
    validate(need(input$sel_analyt, message = "please select analyte"))
    # filter for specific analyte and remove user defined samples
    if (!is.null(input$sel_analyt) && input$sel_analyt %in% out[, "analyte"])
      out <- out[out[, "analyte"] %in% input$sel_analyt, ]
    if (!is.null(input$flt_samples) &&any(out[, "ID"] %in% input$flt_samples))
      out <-
      out[!(out[, "ID"] %in% input$flt_samples), ]
    else
      out[, "S_flt"] <- FALSE
    if (!is.null(input$flt_labs) && any(out[, "Lab"] %in% input$flt_labs))
      out[, "L_flt"] <- out[, "Lab"] %in% input$flt_labs
    else
      out[, "L_flt"] <- FALSE
    
    # adjust factor levels
    out[, "Lab"] <- factor(out[, "Lab"])
    
    # Notify User in case that only 1 finite measurement remained within group
    validate(
      need(
        all(sapply(split(out[, "value"], out[, "Lab"]), length) >= 2),
        message = paste(names(which(
          sapply(split(out[, "value"], out[, "Lab"]), length) < 2
        ))[1], "has less than 2 replicates left.")
      ),
      need(
        is.numeric(input$precision) &&
          input$precision >= 0 &&
          input$precision <= 6,
        message = "please check precision value"
      )
    )
    
    # round input values
    out[, "value"] <- round(out[, "value"], input$precision)
    
    # read/write permanent data
    if (exists("dat", envir = env_perm)) {
      dat <- get("dat", envir = env_perm)
      dat[dat[, "analyte"] %in% input$sel_analyt, "S_flt"] <- FALSE
      if (!is.null(input$flt_samples))
        dat[dat[, "analyte"] %in% input$sel_analyt &
              dat[, "ID"] %in% input$flt_samples, "S_flt"] <- TRUE
      dat[dat[, "analyte"] %in% input$sel_analyt &
            dat[, "Lab"] %in% levels(out[, "Lab"]), "L_flt"] <- FALSE
      if (!is.null(input$flt_labs))
        dat[dat[, "analyte"] %in% input$sel_analyt &
              dat[, "Lab"] %in% input$flt_labs, "L_flt"] <- TRUE
      assign("dat", value = dat, envir = env_perm)
    }
    return(out)
  })
  
  output$sel_analyt_ui <- renderUI({
    req(c_Data())
    if (!is.null(c_Data()) &&
        attr(c_Data(), "file.type") == "RData") {
      load(input$in_file_ecerto_backup$datapath[1])
      selected <- res$Certification$opt$sel_analyt
    } else {
      selected <- levels(c_Data()[, "analyte"])[1]
    }
    selectInput(
      inputId = "sel_analyt",
      label = "analyte",
      choices = levels(c_Data()[, "analyte"]),
      selected = selected
    )
  })
  
  output$flt_samples <- renderUI({
    req(c_Data(), input$sel_analyt) 
    tmp <- getData("dat")
    choices <- tmp[tmp[, "analyte"] == input$sel_analyt, "ID"]
    selected <-
      choices[which(tmp[tmp[, "analyte"] == input$sel_analyt, "S_flt"])]
    selectizeInput(
      inputId = "flt_samples",
      label = "Filter Sample IDs",
      choices = choices,
      selected = selected,
      multiple = TRUE
    )
  })
  
  output$flt_labs <- renderUI({
    req(c_Data(), input$sel_analyt)
    tmp <- getData("dat")
    tmp <-
      tmp[tmp[, "analyte"] == input$sel_analyt &
            is.finite(tmp[, "value"]), ]
    choices <- levels(factor(tmp[, "Lab"]))
    selected <-
      choices[which(sapply(split(tmp[, "L_flt"], factor(tmp[, "Lab"])), all))]
    selectizeInput(
      inputId = "flt_labs",
      label = "Filter Labs",
      choices = choices,
      selected = selected,
      multiple = TRUE
    )
  })
  
  output$c_fix_col_names <- renderUI({
    validate(need(input$sel_analyt, message = "please select analyte"))
    cert_vals <- getData("cert_vals")
    selectInput(
      inputId = "c_fix_col_names",
      label = "select internal column names",
      selectize = TRUE,
      choices = attr(cert_vals, "col_code")[, "ID"]
    )
  })
  
  output$c_displayed_col_name <- renderUI({
    validate(need(input$c_fix_col_names, message = "please select col name"))
    cert_vals <- getData("cert_vals")
    textInput(
      inputId = "c_displayed_col_name",
      label = "modify displayed column name",
      value = attr(cert_vals, "col_code")[attr(cert_vals, "col_code")[, "ID"] ==
                                            input$c_fix_col_names, "Name"]
    )
  })
  
  # generate the full lab means dataset with all tests as an overview
  lab_means <- reactive({
    req(dataset_flt())
    data <- dataset_flt()
    out <-
      plyr::ldply(split(data$value, data$Lab), function(x) {
        data.frame(
          "mean" = mean(x, na.rm = T),
          "sd" = sd(x, na.rm = T),
          "n" = sum(is.finite(x))
        )
      }, .id = "Lab")
    rownames(out) <- out$Lab
    suppressWarnings(KS_p <-
                       stats::ks.test(
                         x = out$mean,
                         y = "pnorm",
                         mean = mean(out$mean),
                         sd = sd(out$mean)
                       )$p.value)
    assign(
      "normality_statement",
      value = paste0(
        "The data is",
        ifelse(KS_p < 0.05, " not ", " "),
        "normally distributed (KS_p=",
        formatC(KS_p, format = "E", digits = 2),
        ")."
      ),
      envir = env_perm
    )
    return(out)
  })
  
  # prepare a compact version of the data table
  dataset_komp <- reactive({
    req(dataset_flt(), input$precision)
    data <- dataset_flt()
    n_reps <- sort(unique(data$replicate))
    data <- plyr::ldply(split(data, data$Lab), function(x) {
      out <- rep(NA, length(n_reps))
      out[x$replicate] <- x$value
      matrix(out,
             ncol = length(n_reps),
             dimnames = list(NULL, paste0("R", n_reps)))
    }, .id = "Lab")
    n <- input$precision
    return(data.frame(
      data[, 1, drop = F],
      round(data[, -1, drop = F], digits = n),
      "mean" = round(apply(data[, -1, drop = F], 1, mean, na.rm = T), digits = n),
      "sd" = round(apply(data[, -1, drop = F], 1, sd, na.rm = T), digits = n)
    ))
  })
  
  cert_mean <- reactive({
    req(dataset_flt(), input$precision2)
    data <- dataset_flt()[!dataset_flt()[, "L_flt"], ]
    # re-factor Lab because user may have excluded one or several labs from calculation of cert mean while keeping it in Figure
    data[, "Lab"] <- factor(data[, "Lab"])
    ifelse(input$pooling,
           round(mean(data[, "value"], na.rm = T), input$precision2),
           round(mean(sapply(
             split(data[, "value"], data[, "Lab"]), mean, na.rm = T
           )), input$precision2))
  })
  output$cert_mean <- renderText({
    cert_mean()
  })
  
  cert_sd <- reactive({
    req(dataset_flt(), input$precision2)
    data <- dataset_flt()[!dataset_flt()[, "L_flt"], ]
    # re-factor Lab because user may have excluded one or several labs from calculation of cert mean while keeping it in Figure
    data[, "Lab"] <- factor(data[, "Lab"])
    ifelse(input$pooling,
           round(sd(data[, "value"], na.rm = T), input$precision2),
           round(sd(sapply(
             split(data[, "value"], data[, "Lab"]), mean, na.rm = T
           )), input$precision2))
  })
  output$cert_sd <- renderText({
    cert_sd()
  })
  
  # Generate an HTML table view of filtered single analyt data
  output$flt_Input_Data <- DT::renderDataTable({
    if (input$opt_show_files == "kompakt") {
      return(dataset_komp())
    }
    if (input$opt_show_files == "standard") {
      return(dataset_flt()[, c("ID", "Lab", "value", "unit", "replicate", "File", "L_flt")])
    } else {
      return()
    }
  }, options = list(paging = FALSE, searching = FALSE), rownames = NULL)
  
  # Homogeneity part
  source("server_modul_homogeneity.R", local = TRUE)
  
  # Stability Part
  source("server_modul_stability.R", local = TRUE)
  
  # LTS part
  source("server_modul_LTS.R", local = TRUE)
  
  # time_stamp() as a reactive value is necessary to ensure that c_res() is reevaluated when dat and cert_vals are changed in memory being not reactive themselves
  time_stamp <- reactiveVal(value = date())
  # Hier ist eine Ergebnis-Liste mit den R calls für die einzelnen Statistik-Module (mainly for results.Rmd and backup.RData)
  c_res <- reactive({
    # once some data for a certification are uploaded res will be established
    if (is.null(c_Data())) {
      return(list("Certification" = NULL))
    } else {
      # to avoid many iterations in setting up 'c_res' I selected an input object created relatively late to be required before 'c_res' is set up
      req(cert_mean())
      return(list(
        "Certification" = list(
          "time_stamp" = time_stamp(),
          "user" = input$user,
          "study_id" = input$study_id,
          # $ToDo$ adjust for backup loaded data
          "input_files" = input$c_input_files[[1]],
          "data_input" = getData("dat"),
          "cert_vals" = getData("cert_vals"),
          "data" = dataset_flt(),
          "data_kompakt" = dataset_komp(),
          "lab_means" = lab_means(),
          "cert_mean" = cert_mean(),
          "cert_sd" = cert_sd(),
          "normality_statement" = get("normality_statement", envir = env_perm),
          "precision" = input$precision,
          "opt" = list(
            "data_style" = input$opt_show_files,
            "show_code" = input$show_code,
            "pooling" = input$pooling,
            "flt_samples" = input$flt_samples,
            "flt_labs" = input$flt_labs,
            "sel_analyt" = input$sel_analyt
          ),
          "boxplot" = list(
            "show" = FALSE,
            "fnc" = deparse(TestPlot),
            "call" = str2lang('TestPlot(data=data)')
          ),
          "stats" = list(
            "show" = input$show_stats,
            "fnc" = deparse(Stats),
            "call" = str2lang('Stats(data=data, precision=precision)')
          ),
          "mstats" = list(
            "show" = input$show_mstats,
            "fnc" = deparse(mstats),
            "call" = str2lang('mstats(data=data, precision=precision)')
          ),
          "CertValPlot" = list(
            "show" = FALSE,
            "fnc" = deparse(CertValPlot),
            "call" = str2lang('CertValPlot(data=data)'),
            "Fig01_width" = input$Fig01_width,
            "Fig01_height" = input$Fig01_height
          )
        )
      ))
    }
  })
  
  # Stats
  output$overview_stats <- DT::renderDataTable({
    Stats(data = dataset_flt(), precision = input$precision)
  }, options = list(paging = FALSE, searching = FALSE), rownames = NULL)
  
  # mStats
  output$overview_mstats <- DT::renderDataTable({
    mstats(data = dataset_flt(), precision = input$precision)
  }, options = list(paging = FALSE, searching = FALSE), rownames = NULL)
  
  output$normality_statement <- renderText({
    lab_means()
    getData("normality_statement")
  })
  
  # BOXPLOT
  output$overview_boxplot <- renderPlot({
    if (input$opt_show_files == "boxplot") {
      TestPlot(data = dataset_flt())
    } else {
      return(NULL)
    }
  })
  
  output$qqplot <- renderPlot({
    req(lab_means())
    y <- lab_means()[, "mean"]
    qqnorm(y = y)
    qqline(y = y, col = 2)
  }, height = 400, width = 400)
  
  # CertVal Plot
  output$overview_CertValPlot <- renderPlot({
    CertValPlot(data = dataset_flt())
  }, height = reactive({
    input$Fig01_height
  }), width = reactive({
    input$Fig01_width
  }))
  
  # Result Material
  observe({
    req(c_Data())
    if (!is.null(getData("cert_vals")) & input$show_table >= 1) {
      # retrieve current cert_vals
      cert_vals <- getData("cert_vals")
      # update column name if desired
      if (!is.null(input$c_displayed_col_name) &&
          input$c_displayed_col_name != "") {
        attr(cert_vals, "col_code")[attr(cert_vals, "col_code")[, "ID"] == input$c_fix_col_names, "Name"] <-
          input$c_displayed_col_name
      }
      # remove column if desired
      if (!is.null(input$c_displayed_col_name) &&
          input$c_displayed_col_name == "delete") {
        k <- which(colnames(cert_vals) == isolate(input$c_fix_col_names))
        tmp_cert_vals <- cert_vals[, -k]
        attr(tmp_cert_vals, "disable") <-
          sapply(attr(cert_vals, "disable"), function(x) {
            ifelse(x >= k, x - 1, x)
          })
        attr(tmp_cert_vals, "col_code") <-
          attr(cert_vals, "col_code")[!attr(cert_vals, "col_code")[, "Name"] == "delete", ]
        cert_vals <- tmp_cert_vals
        updateTextInput(session, inputId = "c_displayed_col_name", value =
                          "")
        updateSelectInput(session,
                          inputId = "c_fix_col_names",
                          choices = attr(cert_vals, "col_code")[, "ID"])
      }
      updateSelectInput(session,
                        inputId = "h_transfer_ubb",
                        choices = attr(cert_vals, "col_code")[substr(attr(cert_vals, "col_code")[, "ID"], 1, 1) ==
                                                                "U", "Name"])
      updateSelectInput(session,
                        inputId = "s_transfer_ubb",
                        choices = attr(cert_vals, "col_code")[substr(attr(cert_vals, "col_code")[, "ID"], 1, 1) ==
                                                                "U", "Name"])
      # update for current analyte
      i <- which(cert_vals[, "analyte"] %in% input$sel_analyt)
      if (length(i) == 1) {
        # take mean and sd from input boxes and calculate n according to selected pooling option
        if (!is.null(cert_mean()))
          cert_vals[i, "mean"] <- cert_mean()
        if (!is.null(cert_sd()))
          cert_vals[i, "sd"] <- cert_sd()
        cert_vals[i, "n"] <-
          ifelse(input$pooling,
                 sum(lab_means()[!(lab_means()[, "Lab"] %in% input$flt_labs), "n"]),
                 nrow(lab_means()) - length(input$flt_labs))
      }
      # recalc all cert_mean values including correction factors
      cert_vals[, "cert_val"] <-
        apply(cert_vals[, unlist(sapply(c("mean", paste0("F", 1:3)), function(x) {
          grep(x, colnames(cert_vals))
        }))], 1, prod, na.rm = T)
      # recalc all U values including correction factors
      cert_vals[, "char"] <-
        cert_vals[, "sd"] / (sqrt(cert_vals[, "n"]) * cert_vals[, "mean"])
      cert_vals[, "com"] <-
        apply(cert_vals[, unlist(sapply(c("char", paste0("U", 2:7)), function(x) {
          grep(x, colnames(cert_vals))
        }))], 1, function(x) {
          sqrt(sum(x ^ 2, na.rm = T))
        })
      cert_vals[, "U"] <- cert_vals[, "k"] * cert_vals[, "com"]
      assign("cert_vals", value = cert_vals, envir = env_perm)
      # monitor table editing and update if necessary
      # rename column header for temporary display
      tmp_cert_vals <- cert_vals
      for (k in unlist(sapply(c("char", paste0("U", 2:7), "com", "U"), function(x) {
        grep(x, colnames(tmp_cert_vals))
      })))
        tmp_cert_vals[, k] <- round(tmp_cert_vals[, k], input$precision2)
      for (k in 1:nrow(attr(cert_vals, "col_code"))) {
        colnames(tmp_cert_vals)[colnames(tmp_cert_vals) == attr(cert_vals, "col_code")[k, "ID"]] <-
          attr(cert_vals, "col_code")[k, "Name"]
      }
      output$matreport <-
        DT::renderDT(
          DT::datatable(
            data = tmp_cert_vals,
            editable = list(target = "cell", disable = list(
              columns = attr(tmp_cert_vals, "disable")
            )),
            options = list(paging = FALSE, searching = FALSE),
            rownames = NULL
          ),
          server = TRUE
        )
      #output$matreport <- DT::renderDT(DT::datatable(data=cert_vals, editable = list(target = "cell", disable = list(columns = attr(cert_vals, "disable"))), options = list(paging = FALSE, searching = FALSE), rownames=NULL), server = TRUE)
      proxy_matreport = DT::dataTableProxy('matreport')
      observeEvent(input$matreport_cell_edit, {
        test <- as.numeric(input$matreport_cell_edit$value)
        cert_vals <- get("cert_vals", envir = env_perm)
        cert_vals[input$matreport_cell_edit$row, input$matreport_cell_edit$col +
                    1] <- as.numeric(input$matreport_cell_edit$value)
        assign("cert_vals", value = cert_vals, envir = env_perm)
        # update selectize input to trigger recalculation --> not working this way --> leads to eternal loop
        #updateTextInput(session, inputId = "c_fix_col_names", value = ifelse(input$c_fix_col_names=="F1","U1","F1"))
      })
      # update time_stamp and thereby also 'dat' and 'cert_vals' in c_res()
      time_stamp(date())
    }
  })
  
  # REPORT Analyte
  # $ToDo$ adjust path variable in reports
  output$FinalReport <- downloadHandler(
    filename = function() {
      paste0(input$study_id, "_", input$sel_analyt, '.', switch(
        input$output_file_format,
        PDF = 'pdf',
        HTML = 'html',
        Word = 'docx'
      ))
    },
    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
      owd <- setwd(tempdir(check = TRUE))
      on.exit(setwd(owd))
      writeLines(text = Report_Vorlage_Analyt(), con = 'Report_Vorlage_tmp.Rmd')
      out <- rmarkdown::render(
        input = 'Report_Vorlage_tmp.Rmd',
        output_format = switch(
          input$output_file_format,
          PDF = rmarkdown::pdf_document(),
          HTML = rmarkdown::html_document(),
          Word = rmarkdown::word_document()
        ),
        params = list("res" = c_res()),
        # !!! das ist die Liste mit Eingabewerten für die weitere Verarbeitung im Report
        envir = new.env(parent = globalenv())
      )
      file.rename(out, file)
    }
  )
  
  # REPORT Material
  output$MaterialReport <- downloadHandler(
    filename = function() {
      paste0(input$study_id, "_", input$user, '.', switch(
        input$output_file_format,
        PDF = 'pdf',
        HTML = 'html',
        Word = 'docx'
      ))
    },
    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write permission to the current working directory
      owd <- setwd(tempdir(check = TRUE))
      on.exit(setwd(owd))
      writeLines(text = Report_Vorlage_Material(), con = 'tmp_Report.Rmd')
      out <- rmarkdown::render(
        input = 'tmp_Report.Rmd',
        output_format = switch(
          input$output_file_format,
          PDF = rmarkdown::pdf_document(),
          HTML = rmarkdown::html_document(),
          Word = rmarkdown::word_document()
        ),
        params = list("res" = c_res()),
        # !!! das ist die Liste mit Eingabewerten für die weitere Verarbeitung im Report
        envir = new.env(parent = globalenv())
      )
      file.rename(out, file)
    }
  )
  
  # FIGURE DOWNLOAD
  output$Fig01 <- downloadHandler(
    filename = function() {
      paste0(input$study_id, "_", input$sel_analyt, '_Fig01.pdf')
    },
    content = function(file) {
      data <- c_res()$Certification$data
      CertValPlot <-
        eval(parse(text = c_res()$Certification$CertValPlot$fnc))
      pdf(
        file = file,
        width = c_res()$Certification$CertValPlot$Fig01_width / 72,
        height = c_res()$Certification$CertValPlot$Fig01_height / 72
      )
      eval(c_res()$Certification$CertValPlot$call)
      dev.off()
    },
    contentType = "image/pdf"
  )
  
  # BACKUP
  output$ecerto_backup <- downloadHandler(
    filename = function() {
      paste0(c_res()[["study_id"]], '.RData')
    },
    content = function(file) {
      res <- c(c_res(), h_res(), s_res())
      if (!is.null(res[["Certification"]]))
        attr(res[["Certification"]][["data_input"]], "file.type") <-
          "RData"
      save(res, file = file)
    },
    contentType = "RData"
  )
  
  # render images for help pages
  output$myImage01 <-
    renderImage({
      list(src = "help/Import_Data_Certification.png",
           contentType = 'image/png',
           alt = "Example for fixed format input file")
    }, deleteFile = FALSE)
  output$myImage02 <-
    renderImage({
      list(src = "help/Import_Data_Certification_labeled.png",
           contentType = 'image/png',
           alt = "Example for fixed format input file")
    }, deleteFile = FALSE)
  output$myImage02a <-
    renderImage({
      list(src = "help/Import_Data_Certification_labeled.png",
           contentType = 'image/png',
           alt = "Example for fixed format input file")
    }, deleteFile = FALSE)
  output$myImage03 <-
    renderImage({
      list(src = "help/Data_Stats01.png",
           contentType = 'image/png',
           alt = "Lab Stats")
    }, deleteFile = FALSE)
  output$myImage04 <-
    renderImage({
      list(src = "help/Data_Stats02.png",
           contentType = 'image/png',
           alt = "Overall mean stats")
    }, deleteFile = FALSE)
  output$myImage05 <-
    renderImage({
      list(src = "help/Data_Stats03.png",
           contentType = 'image/png',
           alt = "Certified values analyte")
    }, deleteFile = FALSE)
  output$myImage06 <-
    renderImage({
      list(src = "help/Data_Stats04.png",
           contentType = 'image/png',
           alt = "Certified values material")
    }, deleteFile = FALSE)
  output$myImage07 <-
    renderImage({
      list(src = "help/Export_Data_Options.png",
           contentType = 'image/png',
           alt = "Export_Data_Options")
    }, deleteFile = FALSE)
  output$myImage08 <-
    renderImage({
      list(src = "help/Import_hData_FixedFormat.png",
           contentType = 'image/png',
           alt = "Example for fixed format input file")
    }, deleteFile = FALSE)
  output$myImage08a <-
    renderImage({
      list(src = "help/Import_hData_FixedFormat.png",
           contentType = 'image/png',
           alt = "Example for fixed format input file")
    }, deleteFile = FALSE)
  output$myImage09 <-
    renderImage({
      list(src = "help/ecerto_header.png",
           contentType = 'image/png',
           alt = "eCerto header")
    }, deleteFile = FALSE)
  output$myImage10 <-
    renderImage({
      list(src = "help/Import_sData_FixedFormat.png",
           contentType = 'image/png',
           alt = "Example for fixed format input file")
    }, deleteFile = FALSE)
  output$myImage10a <-
    renderImage({
      list(src = "help/Import_sData_FixedFormat.png",
           contentType = 'image/png',
           alt = "Example for fixed format input file")
    }, deleteFile = FALSE)
  output$myImage11 <-
    renderImage({
      list(src = "help/Import_ltsData_FixedFormat.png",
           contentType = 'image/png',
           alt = "Example for fixed format input file")
    }, deleteFile = FALSE)
  output$myImage11a <-
    renderImage({
      list(src = "help/Import_ltsData_FixedFormat.png",
           contentType = 'image/png',
           alt = "Example for fixed format input file")
    }, deleteFile = FALSE)
  output$myImage11b <-
    renderImage({
      list(src = "help/Import_ltsData_FixedFormat.png",
           contentType = 'image/png',
           alt = "Example for fixed format input file")
    }, deleteFile = FALSE)
  
})