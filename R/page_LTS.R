#' @title page_LTS.
#' @description Modul for LongTermStability Monitoring as currently (2021) used by Carsten Prinz.
#' @param id Name when called as a module in a shiny app.
#' @return nothing
#' @noRd
#' @keywords internal
m_longtermstabilityUI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::conditionalPanel(
      condition = "output.LTS_fileUploaded == false",
      ns = ns, # namespace of current module
      shiny::fileInput(
        inputId = ns("LTS_input_file"),
        label = shiny::actionLink(inputId = ns("InputHelp"), "Import Excel/RData File"),
        multiple = FALSE,
        accept = c("xls", "xlsx", "RData")
      ),
      shiny::p(shiny::helpText("Example Table")),
      shiny::img(src = "www/rmd/fig/L_Modul_Import.png")
    ),
    shiny::conditionalPanel(
      condition = "output.LTS_fileUploaded == true",
      ns = ns, # namespace of current module
      shiny::fluidRow(
        shiny::column(
          width = 3,
          sub_header(
            txt = shiny::actionLink(
              inputId = ns("TabL1_link"),
              label = "Tab.L1 - Long Term Stability measurement data"
            )
          ),
          DT::dataTableOutput(ns("LTS_vals"))
        ),
        shiny::column(
          width = 9,
          # shiny::fluidRow(
          #   shiny::column(
          #     width = 12,
          DT::dataTableOutput(ns("LTS_def")),
          #   ), style = "margin-bottom: 15px;"
          # ),
          shiny::wellPanel(
            shiny::fluidRow(
              shiny::column(2, shiny::uiOutput(ns("LTS_sel_KW"))),
              shiny::tags$style(type = "text/css", "#lts-Report {margin-top:-1%;}"),
              shiny::column(
                width = 2,
                shiny::checkboxGroupInput(
                  inputId = ns("LTS_opt"), label = NULL,
                  choiceNames = list("Show property values", shiny::HTML("Use CI<sub>95</sub> (slope)")),
                  choiceValues = list("show_property_values", "show_ci"),
                  selected = c("show_property_values", "show_ci")
                )
              ),
              shiny::column(6, DT::dataTableOutput(ns("LTS_NewVal"))),
              shiny::tags$style(type = "text/css", "#lts-LTS_NewVal {margin-top:-2%;}"),
              shiny::column(2, shiny::strong("New Entry"), shiny::p(), shiny::actionButton(inputId = ns("LTS_ApplyNewValue"), label = "Add data")),
              shiny::tags$style(type = "text/css", "#lts-LTS_ApplyNewValue {margin-top:-1%;}")
            ),
            shiny::fluidRow(
              shiny::column(2, shiny::strong("Download Report"), shiny::p(), shiny::downloadButton(ns("Report"))),
              shiny::column(2, shiny::strong("Save LTS Data"), shiny::p(), shiny::downloadButton(ns("LTS_Save"), label = "Backup")),
              shiny::column(
                width = 6,
                shinyjs::disabled(
                  shiny::textInput(
                    inputId = ns("datacomment"),
                    label = "data comment",
                    value = "",
                    placeholder = "select point or row, enter comment and confirm"
                  )
                )
              ),
              shiny::column(2, shiny::strong("New Comment"), shiny::p(), shiny::actionButton(inputId = ns("LTS_ApplyNewComment"), label = "Add comment"))
            )
          ),
          shiny::fluidRow(
            shiny::div(
              style = "float: left; width: 45%; max-width: 290px; padding-left: 15px; margin-top: 10px",
              shiny::strong(
                shiny::actionLink(
                  inputId = ns("FigL1_link"),
                  label = "Fig.L1 - Long Term Stability calculation"
                )
              ),
            ) # ,
            # shiny::div(style = "float: left; width: 25%; max-width: 160px; padding-left: 15px;", shiny::checkboxInput(inputId = ns("show_ci"), label = shiny::HTML("Use CI<sub>95</sub> (slope)"), value = FALSE)),
            # shiny::div(style = "float: left; width: 30%; max-width: 210px; padding-left: 15px;", shiny::checkboxInput(inputId = ns("show_plot_L3"), label = shiny::HTML("Show running predictor plot"), value = FALSE))
          ),
          # shiny::fluidRow(shiny::column(12, shiny::plotOutput(ns("LTS_plot1_1"), height = "450px", click = ns("plot1_click"), hover = ns("plot1_hover")))),
          # shiny::fluidRow(shiny::column(12, shiny::plotOutput(ns("LTS_plot1_2"), height = "450px"))),
          # shiny::fluidRow(shiny::column(12, shiny::plotOutput(ns("LTS_plot2"), height = "450px")))
          shiny::fluidRow(
            shiny::column(6, shiny::plotOutput(ns("LTS_plot1_1"), height = "450px", click = ns("plot1_click"), hover = ns("plot1_hover"))),
            shiny::column(6, shiny::plotOutput(ns("LTS_plot1_2"), height = "450px"))
          )
        )
      )
    ) # conditionalPanel
  )
}

#' @noRd
#' @keywords internal
m_longtermstabilityServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    datalist <- shiny::reactiveValues("lts_data" = NULL, "comment" = NULL)

    LTS_Data <- shiny::reactive({
      if (!is.null(input$LTS_input_file)) {
        file.type <- tools::file_ext(input$LTS_input_file$datapath)
        if (!tolower(file.type) %in% c("rdata", "xls", "xlsx")) {
          shinyWidgets::show_alert(title = "Wrong Filetype?", text = "Please select an RData file or an Excel file.", type = "warning")
          return(NULL)
        }
        if (tolower(file.type) == "rdata") {
          tryCatch(
            {
              load(input$LTS_input_file$datapath[1])
            },
            error = function(e) {
              stop(shiny::safeError(e))
            }
          )
          if (!exists("LTS_dat")) {
            warning("Did load RData backup but could not find object 'LTS_dat' inside.")
            LTS_dat <- NULL
          }
        } else {
          LTS_dat <- read_lts_input(file = input$LTS_input_file$datapath[1])
          check_validity <- TRUE
          i <- 0
          while (check_validity & i < length(LTS_dat)) {
            for (i in 1:length(LTS_dat)) {
              def_cols <- c("RM", "KW", "KW_Def", "KW_Unit", "CertVal", "U", "U_Def", "Device", "Method", "Coef_of_Var", "acc_Datasets")
              check_def_cols <- def_cols %in% colnames(LTS_dat[[i]][["def"]])
              val_cols <- c("Value", "Date", "File")
              check_val_cols <- val_cols %in% colnames(LTS_dat[[i]][["val"]])
              if (!all(check_def_cols)) {
                warn_txt <- paste0("Can't find the following columns in input file", i, " 'definition' part: ", paste(def_cols[!check_def_cols], collapse = ", "))
                shinyWidgets::show_alert(title = "Warning", text = warn_txt, type = "warning")
                LTS_dat[[i]][["def"]] <- cbind(LTS_dat[[i]][["def"]], as.data.frame(matrix(NA, ncol = sum(!check_def_cols), nrow = 1, dimnames = list(NULL, def_cols[!check_def_cols]))))
              }
              if (!all(check_val_cols)) {
                warn_txt <- paste0("Can't find the following columns in input file", i, " 'definition' part: ", paste(val_cols[!check_val_cols], collapse = ", "))
                shinyWidgets::show_alert(title = "Warning", text = warn_txt, type = "warning")
              }
              if (!"Comment" %in% colnames(LTS_dat[[i]][["val"]])) LTS_dat[[i]][["val"]] <- cbind(LTS_dat[[i]][["val"]], "Comment" = as.character(rep(NA, nrow(LTS_dat[[i]][["val"]]))))
              if (!inherits(LTS_dat[[i]][["val"]][, "Date"], "Date")) {
                LTS_dat[[i]][["val"]][, "Date"] <- as.Date.character(LTS_dat[[i]][["val"]][, "Date"], tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%Y/%m/%d"))
              }
              shiny::validate(shiny::need(inherits(LTS_dat[[i]][["val"]][, "Date"], "Date"), "Sorry, could not convert column 'Date' into correct format."))
              shiny::validate(shiny::need(LTS_dat[[i]][["def"]][, "U_Def"] %in% c("1s", "2s", "CI", "1sx", "2sx"), "Sorry, unexpected value in 'U_Def'. Allowed: '1s', '2s', 'CI', '1sx' and '2sx'. Please check."))
              LTS_dat[[i]][["def"]] <- LTS_dat[[i]][["def"]][, def_cols]
              LTS_dat[[i]][["val"]] <- LTS_dat[[i]][["val"]][, c(val_cols, "Comment")]
            }
          }
        }
        return(LTS_dat)
      }
    })

    shiny::observeEvent(input$LTS_opt,
      {
        shinyjs::toggleElement(id = "LTS_def", condition = "show_property_values" %in% input$LTS_opt)
      },
      ignoreInit = FALSE,
      ignoreNULL = FALSE
    )


    shiny::observeEvent(LTS_Data(), {
      datalist[["lts_data"]] <- LTS_Data()
    })

    # upload info used in UI part
    output$LTS_fileUploaded <- shiny::reactive({
      return(!is.null(datalist$lts_data))
    })
    shiny::outputOptions(output, "LTS_fileUploaded", suspendWhenHidden = FALSE)

    LTS_KWs <- shiny::reactive({
      shiny::req(datalist$lts_data)
      sapply(datalist$lts_data, function(x) {
        x[["def"]][, "KW"]
      })
    })

    # i() will provide the currently selected KW from the list as a numeric index throughout Server
    i <- shiny::reactiveVal(1)

    output$LTS_sel_KW <- shiny::renderUI({
      shiny::req(LTS_KWs())
      shiny::selectInput(inputId = shiny::NS(id, "LTS_sel_KW"), label = "Property", choices = LTS_KWs(), selected = shiny::isolate(i()))
    })

    shiny::observeEvent(input$LTS_sel_KW, {
      i(which(LTS_KWs() %in% input$LTS_sel_KW))
    })

    LTS_new_val <- data.frame(
      "Value" = 0.0,
      "Date" = as.Date(format(Sys.time(), "%Y-%m-%d")),
      "File" = as.character("filename"),
      "Comment" = as.character(NA), stringsAsFactors = FALSE
    )
    LTS_tmp_val <- shiny::reactiveVal(LTS_new_val)

    # Data Tables
    # current LTS values
    tab_LTSvals <- shiny::reactiveVal(shiny::isolate(datalist[["lts_data"]][[i()]][["val"]][, 1:3]))
    shiny::observeEvent(i(), {
      # select the current set of values based on i() without showing the comments (to save screen space)
      tab_LTSvals(datalist[["lts_data"]][[i()]][["val"]][, 1:3])
    })
    output$LTS_vals <- DT::renderDataTable(
      {
        shiny::req(i())
        # trigger redraw on new value and update reactive Value to this end
        input$LTS_ApplyNewValue
        tab_LTSvals(shiny::isolate(datalist[["lts_data"]][[i()]][["val"]][, 1:3]))
        styleTabL1(x = tab_LTSvals())
      },
      server = FALSE
    )

    # current LTS definition
    output$LTS_def <- DT::renderDataTable(
      {
        shiny::req(datalist$lts_data)
        out <- datalist$lts_data[[i()]][["def"]]
        out[, "Coef_of_Var"] <- formatC(round(out[, "Coef_of_Var"], 4), digits = 4, format = "f")
        # reorder and rename columns according to wish of Carsten Prinz
        out <- out[, c("RM", "KW", "KW_Def", "KW_Unit", "CertVal", "U", "U_Def", "Coef_of_Var", "acc_Datasets", "Device", "Method")]
        colnames(out) <- c("Reference Material", "Property", "Name", "Unit", "Certified value", "Uncertainty", "Uncertainty unit", "Coeff. of Variance", "accepted Datasets", "Device", "Method")
        return(out)
      },
      options = list(paging = FALSE, searching = FALSE, ordering = FALSE, dom = "t"),
      rownames = NULL
    )

    # entry table for new datapoint
    output$LTS_NewVal <- DT::renderDataTable({
      DT::datatable(
        data = LTS_new_val,
        options = list(
          paging = FALSE, searching = FALSE, ordering = FALSE, dom = "t",
          columnDefs = list(
            list("width" = "80px", "targets" = which(!(colnames(LTS_new_val) %in% c("Comment"))) - 1)
          )
        ),
        rownames = NULL, editable = TRUE
      )
    })

    # helper data.frame containing only Month and Value information of current KW
    d <- shiny::reactive({
      x <- datalist$lts_data[[i()]]
      vals <- x[["val"]][, "Value"]
      rt <- x[["val"]][, "Date"]
      mon <- round(calc_time_diff(x = rt, type = "mon", exact = TRUE), 2)
      data.frame(mon, vals)
    })

    # when a row in table was selected (either by user clicking the table or clicking in the plot)
    shiny::observeEvent(input$LTS_vals_rows_selected,
      {
        shiny::req(datalist$lts_data)
        if (!is.null(input$LTS_vals_rows_selected)) {
          # when a row is selected in table or plot change title and value
          sr <- input$LTS_vals_rows_selected # selected row
          shinyjs::enable(id = "datacomment")
          shiny::updateTextInput(
            session = session,
            inputId = "datacomment",
            label = paste0("Comment for month ", d()[sr, "mon"], " and value ", d()[sr, "vals"]),
            value = datalist[["lts_data"]][[i()]][["val"]][[sr, "Comment"]]
          )
        } else {
          # when row gets deselected/ no row is selected
          shinyjs::disable(id = "datacomment")
          shiny::updateTextInput(
            session = session,
            inputId = "datacomment",
            label = "Comment",
            value = NA
          )
        }
      },
      ignoreNULL = FALSE
    )

    # Data Figures
    output$LTS_plot1_1 <- shiny::renderPlot({
      shiny::req(datalist[["lts_data"]], i(), d())
      input$LTS_ApplyNewValue
      # $$minor ToDo$$ plot does not need to be updated if only comment was edited --> test for this situation
      # Careful! If user goes to next comment plot needs to be updated again
      plot_lts_data(x = datalist$lts_data[[i()]], type = 1)
      ### if a point in data table is selected --> mark in plot 1
      sr <- input$LTS_vals_rows_selected
      if (length(sr)) {
        graphics::points(x = rep(d()[sr, "mon"], 2), y = rep(d()[sr, "vals"], 2), pch = c(21, 4), cex = 2, col = 5)
      }
    })

    output$LTS_plot1_2 <- shiny::renderPlot({
      shiny::req(datalist[["lts_data"]], i())
      input$LTS_ApplyNewValue
      # plot_lts_data(x = datalist$lts_data[[i()]], type=ifelse(input$show_ci, 3, 2))
      plot_lts_data(x = datalist$lts_data[[i()]], type = ifelse("show_ci" %in% input$LTS_opt, 3, 2))
    })

    output$LTS_plot2 <- shiny::renderPlot({
      shiny::req(datalist[["lts_data"]], i(), input$show_plot_L3)
      input$LTS_ApplyNewValue
      # validate(need(input$show_plot_L3))
      tmp <- datalist$lts_data[[i()]]
      if (nrow(tmp[["val"]]) >= 6) {
        est <- sapply(6:nrow(tmp[["val"]]), function(i) {
          x <- tmp
          x[["val"]] <- x[["val"]][1:i, ]
          plot_lts_data(x = x, type = 0)
        })
        x <- calc_time_diff(tmp[["val"]][, "Date"], type = "mon")
        plot(x = x[-c(1:5)], y = est, xlab = "Measurement Point", ylab = "LTS month estimate (excluding initital 5 values)", xlim = range(x), pch = 24)
      }
    })

    # proxy for changing the table
    proxy <- DT::dataTableProxy("LTS_vals")

    #  when clicking on a point in the plot, select Rows in data table proxy
    shiny::observeEvent(input$plot1_hover, {
      p <- input$plot1_hover
      # print(shiny::nearPoints(d(), p, xvar = "mon", yvar = "vals", addDist = TRUE, threshold = 10))
    })
    #  when clicking on a point in the plot, select Rows in data table proxy
    shiny::observeEvent(input$plot1_click, {
      # 1/3 nearest point to click location
      a <- shiny::nearPoints(d(), input$plot1_click, xvar = "mon", yvar = "vals", addDist = TRUE, threshold = 10)
      # 2/3 index in table
      if (nrow(a) >= 2) {
        shinyWidgets::show_alert(title = "Warning", text = "More than one data point in proximity to click event. Please cross check with table entry if correct data point is selected.", type = "warning")
        a <- a[which.min(a[, "dist_"])[1], ]
      }
      idx <- which(d()$mon == a$mon & d()$vals == a$vals)
      # 3/3
      DT::selectRows(proxy = proxy, selected = idx)
      DT::selectPage(proxy = proxy, page = (idx - 1) %/% input$LTS_vals_state$length + 1)
    })


    # Edit Value/Information of new datapoint
    shiny::observeEvent(input[["LTS_NewVal_cell_edit"]], {
      cell <- input[["LTS_NewVal_cell_edit"]]
      i <- cell$row
      j <- 1 + cell$col
      tmp <- LTS_tmp_val()
      tmp[i, j] <- DT::coerceValue(val = cell$value, old = tmp[i, j])
      LTS_tmp_val(tmp)
    })

    # add new value
    shiny::observeEvent(input$LTS_ApplyNewValue, ignoreNULL = TRUE, ignoreInit = TRUE, {
      if (input$LTS_ApplyNewValue >= 1) {
        tmp <- datalist$lts_data[[i()]][["val"]]
        nval <- LTS_tmp_val()
        if (nval$Date < max(tmp$Date)) {
          shinyWidgets::show_alert(title = "Warning", text = "You added a data point for an earlier date. Resorting the table accordingly.", type = "warning")
          ord <- order(c(tmp$Date, nval$Date))
        } else {
          ord <- 1:(nrow(tmp) + 1)
        }
        datalist$lts_data[[i()]][["val"]] <- rbind(tmp, nval)[ord, ]
      }
    })

    # add new comment
    shiny::observeEvent(input$LTS_ApplyNewComment, ignoreNULL = TRUE, ignoreInit = TRUE, {
      shiny::req(input$LTS_vals_rows_selected)
      if (input$LTS_ApplyNewComment >= 1) {
        # comment input is explicitly triggered now to allow deletion of comments as well as to avoid unnecessary update of the plots
        if (input$datacomment == "") {
          datalist[["lts_data"]][[i()]][["val"]][input$LTS_vals_rows_selected, "Comment"] <- NA
        } else {
          datalist[["lts_data"]][[i()]][["val"]][input$LTS_vals_rows_selected, "Comment"] <- input$datacomment
        }
      }
    })

    # REPORT LTS
    output$Report <- shiny::downloadHandler(
      filename = paste0("LTS_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
      content = function(file) {
        # ensure that logo file and font files are in the same folder as the Rmd.
        rmdfile <- get_local_file("report_vorlage_lts.Rmd")
        logofile <- "BAMLogo2015.png"
        # font files: "BAMKlavika-Light.ttf", "BAMKlavika-Medium.ttf", "BAMKlavika-LightItalic.ttf", "BAMKlavika-MediumItalic.ttf"

        # Set up parameters to pass to Rmd document
        dat <- datalist[["lts_data"]]
        if (length(dat) >= 2 & i() >= 2) for (j in rev(1:(i() - 1))) dat[j] <- NULL
        params <- list(
          "dat" = dat,
          "logo_file" = logofile,
          "fnc" = list("plot_lts_data" = plot_lts_data)
        )

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        shiny::withProgress(
          expr = {
            incProgress(0.5)
            rmarkdown::render(
              input = rmdfile,
              output_file = file,
              output_format = "pdf_document",
              params = params,
              envir = new.env(parent = globalenv())
            )
          },
          message = "Rendering LTS Report.."
        )
      }
    )

    # BACKUP
    output$LTS_Save <- shiny::downloadHandler(
      filename = function() {
        paste0(datalist$lts_data[[i()]][["def"]][, "RM"], ".RData")
      },
      content = function(file) {
        # !! save cant handle reactiveVal object properly. has to be written to local variable first
        LTS_dat <- datalist[["lts_data"]]
        save(LTS_dat, file = file)
      },
      contentType = "RData"
    )

    # Help Files
    shiny::observeEvent(input$TabL1_link, {
      show_help("lts_tab_L1")
    })
    shiny::observeEvent(input$FigL1_link, {
      show_help("lts_fig_L1")
    })
    shiny::observeEvent(input$InputHelp, {
      show_help("lts_dataupload")
    })
  })
}
