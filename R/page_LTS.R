#' @title page_LTS.
#' @description Modul for LongTermStability Monitoring as currently (2021) used by Carsten Prinz.
#' @param id Name when called as a module in a shiny app.
#' @param test_data Provide test_data file to module.
#' @return nothing
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = bslib::page_fluid(
#'       shinyjs::useShinyjs(),
#'       eCerto:::m_longtermstabilityUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       fl <- "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/Testdaten/TS5/LTS_BAM-B003.xlsx"
#'       eCerto:::m_longtermstabilityServer(id = "test", test_data = fl)
#'     }
#'   )
#' }
#' @noRd
#' @keywords internal
m_longtermstabilityUI <- function(id) {
  ns <- shiny::NS(id)

  tab_L1_card <- bslib::card(
    id = ns("tab_L1_panel"),
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::actionLink(inputId = ns("TabL1_link"), label = "Tab.L1 - LTS data"),
      shinyWidgets::dropdownButton(
        label = "Add data point", circle = FALSE, width = "100%", inline = TRUE,
        shiny::numericInput(inputId = ns("LTS_newPoint_Val"), label = "Value", value = 0),
        shiny::dateInput(inputId = ns("LTS_newPoint_Date"), label = "Date"),
        shiny::textInput(inputId = ns("LTS_newPoint_File"), label = "File"),
        shiny::textInput(inputId = ns("LTS_newPoint_Comment"), label = "Comment"),
        shiny::actionButton(inputId = ns("LTS_newPoint_Apply"), label = "Add data")
      )
    ),
    bslib::card_body(shiny::div(DT::DTOutput(outputId = ns("tab_L1"))))
  )

  fig_L1_card <- bslib::card(
    id = ns("fig_L1_panel"),
    full_screen = TRUE,
    min_height = "460px",
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::actionLink(inputId = ns("FigL1_link"), label = "Fig.L1 - LTS calculation"),
      shiny::div(
        shiny::div(style = "float: left; margin-left: 15px;", shinyWidgets::pickerInput(inputId = ns("LTS_sel_KW"), label = NULL, choices = "", width = "200px")),
        shiny::div(style = "float: left; margin-left: 15px;", shinyWidgets::dropdownButton(
          inputId = ns("btn_Comment"), label = "Comment or Filter", circle = FALSE, width = "300px", inline = TRUE, right=FALSE,
          shinyjs::disabled(shiny::textInput(inputId = ns("datacomment"), label = "Comment text", value = "", placeholder = "Select point in Fig.L1 or row in Tab.L1 and modify comment")),
          shiny::checkboxInput(inputId = ns("dataflt"), label = "Filter datapoint", value = FALSE)
          #shiny::actionButton(inputId = ns("LTS_ApplyNewComment"), label = "Add comment")
        )),
        shiny::div(style = "float: left; margin-left: 15px; text-align: right; width: 132px;", shiny::checkboxInput(inputId = ns("LTS_opt_show_ci"), label = shiny::HTML("Use CI<sub>95</sub> (slope)"), value = TRUE))
      )
    ),
    bslib::card_body(
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          position = "right", open = "open", width = "340px",
          DT::dataTableOutput(ns("LTS_def"))
        ),
        bslib::layout_columns(
          shiny::plotOutput(ns("LTS_plot1_1"), height = "450px", click = ns("plot1_click"), hover = ns("plot1_hover")),
          shiny::plotOutput(ns("LTS_plot1_2"), height = "450px")
        )
      )
    ),
    bslib::card_footer(
      class = "d-flex justify-content-between",
      shiny::downloadButton(ns("Report"), label = "Download PDF Report"),
      shiny::div(id = ns("LTS_selected_point"), shiny::HTML("Selected data point: none")),
      shiny::downloadButton(ns("LTS_Save"), label = "Download LTS Data Backup")
    )
  )

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
      bslib::layout_columns(
        tab_L1_card,
        fig_L1_card,
        col_widths =  bslib::breakpoints(
          sm = c(12, 12),
          xl = c(3, 9)
        )
      )
    ) # conditionalPanel
  )
}

#' @noRd
#' @keywords internal
m_longtermstabilityServer <- function(id, test_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    lts <- shiny::reactiveValues("data" = NULL)

    LTS_Data <- shiny::reactive({
      if (!is.null(input$LTS_input_file) | !is.null(test_data)) {
        fl_path <- ifelse(!is.null(test_data), test_data, input$LTS_input_file$datapath[1])
        file.type <- tools::file_ext(fl_path)
        if (!tolower(file.type) %in% c("rdata", "xls", "xlsx")) {
          shinyWidgets::show_alert(title = "Wrong Filetype?", text = "Please select an RData file or an Excel file.", type = "warning")
          return(NULL)
        }
        if (tolower(file.type) == "rdata") {
          tryCatch({load(fl_path)}, error = function(e) { stop(shiny::safeError(e)) })
          if (!exists("LTS_dat")) {
            warning("Did load RData backup but could not find object 'LTS_dat' inside.")
            LTS_dat <- NULL
          }
        } else {
          LTS_dat <- read_lts_input(file = fl_path)
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
              if (!"Filter" %in% colnames(LTS_dat[[i]][["val"]])) LTS_dat[[i]][["val"]] <- cbind(LTS_dat[[i]][["val"]], "Filter" = rep(FALSE, nrow(LTS_dat[[i]][["val"]])))
              if (!inherits(LTS_dat[[i]][["val"]][, "Date"], "Date")) {
                LTS_dat[[i]][["val"]][, "Date"] <- as.Date.character(LTS_dat[[i]][["val"]][, "Date"], tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%Y/%m/%d"))
              }
              if (!inherits(LTS_dat[[i]][["val"]][, "Date"], "Date")) e_msg("Sorry, could not convert column 'Date' into correct format.")
              if (!LTS_dat[[i]][["def"]][, "U_Def"] %in% c("1s", "2s", "CI", "1sx", "2sx")) e_msg("Sorry, unexpected value in 'U_Def'. Allowed: '1s', '2s', 'CI', '1sx' and '2sx'. Please check.")
              shiny::validate(shiny::need(inherits(LTS_dat[[i]][["val"]][, "Date"], "Date"), "Sorry, could not convert column 'Date' into correct format."))
              shiny::validate(shiny::need(LTS_dat[[i]][["def"]][, "U_Def"] %in% c("1s", "2s", "CI", "1sx", "2sx"), "Sorry, unexpected value in 'U_Def'. Allowed: '1s', '2s', 'CI', '1sx' and '2sx'. Please check."))
              LTS_dat[[i]][["def"]] <- LTS_dat[[i]][["def"]][, def_cols]
              LTS_dat[[i]][["val"]] <- LTS_dat[[i]][["val"]][, c(val_cols, "Comment", "Filter")]
            }
          }
        }
        return(LTS_dat)
      }
    })

    shiny::observeEvent(LTS_Data(), {
      lts[["data"]] <- LTS_Data()
    })

    # upload info used in UI part
    output$LTS_fileUploaded <- shiny::reactive({
      return(!is.null(lts$data))
    })
    shiny::outputOptions(output, "LTS_fileUploaded", suspendWhenHidden = FALSE)

    LTS_KWs <- shiny::reactive({
      shiny::req(lts$data)
      kw_names <- sapply(lts$data, function(x) { x[["def"]][, "KW"] })
      return(kw_names)
    })

    shiny::observeEvent(LTS_KWs, {
      #shiny::updateSelectInput(inputId = "LTS_sel_KW", choices = LTS_KWs(), selected = LTS_KWs()[1])
      shinyWidgets::updatePickerInput(inputId = "LTS_sel_KW", choices = LTS_KWs(), selected = LTS_KWs()[1])
    }, ignoreInit = FALSE)

    # i() will provide the currently selected KW from the list as a numeric index throughout Server
    i <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$LTS_sel_KW, {
      if (!identical(i(), which(LTS_KWs() %in% input$LTS_sel_KW))) {
        i(which(LTS_KWs() %in% input$LTS_sel_KW))
      }
    }, ignoreInit = TRUE)

    LTS_new_val <- data.frame(
      "Value" = 0.0,
      "Date" = as.Date(format(Sys.time(), "%Y-%m-%d")),
      "File" = as.character("filename"),
      "Comment" = as.character(NA), stringsAsFactors = FALSE
    )
    LTS_tmp_val <- shiny::reactiveVal(LTS_new_val)

    # Data Tables
    # current LTS values
    tab_LTSvals <- shiny::reactiveVal(shiny::isolate(lts[["data"]][[i()]][["val"]][, 1:3]))
    shiny::observeEvent(i(), {
      # select the current set of values based on i() without showing the comments (to save screen space)
      tab_LTSvals(lts[["data"]][[i()]][["val"]][, 1:3])
    })
    output$tab_L1 <- DT::renderDataTable(
      {
        shiny::req(i())
        # trigger redraw on new value and update reactive Value to this end
        input$LTS_newPoint_Apply
        tab_LTSvals(shiny::isolate(lts[["data"]][[i()]][["val"]][, 1:3]))
        styleTabL1(x = tab_LTSvals())
      },
      server = FALSE
    )

    # current LTS definition
    output$LTS_def <- DT::renderDataTable(
      {
        shiny::req(lts$data, i())
        out <- lts$data[[i()]][["def"]]
        out[, "Coef_of_Var"] <- formatC(round(out[, "Coef_of_Var"], 4), digits = 4, format = "f")
        # reorder and rename columns according to wish of Carsten Prinz
        out <- out[, c("RM", "KW", "KW_Def", "KW_Unit", "CertVal", "U", "U_Def", "Coef_of_Var", "acc_Datasets", "Device", "Method")]
        colnames(out) <- c("Reference Material", "Property", "Name", "Unit", "Certified value", "Uncertainty", "Uncertainty unit", "Coeff. of Variance", "accepted Datasets", "Device", "Method")
        rownames(out) <- out[,"Property"]
        return(t(out))
      },
      options = list(paging = FALSE, searching = FALSE, ordering = FALSE, dom = "t")
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
      req(lts$data[[i()]])
      x <- lts$data[[i()]]
      vals <- x[["val"]][, "Value"]
      rt <- x[["val"]][, "Date"]
      mon <- round(calc_time_diff(x = rt, type = "mon", exact = TRUE), 2)
      data.frame(mon, vals)
    })

    # when a row in table was selected (either by user clicking the table or clicking in the plot)
    shiny::observeEvent(input$tab_L1_rows_selected,
      {
        shiny::req(lts$data)
        if (!is.null(input$tab_L1_rows_selected)) {
          # when a row is selected in table or plot change title and value
          sr <- input$tab_L1_rows_selected # selected row
          comm <- lts[["data"]][[i()]][["val"]][[sr, "Comment"]]
          shinyjs::enable(id = "datacomment")
          shinyjs::enable(id = "dataflt")
          shiny::updateTextInput(session = session, inputId = "datacomment", value = comm)
          shiny::updateCheckboxInput(inputId = "dataflt", value = lts[["data"]][[i()]][["val"]][sr, "Filter"])
          if (!is.na(comm)) comm <- paste("<br>Comment:", comm) else comm <- ""
          shinyjs::html(id = "LTS_selected_point", html = paste0("Selected data point: month ", d()[sr, "mon"], " and value ", round(d()[sr, "vals"],4), comm))
        } else {
          # when row gets deselected/ no row is selected
          shinyjs::disable(id = "datacomment")
          shiny::updateTextInput(session = session, inputId = "datacomment", value = NA)
          shinyjs::disable(id = "dataflt")
          shiny::updateCheckboxInput(inputId = "dataflt", value = FALSE)
          shinyjs::html(id = "LTS_selected_point", html = "Selected data point: none")
        }
      },
      ignoreNULL = FALSE
    )

    # Data Figures
    output$LTS_plot1_1 <- shiny::renderPlot({
      shiny::req(lts[["data"]], i(), d())
      input$LTS_newPoint_Apply
      e_msg("Render LTS_plot1_1")
      # $$minor ToDo$$ plot does not need to be updated if only comment was edited --> test for this situation
      # Careful! If user goes to next comment plot needs to be updated again
      plot_lts_data(x = lts$data[[i()]], type = 1)
      ### if a point in data table is selected --> mark in plot 1
      sr <- input$tab_L1_rows_selected
      if (length(sr)) {
        graphics::points(x = rep(d()[sr, "mon"], 2), y = rep(d()[sr, "vals"], 2), pch = c(21, 4), cex = 2, col = 5)
      }
    })

    output$LTS_plot1_2 <- shiny::renderPlot({
      shiny::req(lts[["data"]], i())
      input$LTS_newPoint_Apply
      e_msg("Render LTS_plot1_2")
      #remove filtered Values from Fig.L1-2
      x <- lts$data[[i()]]
      if (any(x[["val"]][,"Filter"])) {
        x[["val"]] <- x[["val"]][!x[["val"]][,"Filter"],]
      }
      plot_lts_data(x = x, type = ifelse(input$LTS_opt_show_ci, 3, 2))
    })

    # proxy for changing the table
    proxy <- DT::dataTableProxy("tab_L1")

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
      DT::selectPage(proxy = proxy, page = (idx - 1) %/% input$tab_L1_state$length + 1)
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
    shiny::observeEvent(input$LTS_newPoint_Apply, ignoreNULL = TRUE, ignoreInit = TRUE, {
      if (input$LTS_newPoint_Apply >= 1) {
        tmp <- lts$data[[i()]][["val"]]
        nval <- LTS_new_val
        nval[1,"Value"] <- input$LTS_newPoint_Val
        nval[1,"Date"] <- input$LTS_newPoint_Date
        nval[1,"File"] <- input$LTS_newPoint_File
        nval[1,"Comment"] <- ifelse(input$LTS_newPoint_Comment=="", as.character(NA), input$LTS_newPoint_Comment)
        if (nval$Date < max(tmp$Date)) {
          shinyWidgets::show_alert(title = "Warning", text = "You added a data point for an earlier date. Resorting the table accordingly.", type = "warning")
          ord <- order(c(tmp$Date, nval$Date))
        } else {
          ord <- 1:(nrow(tmp) + 1)
        }
        lts$data[[i()]][["val"]] <- rbind(tmp, nval)[ord, ]
      }
    })

    # add new comment and set filter
    shiny::observeEvent(input$btn_Comment_state, {

      if (input$btn_Comment_state) {

      } else {
        if (any(input$tab_L1_rows_selected)) {
          lts[["data"]][[i()]][["val"]][input$tab_L1_rows_selected, "Comment"] <- ifelse(input$datacomment == "", NA, input$datacomment)
          sr <- input$tab_L1_rows_selected # selected row
          comm <- lts[["data"]][[i()]][["val"]][sr, "Comment"]
          if (!is.na(comm)) comm <- paste("<br>Comment:", comm) else comm <- ""
          shinyjs::html(id = "LTS_selected_point", html = paste0("Selected data point: month ", d()[sr, "mon"], " and value ", round(d()[sr, "vals"],4), comm))
          lts[["data"]][[i()]][["val"]][sr, "Filter"] <- input$dataflt
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
        dat <- lts[["data"]]
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
        paste0(lts$data[[i()]][["def"]][, "RM"], ".RData")
      },
      content = function(file) {
        # !! save cant handle reactiveVal object properly. has to be written to local variable first
        LTS_dat <- lts[["data"]]
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
