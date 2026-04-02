#' @title Validation-Page DIN 5725-2.
#' @description \code{page_validation57252} is the module for validation according to DIN 5725-2.
#' @details Not yet.
#' @param id Name when called as a module in a shiny app.
#' @param test_data Provide test_data to module.
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = bslib::page_fluid(
#'       eCerto:::page_validation57252UI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       #fl <- "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/Validierung/DIN5725-2/testdata.xlsx"
#'       #td <- openxlsx::read.xlsx(xlsxFile = fl, sheet = 1)
#'       td <- init_V2_data()
#'       td <- rbind(
#'         cbind("Property" = "prop1", "Unit" = "kg/m^2^", td),
#'         cbind("Property" = "prop2", "Unit" = "H~2~O", td)
#'        )
#'       #td <- NULL
#'       eCerto:::page_validation57252Server(id = "test", test_data = td)
#'     }
#'   )
#' }
#' @return Nothing
#' @noRd

page_validation57252UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::conditionalPanel(
      condition = "output.V_fileUploaded == false",
      ns = ns, # namespace of current module
      shiny::fileInput(
        inputId = ns("inp_file"),
        label = shiny::actionLink(inputId = ns("InputHelp"), "Import Excel/RData File"),
        multiple = FALSE,
        placeholder = "xlsx | Rdata",
        accept = c("xlsx", "RData")
      ),
      shiny::div(
        shiny::p(shiny::helpText("Example Table (generic format). Optional columns in grey. Column order is not relevant but column names are. 'Lab' and 'Level' can be specified as number or text, 'Replicate' and 'Value' have to be numbers.")),
        shiny::uiOutput(outputId = ns("example_table_generic"))
      )
    ),
    shiny::conditionalPanel(
      condition = "output.V_fileUploaded == true",
      ns = ns, # namespace of current module
      bslib::card(
        id = ns("v2_test"),
        fillable = FALSE,
        bslib::card_header("DIN 5725-2 output panel"),
        bslib::card_body(
          fill = FALSE,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              position = "left", open = "open", width = "480px",
              shiny::div(id = ns("ori_inp_file_name"), "This div will show the original Excel File name used upon import if shinyjs is active."),
              bslib::layout_columns(
                shiny::numericInput(inputId = ns("opt_tab_precision"), value = 3, step = 1, min = 0, max = 6, label = "Table digits precision"),
                shiny::selectInput(inputId = ns("opt_cur_analyte"), label = "Select Property", choices = "", multiple = FALSE)
              ),
              bslib::layout_columns(
                shiny::uiOutput(ns("TabV0")),
                shiny::uiOutput(ns("filter_tree_ui"))
              )
            ),
            shiny::uiOutput(ns("TabV1")),
            shiny::uiOutput(ns("FigV1")),
            shiny::uiOutput(ns("TabV2")),
            shiny::uiOutput(ns("FigV2")),
            shiny::uiOutput(ns("TabV3")),
            bslib::layout_column_wrap(
              width = "400px", fixed_width = TRUE,
              shiny::plotOutput(ns("FigV3a"), width = "400px", height = "400px"),
              shiny::plotOutput(ns("FigV3b"), width = "400px", height = "400px")
            )
          )
        )
      )
    )
  )

}

#' @noRd
page_validation57252Server <- function(id, test_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- shiny::NS(id)

    # User pars for V2 module ====
    V2_pars <- shiny::reactiveValues(
      "opt_tab_precision" = 3,
      "opt_cur_analyte" = "",
      "inp_file_path" = "",
      "ori_inp_file_name" = "",
      "excl_ids" = integer(0),
      "par_update" = 0
    )

    shiny::observeEvent(input$opt_tab_precision, {
      if (!identical(V2_pars$opt_tab_precision, as.numeric(input$opt_tab_precision))) V2_pars$opt_tab_precision <- as.numeric(input$opt_tab_precision)
    })
    shiny::observeEvent(input$opt_cur_analyte, {
      V2_pars$opt_cur_analyte <- input$opt_cur_analyte
    })

    # generic input table example
    output$example_table_generic <- renderUI({
      x <- cbind("Property" = "measurand name", "Unit" = "measurand unit", init_V2_data()[1:16,])
      ft <- show_upload_example_table(x=x, max_char = 15, optional = c(1,2,7,8))
      flextable::htmltools_value(ft, ft.align = "left")
    })

    # Upload & Data preparation ====
    # upload info used in UI part
    output$V_fileUploaded <- shiny::reactive({
      return(!is.null(input$inp_file$datapath) | !is.null(test_data))
    })
    shiny::outputOptions(output, "V_fileUploaded", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$inp_file$datapath, {
      shinyjs::html(id = "ori_inp_file_name", html = shiny::HTML("Data imported from file: ", input$inp_file$name))
      # keep name of XLSX file
      if (tolower(tools::file_ext(input$inp_file$name)) == "xlsx") V2_pars$ori_inp_file_name <- input$inp_file$name
      V2_pars$inp_file_path <- normalizePath(input$inp_file$datapath)
    })

    # Reactives ====
    inp_raw <- shiny::reactive({
      # reset pars
      shiny::isolate({
        V2_pars$opt_cur_analyte <- ""
        V2_pars$excl_ids <- integer(0)
      })
      if (!is.null(test_data)) {
        df <- test_data
      } else {
        req(V2_pars$inp_file_path)
        x <- V2_pars$inp_file_path
        if (tolower(tools::file_ext(x)) %in% c("rdata", "rda")) {
          v_env <- new.env()
          load(file = x, envir = v_env)
          shiny::isolate(V2_pars$par_update <- V2_pars$par_update + 1)
          df <- get("eCerto_V_backup", envir = v_env)[["tab"]]
        } else {
          df <- openxlsx::read.xlsx(xlsxFile = x, sheet = 1)
        }
      }
      req(df)
      if (!"ID" %in% colnames(df) | "ID" %in% colnames(df) && any(duplicated(df[,"ID"]))) {
        df[,"ID"] <- seq_len(nrow(df))
      }
      if (!"Filter" %in% colnames(df)) {
        # either assign Filter column...
        df[,"Filter"] <- ""
      } else {
        # .. or update Filter parameter
        V2_pars$excl_ids <- df[df[,"Filter"]!="","ID"]
      }
      if ("Property" %in% colnames(df)) {
        choices <- unique(df[,"Property"])
        shiny::updateSelectInput(inputId = "opt_cur_analyte", choices = choices)
        shinyjs::show(id = "opt_cur_analyte")
      } else {
        shinyjs::hide(id = "opt_cur_analyte")
      }
      return(df)
    })

    inp_with_pars_applied <- shiny::reactive({
      req(inp_raw())
      df <- inp_raw()
      if ("Property" %in% colnames(df)) {
        if (V2_pars$opt_cur_analyte %in% df[,"Property"]) {
          return(df[df[,"Property"] == V2_pars$opt_cur_analyte,])
        } else {
          return(NULL)
        }
      } else {
        return(df)
      }
    })

    # Filtered data for all downstream analysis ====
    inp <- shiny::reactive({
      req(inp_with_pars_applied())
      df <- inp_with_pars_applied()
      if (length(V2_pars$excl_ids) > 0L) {
        df[!(df$ID %in% V2_pars$excl_ids),"Filter"] <- ""
        df[which(df$ID %in% V2_pars$excl_ids)[df[df$ID %in% V2_pars$excl_ids,"Filter"]==""], "Filter"] <- "Removed due to..."
      } else {
        df[,"Filter"] <- ""
      }
      df
    })

    mns <- shiny::reactive({
      req(inp())
      V2_calc_stats(inp = inp())
    })

    res <- shiny::reactive({
      req(mns())
      prepTabV2_3(mns = mns())
    })

    # Tree-based filter UI (Lab -> Level -> ID) ====
    output$filter_tree_ui <- shiny::renderUI({
      req(inp())
      message("This is the setup of the V2 filter UI. It should fire only at data upload or when a different analyte is selected!!")
      df <- inp()[,c("Lab","Level","Replicate","ID","Filter")]
      df$Lab   <- as.character(paste("Lab", df$Lab))
      df$Level <- as.character(paste("Level", df$Level))
      df$Replicate <- as.character(paste("Replicate", df$Replicate))
      shiny::tags$details(
        shiny::tags$summary("Filter (Lab / Lev / Rep)"),
        shinyWidgets::treeInput(
          inputId = ns("f_tree_excl"),
          #label = "Filter (Lab / Lev / Rep)",
          label = NULL,
          choices = shinyWidgets::create_tree(data = df[,c("Lab","Level","Replicate","ID")]),
          selected = as.character(df[df[,"Filter"]!="","ID"])
        )
      )
    })

    # Recompute excluded IDs when tree selection changes ====
    shiny::observeEvent(input$f_tree_excl, {
      sel <- suppressWarnings(c(stats::na.omit(as.numeric(input$f_tree_excl))))
      cur_exc <- V2_pars$excl_ids
      cur_ids <- shiny::isolate(inp()[,"ID"])
      # remove all IDs from the current analyte from the global par
      cur_exc <- cur_exc[!(cur_exc %in% cur_ids)]
      # add all IDs from the tree object (Filter)
      nv <- sort(c(cur_exc, sel))
      V2_pars$excl_ids <- nv
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    # Tables ====
    output$TabV0 <- shiny::renderUI({
      req(inp())
      ft <- prepTabV2_0(inp = inp(), excl_ids = V2_pars$excl_ids, output = "ftl", id = "Tab.V0", caption = "Input data, grouped per cell (levels in columns, filtered data in red)")
      flextable::htmltools_value(ft, ft.align = "left")
    })

    output$TabV1 <- shiny::renderUI({
      req(mns(), V2_pars$opt_tab_precision)
      ft <- prepTabV2_1(mns = mns(), prec = V2_pars$opt_tab_precision, output = "ftl", id = "Tab.V1", caption = "Cell means, standard deviations (or diff for n=2) and number of finite, non-excluded measurement replicates per cell")
      flextable::htmltools_value(ft, ft.align = "left")
    })

    output$TabV2 <- shiny::renderUI({
      req(mns(), V2_pars$opt_tab_precision)
      n_q <- length(unique(mns()[,"Level"]))
      fts <- lapply(1:n_q, function(q) {
        ft <- prepTabV2_2(inp = inp(), q = q, prec = V2_pars$opt_tab_precision, output = "ftl", id = paste0("Tab.V2", letters[q]), caption = paste("Statistic values for Level", q))
        flextable::htmltools_value(ft, ft.align = "left")
      })
      bslib::layout_column_wrap(width = "520px", fixed_width = TRUE, !!!fts)
    })

    output$TabV3 <- shiny::renderUI({
      req(res(), V2_pars$opt_tab_precision)
      ft <- styleTabV2_3(x = res(), prec = V2_pars$opt_tab_precision, output = "ftl", id = "Tab.V3", caption = "Calculated repeatability values")
      flextable::htmltools_value(ft, ft.align = "left")
    })

    # Figures ====
    output$FigV1 <- shiny::renderUI({
      req(inp())
      h <- paste0(240+20*length(unique(inp()[,"Lab"])), "px")
      plots <- lapply(1:length(unique(inp()[,"Level"])), function(x) {
        local({
          local_x <- x
          plot_output <- shiny::plotOutput(outputId = ns(paste0("plot_v1_", local_x)), height = h)
          output[[paste0("plot_v1_", local_x)]] <- renderPlotHD({
            prepFigV2_1(inp = inp(), q = local_x)
          })
          plot_output
        })
      })
      do.call(bslib::layout_column_wrap, c(list(width = "520px", fixed_width = TRUE), plots))
    })

    output$FigV2 <- shiny::renderUI({
      req(mns())
      w <- paste0(120+nrow(mns())*20, "px")
      bslib::layout_column_wrap(
        width = w, fixed_width = TRUE,
        shiny::plotOutput(ns("FigV2a"), width = w, height = "400px"),
        shiny::plotOutput(ns("FigV2b"), width = w, height = "400px")
      )
    })

    output$FigV2a <- renderPlotHD({
      req(mns())
      prepFigV2_2(mns(), type = "h")
    })

    output$FigV2b <- renderPlotHD({
      req(mns())
      prepFigV2_2(mns(), type = "k")
    })

    output$FigV3a <- renderPlotHD({
      req(res())
      prepFigV2_3(res()[,c(3,4)])
    })

    output$FigV3b <- renderPlotHD({
      req(res())
      prepFigV2_3(res()[,c(3,5)])
    })

  })
}
