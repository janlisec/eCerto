#' @title page_DRMD.
#' @description Modul for DRMD processing.
#' @param id Name when called as a module in a shiny app.
#' @param test_data Provide test_data file to module.
#' @return nothing
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = bslib::page_fluid(
#'       shinyjs::useShinyjs(),
#'       eCerto:::page_DRMDUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       fl <- "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/DRMD/drmc-007.xml"
#'       eCerto:::page_DRMDServer(id = "test", test_data = fl)
#'     }
#'   )
#' }
#' @noRd
#' @keywords internal
page_DRMDUI <- function(id) {
  ns <- shiny::NS(id)

  tab_D1_card <- bslib::card(
    id = ns("tab_D1_panel"),
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::actionLink(inputId = ns("TabD1_link"), label = "Tab.D1 - Admin data"),
      shinyWidgets::dropdownButton(
        label = "Modify Admin Data", circle = FALSE, width = "100%", inline = TRUE,
        shiny::actionButton(inputId = ns("D_add_admin"), label = "Add term"),
        shiny::actionButton(inputId = ns("D_rem_admin"), label = "Rem term")
      )
    ),
    bslib::card_body(shiny::div(DT::DTOutput(outputId = ns("tab_D1"))), max_height = 920),
    bslib::card_footer(
      class = "d-flex justify-content-between",
      shiny::div(id = ns("D_level_path"), "D_level_path"),
      shiny::div(
        shiny::textAreaInput(ns("D1_current_value"), label = NULL, rows = 5, width = "520px"),
        shiny::actionButton(inputId = ns("btn_D1_modify"), label = "Apply", height = "36px")
      )
    )
  )

  tab_D2_card <- bslib::card(
    id = ns("tab_D2_panel"),
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::actionLink(inputId = ns("TabD2_link"), label = "Tab.D2 - Quant data"),
      shiny::div(
        shiny::div(id = ns("Result_set_annotations"), style = "float: left; margin-left: 15px; text-align: right; width: 420px;", "Result set annotations"),
        # "D_sel_i" will hold all available 'Results' nodes
        shiny::div(style = "float: left; margin-left: 15px;", shinyWidgets::pickerInput(inputId = ns("D_sel_i"), label = NULL, choices = "", width = "200px"))
        # shiny::div(style = "float: left; margin-left: 15px;", shinyWidgets::dropdownButton(
        #   inputId = ns("btn_Comment"), label = "Comment or Filter", circle = FALSE, width = "300px", inline = TRUE, right=FALSE,
        #   shinyjs::disabled(shiny::textInput(inputId = ns("datacomment"), label = "Comment text", value = "", placeholder = "Select point in Fig.L1 or row in Tab.L1 and modify comment")),
        #   shiny::checkboxInput(inputId = ns("dataflt"), label = "Filter datapoint", value = FALSE)
        #   #shiny::actionButton(inputId = ns("LTS_ApplyNewComment"), label = "Add comment")
        # )),
      )
    ),
    bslib::card_body(
      bslib::layout_sidebar(
        padding = 0,
        sidebar = bslib::sidebar(
          position = "right", open = FALSE, width = "280px",
          shiny::div(
            shiny::div("Here could be a form where values of the selected 'quant' item from Tab.D2 are modified.")
          )
        ),
        shiny::div(DT::DTOutput(outputId = ns("tab_D2")))
      )
    ),
    bslib::card_footer(
      class = "d-flex justify-content-between",
      shiny::downloadButton(ns("Report"), label = "Download Report"),
      shiny::downloadButton(ns("D_Save"), label = "Download modified XML")
    )
  )

  tab_D3_card <- bslib::card(
    id = ns("tab_D3_panel"),
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::actionLink(inputId = ns("TabD1_link"), label = "Tab.D3 - Full XML data")
    ),
    bslib::card_body(shiny::div(DT::DTOutput(outputId = ns("tab_D3"))), max_height = 920)
  )

  shiny::tagList(
    shiny::conditionalPanel(
      condition = "output.D_fileUploaded == false",
      ns = ns, # namespace of current module
      shiny::fileInput(
        inputId = ns("D_input_file"),
        label = shiny::actionLink(inputId = ns("InputHelp"), "Import XML File"),
        multiple = FALSE,
        accept = c("xml")
      )
    ),
    shiny::conditionalPanel(
      condition = "output.D_fileUploaded == true",
      ns = ns, # namespace of current module
      bslib::layout_columns(
        tab_D1_card,
        tab_D2_card,
        tab_D3_card,
        col_widths =  bslib::breakpoints(
          sm = c(12, 12, 12),
          xl = c(4, 8, 12)
        )
      )
    )
  )
}

#' @noRd
#' @keywords internal
page_DRMDServer <- function(id, test_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    D <- shiny::reactiveValues(
      "xml_file" = NULL,
      "data" = NULL,
      "data_mod" = NULL,
      "tab_D1" = NULL,
      "tab_D1_i" = NULL,
      "tab_D2" = NULL,
      "tab_D3" = NULL,
      "all_i" = NULL,
      "i" = NULL,
      "all_j" = NULL,
      "j" = NULL
    )

    # reactives
    D_data <- shiny::reactive({
      if (!is.null(input$D_input_file) | !is.null(test_data)) {
        fl_path <- ifelse(!is.null(test_data), test_data, input$D_input_file$datapath[1])
        file.type <- tools::file_ext(fl_path)
        #browser()
        if (!tolower(file.type) %in% c("xml")) {
          shinyWidgets::show_alert(title = "Wrong Filetype?", text = "Please select an XML file.", type = "warning")
          return(NULL)
        }
        D_dat <- read_drmd_xml(fl_path)
        # perform checks
        check_validity <- TRUE
        #while (!is.null(D_dat) && check_validity) {
          # check that DRMD format is correct
            #e_msg("Sorry, could not...")
        #}
        return(D_dat)
      } else {
        return(NULL)
      }
    })

    # observers
    # upload info used in UI part
    output$D_fileUploaded <- shiny::reactive({
      return(!is.null(input$D_input_file$datapath) | !is.null(test_data))
    })
    shiny::outputOptions(output, "D_fileUploaded", suspendWhenHidden = FALSE)

    # data and parameters as reactiveVal object
    shiny::observeEvent(D_data(), {
      if (is.null(D_data())) {
        D$data <- NULL
        D$data_mod <- NULL
        D$xml_file <- NULL
        D$tab_D1 <- NULL
        D$tab_D1_i <- NULL
        D$tab_D2 <- NULL
        D$tab_D3 <- NULL
        D$all_i <- NULL
        D$i <- NULL
      } else {
        D$data <- D_data()
        D$data_mod <- NULL
        D$xml_file <- input$D_input_file$name[1]
        #D$tab_D1 <- xml2df(D_data(), type = "admin")
        D$tab_D1 <- filter_flattened_list(flatten_list_to_df(D_data()), flt = "^1_1")
        D$tab_D1_i <- 1
        #D$tab_D2 <- xml2df(D_data(), type = "quant")
        #D$all_i <- unique(D$tab_D2$L3)
        #D$i <- unique(D$tab_D2$L3)[1]
        D$tab_D2 <- filter_flattened_list(flatten_list_to_df(D_data()), flt = "^1_2")
        idx_results <- unique(sapply(strsplit(D$tab_D2[,"idx"],"_"),function(x){x[3]}))
        D$all_i <- idx_results
        D$i <- idx_results[length(idx_results)]
        D$tab_D3 <- flatten_list_to_df(D_data())
      }
    }, ignoreNULL = FALSE)

    shiny::observeEvent(D$all_i, {
      shinyWidgets::updatePickerInput(inputId = "D_sel_i", choices = D$all_i, selected = D$all_i[1])
    })

    shiny::observeEvent(input$D_sel_i, {
      if (!identical(D$i, input$D_sel_i)) D$i <- input$D_sel_i
    })

    shiny::observeEvent(input$btn_D1_modify, {
      if (!identical(D$tab_D1[input$tab_D1_rows_selected, "value"], input$D1_current_value)) {
        # apply checks for new value
        # tbd

        # assign new value to Tab.D1
        D$tab_D1[input$tab_D1_rows_selected,"value"] <- input$D1_current_value

        # assign new value to modified data list
        if (is.null(D$data_mod)) D$data_mod <- D$data
        # ===
        # old version
        #ele <- stats::na.omit(unname(unlist(D$tab_D1[input$tab_D1_rows_selected,-ncol(D$tab_D1)])))
        # new version
        ele <- as.numeric(strsplit(D$tab_D1[input$tab_D1_rows_selected,"idx"], "_")[[1]])
        # ===
        purrr::pluck(D$data_mod, !!!ele)[[1]] <- input$D1_current_value
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$D1_current_value, {
      shinyjs::toggleState(id = "btn_D1_modify", condition = !identical(input$D1_current_value, D$tab_D1[input$tab_D1_rows_selected,"value"]))
    }, ignoreInit = TRUE)


    shiny::observeEvent(D$i, {
      req(D$tab_D2)
      txt <- "new text"
      L3 <- sapply(strsplit(D$tab_D2[,"idx"],"_"), function(x) { x[3] })
      L6 <- sapply(strsplit(D$tab_D2[,"idx"],"_"), function(x) { x[6] })
      txt <- D$tab_D2[which(L3 == D$i & L6 == 1),"value"]
      #browser()
      if (length(txt)>=1 && !all(is.na(txt))) shinyjs::html(id = "Result_set_annotations", html = shiny::HTML(paste(txt, sep="<br>")))
    })

    # tables
    # the admin data
    output$tab_D1 <- DT::renderDataTable({
      shiny::req(D$tab_D1)
      styleTabD1(df = D$tab_D1, selected = shiny::isolate(D$tab_D1_i))
    })

    # the quantified data
    output$tab_D2 <- DT::renderDataTable({
      shiny::req(D$tab_D2)
      styleTabD2(df = D$tab_D2, L3 = D$i)
    })

    output$tab_D3 <- DT::renderDataTable({
      shiny::req(D$tab_D3)
      styleTabD3(df = D$tab_D3)
    })

    # table observers
    # when a row in table was selected (either by user clicking the table or clicking in the plot)
    shiny::observeEvent(input$tab_D1_rows_selected, {
      i <- input$tab_D1_rows_selected
      D$tab_D1_i <- i
      shinyjs::toggle(id = "D1_current_value", condition = !is.null(i))
      shinyjs::toggle(id = "btn_D1_modify", condition = !is.null(i))
      shinyjs::toggle(id = "D_level_path", condition = !is.null(i))
      if (!is.null(i)) {
        x <- D$tab_D1[input$tab_D1_rows_selected,]
        shiny::updateTextAreaInput(inputId = "D1_current_value", value = unname(unlist(x[,"value"])))
        # ====
        # old Version
        #tab_cap <- rev(unname(rev(stats::na.omit(unlist(x)))[-1]))
        # new version
        tab_cap <- sub("^[^:]*:", "", strsplit(x[,"path"], "_")[[1]])
        # ====
        shinyjs::html(id = "D_level_path", paste(tab_cap, collapse="<br>"))
        #shinyjs::disable(id = "btn_D1_modify")
      }
    }, ignoreNULL = FALSE)


    # REPORT DRMD
    output$Report <- shiny::downloadHandler(
      filename = paste0("DRMD_Report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
      content = function(file) {
        # ensure that logo file and font files are in the same folder as the Rmd.
        rmdfile <- get_local_file("report_vorlage_drmd.Rmd")
        logofile <- "BAMLogo2015.png"
        # font files: "BAMKlavika-Light.ttf", "BAMKlavika-Medium.ttf", "BAMKlavika-LightItalic.ttf", "BAMKlavika-MediumItalic.ttf"

        # Set up parameters to pass to Rmd document
        D <- shiny::reactiveValuesToList(D)
        params <- list(
          "D" = D,
          "logo_file" = logofile
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
              output_format = "html_document",
              params = params,
              envir = new.env(parent = globalenv())
            )
          },
          message = "Rendering DRMD Report.."
        )
      }
    )

    shiny::observeEvent(D$data_mod, {
      #message("D$data_mod status", is.null(D$data_mod))
      shinyjs::toggle(id = "D_Save", condition = !is.null(D$data_mod))
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    # BACKUP
    output$D_Save <- shiny::downloadHandler(
      filename = function() { ifelse(is.null(D$xml_file), "test.xml", D$xml_file) },
      content = function(file) {
        # convert modified list back to xml structure and save to disc
        xml_mod <- xml2::as_xml_document(x = D$data_mod)
        xml2::write_xml(x = xml_mod, file = file)
      },
      contentType = "XML"
    )

    # Help Files
    shiny::observeEvent(input$InputHelp, {
      show_help("drmd_dataupload")
    })
    shiny::observeEvent(input$TabD1_link, {
      show_help("drmd_tab_D1")
    })
    shiny::observeEvent(input$TabD2_link, {
      show_help("drmd_tab_D2")
    })
  })
}
