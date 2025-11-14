#' @name xlsx_range_select
#' @aliases m_xlsx_range_select_UI
#' @aliases m_xlsx_range_select_Server
#'
#' @title A module to preview and select a range from a XLSX File.
#'
#' @description \code{xlsx_range_select} will provide a preview for an excel data file
#'     and allow the user to specify a range by mouse click(s).
#'
#' @details not yet
#'
#' @param id Module ID when called in a shiny app.
#' @param current_file_input Shiny fileInput referencing excel file(s).
#' @param sheet Number of the sheet to preview.
#' @param file Number of the file to preview.
#' @param excelformat Selected sub format as reactive string.
#'
#' @return A reactiveValues list with \code{start_col}, \code{end_col}, \code{tab_flt}
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyjs::useShinyjs(),
#'       shiny::fluidRow(
#'         shiny::column(3, shiny::fileInput(inputId = "x", label = "Modul parameter: x", accept = "xlsx", multiple = TRUE)),
#'         shiny::column(3, shiny::numericInput(inputId = "sheet", label = "Modul parameter: sheet", value = 1)),
#'         shiny::column(3, shiny::selectInput(inputId = "excelformat", label = "Modul parameter: excelformat", choices = c("Certification", "Homogeneity", "Stability")))
#'       ),
#'       shiny::hr(),
#'       eCerto:::m_xlsx_range_select_UI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       out <- eCerto:::m_xlsx_range_select_Server(
#'         id = "test",
#'         current_file_input = reactive({
#'           input$x
#'         }),
#'         sheet = reactive({
#'           input$sheet
#'         }),
#'         excelformat = reactive({
#'           input$excelformat
#'         })
#'       )
#'       shiny::observeEvent(out$rng, {
#'         print(out$rng)
#'       })
#'     }
#'   )
#' }
#'
#' @noRd
#' @keywords internal
m_xlsx_range_select_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    # [JL] calling useShinyjs() here is required because shinyjs::hidden statements do not work otherwise
    shinyjs::useShinyjs(),
    bslib::card(
      id = ns("range_select_card"),
      bslib::card_header(shiny::uiOutput(outputId = ns("uitxt"))),
      shiny::div(DT::DTOutput(outputId = ns("uitab")))
    )
  )
}

#' @noRd
#' @keywords internal
m_xlsx_range_select_Server <- function(id, current_file_input = shiny::reactive({NULL}), sheet = shiny::reactive({1}), file = shiny::reactive({1}), excelformat = shiny::reactive({"Certification"})) {

  stopifnot(shiny::is.reactive(current_file_input))
  stopifnot(shiny::is.reactive(sheet))
  stopifnot(shiny::is.reactive(file))

  ns <- shiny::NS(id)

  shiny::moduleServer(id, function(input, output, session) {

    shinyjs::hide(id = "range_select_card")
    #shinyjs::hide(id = "info_msg")

    getRngTxt <- function(sc = 1, sr = 1, ec = 1, er = 1) {
      paste0(LETTERS[sc], sr, ":", LETTERS[ec], er)
    } # getRngTxt(tab_param$start_col, tab_param$start_row, tab_param$end_col, tab_param$end_row)

    fmt_idx <- reactive({
      ifelse(excelformat() == "Stability", sheet(), file())
    })

    shiny::observeEvent(current_file_input(), {
      shinyjs::toggle(id = "range_select_card", condition = !is.null(current_file_input()))
    }, ignoreNULL = FALSE)

    tab <- shiny::reactive({
      shiny::req(current_file_input(), sheet(), file(), excelformat())
      xl_fmt <- excelformat()
      # use different modes of fnc_read_xlsx to import data depending on file type
      e_msg(paste("load ", nrow(current_file_input()), " files"))
      if (xl_fmt == "Certification") {
        l <- lapply(current_file_input()$datapath, function(x) {
          fnc_read_xlsx(filepath = x, sheet = sheet(), method = "tidyxl")
        })
        shiny::validate(
          shiny::need(all(!sapply(l, is.null)), "uploaded Excel files contain an empty one"),
          shiny::need(length(l) >= 2, "less than 2 laboratory files uploaded. Upload more!")
        )
        # check if all tables have the same dimensions
        test <- length(unique(sapply(l, nrow))) == 1 && length(unique(sapply(l, ncol))) == 1
        if (!test) {
          warning("m_xlsx_range_select_Server: Certification Excel Files contain different dimensions.")
          err_hint <- c(which(sapply(l, nrow) != stats::median(sapply(l, nrow))), which(sapply(l, ncol) != stats::median(sapply(l, ncol))))
          err_hint <- ifelse(length(err_hint) >= 1, paste("You might want to check file(s):", paste(current_file_input()$name[err_hint], collapse = ", ")), "")
          shiny::showNotification(
            ui = shiny::tagList(
              shiny::h3("Certification Excel Files contain different dimensions."),
              shiny::p(err_hint)
            ),
            duration = NULL,
            closeButton = TRUE,
            type = "warning"
          )
        }
      } else if (xl_fmt == "Stability") {
        # for Stability, all sheets are loaded in Background
        l <- lapply(1:length(xlsxSheetNames(current_file_input()$datapath)), function(x) {
          fnc_read_xlsx(filepath = current_file_input()$datapath[1], sheet = x, method = "openxlsx")
        })
      } else if (xl_fmt == "Homogeneity") {
        l <- list(fnc_read_xlsx(filepath = current_file_input()$datapath[1], sheet = sheet(), method = "openxlsx"))
      }
      return(l)
    })

    tab_param <- shiny::reactiveValues("tab" = NULL, "start_row" = 1, "end_row" = 1, "start_col" = 1, "end_col" = 1, "tab_flt" = matrix(1), "rng" = "A1:A1")

    # event: upload of excel file(s)
    shiny::observeEvent(tab(), {
      e_msg("m_xlsx_range_select_Server: observeEvent(tab): table uploaded; set initial crop parameters")
      tab_param$tab <- tab()
      tab_param$tab_upload <- shiny::isolate(tab()) # unchanged table from upload (for checking if row and column was selected)
      tab_param$start_row <- 1
      tab_param$start_col <- 1
      tab_param$end_row <- nrow(tab()[[fmt_idx()]])
      tab_param$end_col <- ncol(tab()[[fmt_idx()]])
      # as user response in UI
      tab_param$rng <- getRngTxt(tab_param$start_col, tab_param$start_row, tab_param$end_col, tab_param$end_row)
    })

    # table Proxy to ensure that only 2 cells are selected at any time
    uitab_proxy <- DT::dataTableProxy("uitab")
    output$uitab <- DT::renderDT(
      {
        shiny::req(tab())
        out <- tab()[[fmt_idx()]]
        if (prod(dim(out)) > 1) {
          # limit preview to 10 characters per cell
          # JL: keep the [] to keep the dimensions even for a single row entry
          out[] <- apply(out, 2, substr, start = 1, stop = 10)
        }
        dt <- DT::datatable(
          data = out,
          extensions = 'AutoFill',
          options = list("dom" = "t", autoFill = TRUE, "pageLength" = -1, ordering = FALSE),
          callback = DT::JS(readLines(get_local_file("dt-callback-color-range.js"))),
          selection = "none"
        )
        return(dt)
      }, server = FALSE
    )

    shiny::observeEvent(input$uitab_range_selected, {
      e_msg("m_xlsx_range_select_Server: observeEvent(input$uitab_range_selected)")
      cs <- input$uitab_range_selected
      if (nrow(cs) >= 2) {
        check_cs <- function(x, exc_fmt = "Certification") {
          min_rows <- switch(exc_fmt, "Certification" = 0, 1)
          min_cols <- switch(exc_fmt, "Certification" = 2, 1)
          diff(range(x[, 1])) >= min_rows &&
            diff(range(x[, 2])) >= min_cols &&
            any(tab_param$start_col != min(x[, 2]), tab_param$end_col != max(x[, 2]), tab_param$start_row != min(cs[, 1]), tab_param$end_row != max(cs[, 1]))
        }
        check_new_point <- function(x) {
          x[3, 1] >= min(x[-3, 1]) & x[3, 1] <= max(x[-3, 1]) & x[3, 2] >= min(x[-3, 2]) & x[3, 2] <= max(x[-3, 2])
        }
        update_cs <- function() {
          tab_param$start_col <- min(cs[, 2])
          tab_param$end_col <- max(cs[, 2])
          tab_param$start_row <- min(cs[, 1])
          tab_param$end_row <- max(cs[, 1])
          if (is.list(tab())) {
            tab_param$tab <- lapply(tab(), function(x) {
              x[min(cs[, 1]):max(cs[, 1]), min(cs[, 2]):max(cs[, 2]), drop = FALSE]
            })
          } else {
            tab_param$tab <- tab()[min(cs[, 1]):max(cs[, 1]), min(cs[, 2]):max(cs[, 2]), drop = FALSE]
          }
          tab_param$rng <- getRngTxt(tab_param$start_col, tab_param$start_row, tab_param$end_col, tab_param$end_row)
        }
        update_cs()
      }
    })

    output$uitxt <- shiny::renderUI({
      shiny::req(tab())
      str1 <- ifelse(is.null(current_file_input()), "", paste0("Preview of file <strong>'", current_file_input()$name[file()], "'</strong>"))
      str2 <- ifelse(excelformat() == "Stability", "No modification possible of", "You may select a range dragging the blue handle by mouse to alter the")
      str3 <- paste0("currently selected range: <strong>", tab_param$rng, "</strong>")
      shiny::HTML(str1, str2, str3)
    })

    return(tab_param)
  })
}
