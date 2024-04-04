#' @title m_materialtabelle.
#'
#' @description
#' \code{m_materialtabelle}
#'
#' @details This module will show the reactive value 'materialtabelle' from the
#'    eCerto R6 object in an editable table along with some action buttons.
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv eCerto R6 object, which includes a 'materialtabelle'.
#'
#' @return Nothing. Will update 'materialtabelle' in eCerto R6 object and trigger
#'    other modules via setting rv$cur_an.
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyjs::useShinyjs(),
#'       eCerto:::m_materialtabelleUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       rv <- eCerto:::test_rv("SR3")
#'       eCerto:::m_materialtabelleServer(id = "test", rv = rv)
#'     }
#'   )
#' }
#'
#' @importFrom shinyWidgets show_alert
#'
#' @noRd
#' @keywords internal
#'
m_materialtabelleUI <- function(id) {
  ns <- shiny::NS(id)
  wb <- "50px"
  shiny::fluidRow(
    shiny::column(
      width = 10,
      shiny::strong(shiny::actionLink(inputId = ns("tabC3head"), label = "Tab.C3 - Certified values within material")),
      DT::DTOutput(ns("matreport"))
    ),
    shiny::column(
      width = 2,
      shiny::wellPanel(
        modify_FUcols_UI(id = ns("FUcols")),
        shiny::actionButton(inputId = ns("clear_FU_cols"), label = "Remove F/U cols without effect"),
        check_stability2_UI(id = ns("post_cert_stab"))
      )
    )
  )
}

#' @noRd
#' @keywords internal
m_materialtabelleServer <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    silent <- get_golem_config("silent")
    ns <- shiny::NS(id)

    modify_FUcols_Server(id = "FUcols", mt = mater_table)

    # use err_txt to provide error messages to the user
    err_txt <- shiny::reactiveVal(NULL)
    shiny::observeEvent(err_txt(),
      {
        shinyWidgets::show_alert(title = NULL, text = err_txt(), type = "info")
        err_txt(NULL)
      },
      ignoreNULL = TRUE
    )

    a <- shiny::reactive({
      req(rv$a_p("name"))
      shiny::validate(shiny::need(expr = rv$cur_an %in% rv$a_p("name"), message = paste("Analyte", rv$cur_an, "is not present in C data.")))
      rv$cur_an
    })

    pooling <- shiny::reactive({
      shiny::req(a(), getValue(rv, c("General", "apm")))
      # getValue(rv, c("General","apm"))[[a()]][["pooling"]]
      rv$a_p("pooling")[a()]
    })

    # helper function to remove unused user columns
    remove_unused_cols <- function(mt = NULL) {
      # strip unused F and U columns from 'mater_table'
      cc <- attr(mt, "col_code")
      if (nrow(cc) >= 1) {
        flt <- sapply(1:nrow(cc), function(i) {
          all(mt[, cc[i, "ID"]] == 1) | all(mt[, cc[i, "ID"]] == 0)
        })
        if (any(flt)) {
          mt <- mt[, !(colnames(mt) %in% cc[flt, "ID"])]
          cc <- cc[!flt, , drop = FALSE]
          attr(mt, "col_code") <- cc
        }
      }
      return(mt)
    }

    # define table as reactiveVal to update it at different places within the module
    mater_table <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$clear_FU_cols, {
      # removal of unused columns works for legacy data but is also removing just added columns if they have a standard name (e.g. 'F1')
      mt <- getValue(rv, c("General", "materialtabelle"))
      mater_table(remove_unused_cols(mt = mt))
    })

    shiny::observeEvent(getValue(rv, c("General", "materialtabelle")), {
      # this section ensures some legacy data to work properly by applying modifications upon load if required
      mt <- getValue(rv, c("General", "materialtabelle"))
      # rename previous "char" column to "u_char" for legacy reasons
      if (any(c("char", "com") %in% colnames(mt))) {
        if ("char" %in% colnames(mt)) colnames(mt)[colnames(mt) == "char"] <- "u_char"
        if ("com" %in% colnames(mt)) colnames(mt)[colnames(mt) == "com"] <- "u_com"
        # notify user
        shinyWidgets::show_alert(title = NULL, text = "Columns 'char' and 'com' in the material table have been renamed to 'u_char' and 'u_com'.", type = "info")
      }
      # add a column for absolute uncertainty if not yet present
      if (!("U_abs" %in% colnames(mt))) {
        cc <- attr(mt, "col_code")
        mt <- cbind(mt, "U_abs" = NA)
        attr(mt, "col_code") <- cc
      }
      # add a column for analyte unit if not yet present or modify column according to rv object data (stored in apm)
      if (!("unit" %in% colnames(mt)) | ("unit" %in% colnames(mt) && all(mt[, "unit"] == "U"))) {
        cc <- attr(mt, "col_code")
        units <- rv$a_p("unit")
        if (is.list(units)) {
          # [ToDo $$JL$$ Fix that old RData don't contain column "unit" in mt]
          tmp <- getValue(rv, c("General", "apm"))
          if (!"unit" %in% names(tmp[[1]])) {
            tmp <- lapply(tmp, function(x) {
              c(x, "unit" = "U")
            })
            setValue(rv, c("General", "apm"), tmp)
          }
        }
        if (identical(names(units), as.character(mt[, "analyte"]))) {
          if (!silent) message("[materialtabelle] Set analyte units for 'mt' from 'rv C data'")
          mt[, "unit"] <- units
        } else {
          err_txt("[materialtabelle] Can't set analyte units for Tab.3 - Material table")
          mt[, "unit"] <- rep("U", nrow(mt))
        }
        attr(mt, "col_code") <- cc
      }
      # check if the option to remove F/U columns without effect should be displayed
      if (!identical(mt, remove_unused_cols(mt = mt))) {
        shinyjs::showElement(id = "clear_FU_cols")
      } else {
        shinyjs::hideElement(id = "clear_FU_cols")
      }
      # store result back if modifications were performed
      if (!identical(mater_table(), mt)) {
        if (!silent) message("[materialtabelle] set local 'mt' from 'rv'")
        mater_table(mt)
      }
    })

    # helper function to update calculations
    recalc_mat_table <- function(mt = NULL) {
      if (any(is.finite(mt[, "mean"])) & any(is.finite(mt[, "sd"]))) {
        if (!silent) message("[materialtabelle] recalculate table")

        # recalculate all cert_mean values including correction factors
        mt[, "cert_val"] <- apply(mt[, get_UF_cols(mt, "F"), drop = FALSE], 1, prod, na.rm = T)
        update_reactivecell(r = mater_table, colname = "cert_val", value = mt[, "cert_val"])

        # update the 'char'acteristic uncertainty
        mt[, "u_char"] <- mt[, "sd"] / (sqrt(mt[, "n"]) * mt[, "mean"])
        update_reactivecell(r = mater_table, colname = "u_char", value = mt[, "u_char"])

        # update the 'com'bined uncertainty
        mt[, "u_com"] <- apply(mt[, get_UF_cols(mt, "U"), drop = FALSE], 1, function(x) {
          sqrt(sum(x^2, na.rm = T))
        })
        update_reactivecell(r = mater_table, colname = "u_com", value = mt[, "u_com"])

        # update the overall uncertainty
        mt[, "U"] <- mt[, "k"] * mt[, "u_com"]
        update_reactivecell(r = mater_table, colname = "U", value = mt[, "U"])

        # update the absolute uncertainty
        mt[, "U_abs"] <- mt[, "U"] * mt[, "cert_val"]
        update_reactivecell(r = mater_table, colname = "U_abs", value = mt[, "U_abs"])
      }
      invisible(mt)
    }

    # data frame of selected analyte
    c_fltData <- shiny::reactive({
      shiny::req(getValue(rv, c("Certification", "data")), getValue(rv, c("General", "apm")), a())
      rv$c_fltData(recalc = TRUE)
    })

    # number of items (either labs or measurements)
    n <- shiny::reactive({
      shiny::req(c_fltData())
      x <- c_fltData()
      return(ifelse(
        test = pooling(),
        yes = sum(!x[, "L_flt"]),
        no = length(unique(as.character(x[!x[, "L_flt"], "Lab"])))
      ))
    })

    # calculate cert_mean
    cert_mean <- shiny::reactive({
      shiny::req(c_fltData())
      if (!silent) message("[materialtabelle] recalc cert_mean")
      data <- c_fltData()[!c_fltData()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs
      # from calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      ifelse(
        pooling(),
        mean(data[, "value"], na.rm = T),
        mean(sapply(split(data[, "value"], data[, "Lab"]), mean, na.rm = T))
      )
    })
    shiny::observeEvent(cert_mean(), {
      setValue(rv, c("Certification_processing", "cert_mean"), cert_mean())
    })

    # calculate cert_sd
    cert_sd <- shiny::reactive({
      shiny::req(c_fltData())
      if (!silent) message("[materialtabelle] recalc cert_sd")
      data <- c_fltData()[!c_fltData()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs
      # from calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      # build either standard deviation of all values or standard deviation of
      # average per lab
      ifelse(
        pooling(),
        stats::sd(data[, "value"], na.rm = T),
        stats::sd(sapply(split(data[, "value"], data[, "Lab"]), mean, na.rm = T))
      )
    })
    shiny::observeEvent(cert_sd(), {
      setValue(rv, c("Certification_processing", "cert_sd"), cert_sd())
    })

    # update mt
    shiny::observeEvent(mater_table(), {
      # set result as new value in the R6 object
      mt <- recalc_mat_table(mt = mater_table())
      if (!identical(getValue(rv, c("General", "materialtabelle")), mt)) {
        setValue(rv, c("General", "materialtabelle"), mt)
      }
    })

    # when an Analyte-tab was selected --> update materialtabelle
    # TODO Check that analyte-column is unique
    # in case mater table has been initiated...
    # shiny::observeEvent(c_fltData(),{
    shiny::observe({
      shiny::req(c_fltData())
      an <- as.character(c_fltData()[1, "analyte"])
      if (!is.null(mater_table()) && an %in% mater_table()[, "analyte"]) {
        if (!silent) message("[materialtabelle] update initiated for ", an)
        update_reactivecell(r = mater_table, colname = "mean", analyterow = an, value = cert_mean())
        update_reactivecell(r = mater_table, colname = "sd", analyterow = an, value = cert_sd())
        update_reactivecell(r = mater_table, colname = "n", analyterow = an, value = n())
      }
    })

    # monitor table editing and update if necessary
    mater_table_print <- shiny::eventReactive(mater_table(), {
      mt <- mater_table()
      # set rows with non-confirmed analytes to NA
      non_conf <- is.na(mt[, "mean"])
      if (any(non_conf)) {
        for (i in which(non_conf)) {
          for (j in 2:ncol(mt)) {
            mt[i, j] <- NA
          }
        }
      }
      # rename column header for temporary display
      cc <- attr(mt, "col_code")
      if (nrow(cc) >= 1) {
        for (k in 1:nrow(cc)) {
          colnames(mt)[colnames(mt) == cc[k, "ID"]] <- cc[k, "Name"]
        }
      }
      if (!silent) message("[materialtabelle] Check if analytes are confirmed and rename F and U cols from mater_table()")
      return(mt)
    })

    # the rendered, editable mat_table as seen by user
    selected_row_idx <- shiny::reactiveValues("row" = 1, "redraw" = 0)
    output$matreport <- DT::renderDT(
      {
        selected_row_idx$redraw
        dt <- mater_table_print()
        styleTabC3(x = dt, apm = getValue(rv, c("General", "apm")), selected_row = selected_row_idx$row)
      },
      server = TRUE
    )

    observeEvent(rv$cur_an,
      {
        req(mater_table_print(), selected_row_idx$row)
        if (!identical(rv$cur_an, mater_table_print()[selected_row_idx$row, "analyte"])) {
          i <- which(as.character(mater_table_print()[, "analyte"]) == rv$cur_an)
          if (length(i) == 1) selected_row_idx$row <- i
        }
      },
      ignoreNULL = TRUE
    )

    shiny::observeEvent(input$matreport_rows_selected,
      {
        shiny::req(mater_table())
        i <- input$matreport_rows_selected
        if (is.null(i)) {
          message("[materialtabelle] input$matreport_rows_selected - [ToDo] implement automatic (re)selection of rows")
          # $$ToDo$$ user deselected row --> reselect previous
          # use proxy <- DT::dataTableProxy('tab') and than
          # DT::selectRows(proxy, selected=selected_row_idx$row

          # once the user starts cell edit the rows_selected property is changed to NULL
          # unfortunately which can not be differentiated from an accidental deselection of the
          # active row. The below solution to redraw tabC3 upon deselection therefore
          # prevents editing :(
          # selected_row_idx$redraw <- selected_row_idx$redraw+1
        } else {
          an <- as.character(mater_table()[i, "analyte"])
          if (!getValue(rv, c("General", "apm"))[[an]][["confirmed"]]) {
            # mark analyte as confirmed
            message("[materialtabelle] setting ", an, " as confirmed")
            tmp <- getValue(rv, c("General", "apm"))
            tmp[[an]][["confirmed"]] <- TRUE
            setValue(rv, c("General", "apm"), tmp)
          }
          if (i != selected_row_idx$row) {
            # update index
            message("[materialtabelle] input$matreport_rows_selected - setting selected_row_idx")
            selected_row_idx$row <- i
          } else {
            if (is.null(rv$cur_an) || rv$cur_an != an) {
              message("[materialtabelle] setting rv$cur_an")
              # set current analyte in rv to trigger calculation of lab_means, c_mean, c_sd etc.
              rv$cur_an <- an
            }
          }
        }
      },
      ignoreNULL = FALSE
    )

    # ensure update of mater_table() on user input
    shiny::observeEvent(input$matreport_cell_edit, {
      if (!silent) message("[materialtabelle] user edited table cell")
      # convert value to numeric
      x <- as.numeric(gsub("[^[:digit:].]", "", input$matreport_cell_edit$value))

      # replace in correct position
      mt <- mater_table()
      mt[input$matreport_cell_edit$row, input$matreport_cell_edit$col + 1] <- x

      # update 'mater_table'
      mater_table(mt)
    })

    check_stability2_Server(id = "post_cert_stab", rv = rv)

    # Help section -------------------------------------------------------------
    shiny::observeEvent(input$tabC3head, {
      show_help("certification_materialtabelle")
    })
  })
}
