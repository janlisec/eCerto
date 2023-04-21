#'@title m_materialtabelle.
#'
#'@description
#'\code{m_materialtabelle}
#'
#'@details This module will show the reactive value 'materialtabelle' from the
#'    eCerto R6 object in an editable table along with some action buttons.
#'
#'@param id Name when called as a module in a shiny app.
#'@param rv eCerto R6 object, which includes a 'materialtabelle'.
#'
#'@return Nothing. Will update 'materialtabelle' in eCerto R6 object and trigger
#'    other modules via setting rv$cur_an.
#'
#'@examples
#'if (interactive()) {
#'shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    shinyjs::useShinyjs(),
#'    eCerto:::m_materialtabelleUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'  rv <- eCerto:::test_rv("SR3")
#'  eCerto:::m_materialtabelleServer(id = "test", rv = rv)
#'  }
#')
#'}
#'
#'@importFrom shinyalert shinyalert
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
      DT::DTOutput(shiny::NS(id, "matreport"))
    ),
    shiny::column(
      width = 2,
      shiny::wellPanel(
        shiny::fluidRow(
          shiny::div(
            style="width=100%; margin-bottom: 5px; margin-left: 15px;",
            shiny::strong(shiny::actionLink(inputId = ns("tabC3opt"), label = "Modify Tab.C3"))
          ),
          shiny::p(
            style = "margin-left: 15px;",
            shiny::actionButton(inputId = ns("c_addF"), label = "Add", width = wb),
            shiny::actionButton(inputId = ns("c_remF"), label = "Del", width = wb),
            shiny::actionButton(inputId = ns("c_renF"), label = "Ren", width = wb),
            shiny::strong("F-cols")
          ),
          shiny::p(
            style = "margin-left: 15px;",
            shiny::actionButton(inputId = ns("c_addU"), label = "Add", width = wb),
            shiny::actionButton(inputId = ns("c_remU"), label = "Del", width = wb),
            shiny::actionButton(inputId = ns("c_renU"), label = "Ren", width = wb),
            shiny::strong("U cols")
          ),
          shiny::div(
            style="margin-top: 15px; margin-left: 15px; margin-right: 15px;",
            shiny::actionButton(inputId = ns("clear_FU_cols"), label = "Remove F/U cols without effect", width = "100%")
          ),
          shiny::p(),
          shiny::div(
            style="margin-top: 15px; margin-left: 15px; margin-right: 15px;",
            check_stability_UI(id = ns("post_cert_stab"))
          )
        )
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

    # use err_txt to provide error messages to the user
    err_txt <- shiny::reactiveVal(NULL)
    shiny::observeEvent(err_txt(), {
      shinyalert::shinyalert(text = err_txt(), type = "info")
      err_txt(NULL)
    }, ignoreNULL = TRUE)

    pooling <- shiny::reactive({
      shiny::req(rv$cur_an)
      getValue(rv, c("General","apm"))[[rv$cur_an]][["pooling"]]
    })

    # helper function to remove unused user columns
    remove_unused_cols <- function(mt=NULL) {
      # strip unused F and U columns from 'mater_table'
      cc <- attr(mt, "col_code")
      #browser()
      if (nrow(cc)>=1) {
        flt <- sapply(1:nrow(cc), function(i) {
          all(mt[, cc[i, "ID"]] == 1) | all(mt[, cc[i, "ID"]] == 0)
        })
        if (any(flt)) {
          mt <- mt[,!(colnames(mt) %in% cc[flt,"ID"])]
          cc <- cc[!flt,,drop=FALSE]
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
      mater_table(remove_unused_cols(mt=mt))
    })

    shiny::observeEvent(getValue(rv, c("General", "materialtabelle")), {
      # this section ensures some legacy data to work properly by applying modifications upon load if required
      mt <- getValue(rv, c("General", "materialtabelle"))
      # rename previous "char" column to "u_char" for legacy reasons
      if (any(c("char","com") %in% colnames(mt))) {
        if ("char" %in% colnames(mt)) colnames(mt)[colnames(mt)=="char"] <- "u_char"
        if ("com" %in% colnames(mt)) colnames(mt)[colnames(mt)=="com"] <- "u_com"
        # notify user
        shinyalert::shinyalert(text = "Columns 'char' and 'com' in the material table have been renamed to 'u_char' and 'u_com'.", type = "info")
      }
      # add a column for absolute uncertainty if not yet present
      if (!("U_abs" %in% colnames(mt))) {
        cc <- attr(mt, "col_code")
        mt <- cbind(mt, "U_abs"=NA)
        attr(mt, "col_code") <- cc
      }
      # add a column for analyte unit if not yet present or modify column according to rv object data (stored in apm)
      if (!("unit" %in% colnames(mt)) | ("unit" %in% colnames(mt) && all(mt[,"unit"]=="U"))) {
        cc <- attr(mt, "col_code")
        units <- rv$a_p("unit")
        if (identical(names(units), as.character(mt[,"analyte"]))) {
          if (!silent) message("[materialtabelle] Set analyte units for 'mt' from 'rv C data'")
          mt[,"unit"] <- units
        } else {
          err_txt("[materialtabelle] Can't set analyte units for Tab.3 - Material table")
          mt[,"unit"] <- rep("U", nrow(mt))
        }
        attr(mt, "col_code") <- cc
      }
      # check if the option to remove F/U columns without effect should be displayed
      if (!identical(mt, remove_unused_cols(mt=mt))) {
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
    recalc_mat_table <- function(mt=NULL) {
      if (any(is.finite(mt[,"mean"])) & any(is.finite(mt[,"sd"]))) {
        if (!silent) message("[materialtabelle] recalculate table")

        # recalculate all cert_mean values including correction factors
        mt[,"cert_val"] <- apply(mt[,get_UF_cols(mt, "F"),drop=FALSE], 1, prod, na.rm = T)
        update_reactivecell(r = mater_table, colname = "cert_val", value = mt[,"cert_val"])

        # update the 'char'acteristic uncertainty
        mt[,"u_char"] <- mt[, "sd"] / (sqrt(mt[, "n"]) * mt[, "mean"])
        update_reactivecell(r = mater_table, colname = "u_char", value = mt[,"u_char"])

        # update the 'com'bined uncertainty
        mt[,"u_com"] <- apply(mt[,get_UF_cols(mt, "U"),drop=FALSE], 1, function(x) { sqrt(sum(x ^ 2, na.rm = T)) })
        update_reactivecell(r = mater_table, colname = "u_com", value = mt[,"u_com"])

        # update the overall uncertainty
        mt[,"U"] <- mt[, "k"] * mt[, "u_com"]
        update_reactivecell(r = mater_table, colname = "U", value = mt[,"U"])

        # update the absolute uncertainty
        mt[,"U_abs"] <- mt[, "U"] * mt[, "cert_val"]
        update_reactivecell(r = mater_table, colname = "U_abs", value = mt[,"U_abs"])
      }
      invisible(mt)
    }

    shiny::observeEvent(input$c_addF, {
      cc <- attr(mater_table(), "col_code")
      # get smallest index number available
      n <- min(which(!(1:9 %in% as.numeric(substr(cc[substr(cc[,"ID"],1,1)=="F","ID"],2,2)))))
      shinyalert::shinyalert(
        html = TRUE, text = shiny::tagList(shiny::textInput(inputId = session$ns("tmp"), label = "Type name to add", value = paste0("F",n))),
        cancelButtonText = "Cancel", confirmButtonText = "Add", showCancelButton = TRUE, size = "xs",
        callbackR = function(value) {
          if (value) {
            mt <- mater_table()
            if (input$tmp %in% c(colnames(mt), cc[,"Name"])) {
              err_txt("Sorry, I can't add this column. Please specify a unique column name.")
            } else {
              cc <- rbind(cc, data.frame("ID"=paste0("F",n), "Name"=input$tmp))
              nc <- matrix(rep(1,nrow(mt)), ncol=1, dimnames=list(rownames(mt), paste0("F",n)))
              cp <- which(colnames(mt)=="cert_val")
              mt <- cbind(mt[,1:(cp-1)], nc, mt[,cp:ncol(mt)])
              attr(mt, "col_code") <- cc
              mater_table(mt)
            }
          }
        }
      )
    })
    # remove a correction factor column
    shiny::observeEvent(input$c_remF, {
      cc <- attr(mater_table(), "col_code")
      if (any(substr(cc[,"ID"],1,1)=="F")) {
        choices <- cc[substr(cc[,"ID"],1,1)=="F","Name"]
        shinyalert::shinyalert(
          html = TRUE, text = shiny::tagList(shiny::selectInput(inputId = session$ns("tmp"), label = "Select to remove", choices = choices)),
          cancelButtonText = "Cancel", confirmButtonText = "Rem", showCancelButton = TRUE, size = "s",
          callbackR = function(value) {
            if (value) {
              mt <- mater_table()
              mt <- mt[,!(colnames(mt)==cc[input$tmp==cc[,"Name"],"ID"])]
              cc <- cc[!(substr(cc[,"ID"],1,1)=="F" & cc[,"Name"]==input$tmp),,drop=FALSE]
              attr(mt, "col_code") <- cc
              mater_table(mt)
            }
          }
        )
      } else {
        shinyalert::shinyalert(text = "No Correction Factor defined. You have to add one before you can remove it.", type = "info")
      }
    })
    # rename a correction factor column
    shiny::observeEvent(input$c_renF, {
      cc <- attr(mater_table(), "col_code")
      if (any(substr(cc[,"ID"],1,1)=="F")) {
        choices <- cc[substr(cc[,"ID"],1,1)=="F","Name"]
        shinyalert::shinyalert(
          html = TRUE, text = shiny::tagList(
            shiny::selectInput(inputId = session$ns("tmp"), label = "Select F column", choices = choices),
            shiny::textInput(inputId = session$ns("tmp2"), label = "New Column Name")
          ),
          cancelButtonText = "Cancel", confirmButtonText = "Rename", showCancelButton = TRUE, size = "s",
          callbackR = function(value) {
            if (value) {
              mt <- mater_table()
              cc[substr(cc[,"ID"],1,1)=="F" & cc[,"Name"]==input$tmp,"Name"] <- input$tmp2
              attr(mt, "col_code") <- cc
              mater_table(mt)
            }
          }
        )
      } else {
        shinyalert::shinyalert(text = "No Correction Factor defined. You have to add one before you can rename it.", type = "info")
      }
    })
    # add a uncertainty factor column
    shiny::observeEvent(input$c_addU, {
      cc <- attr(mater_table(), "col_code")
      # get smallest index number available
      n <- min(which(!(1:9 %in% as.numeric(substr(cc[substr(cc[,"ID"],1,1)=="U","ID"],2,2)))))
      shinyalert::shinyalert(
        html = TRUE, text = shiny::tagList(shiny::textInput(inputId = session$ns("tmp"), label = "Type name to add", value = paste0("u_",n))),
        cancelButtonText = "Cancel", confirmButtonText = "Add", showCancelButton = TRUE, size = "s",
        callbackR = function(value) {
          if (value) {
            mt <- mater_table()
            if (input$tmp %in% c(colnames(mt), cc[,"Name"])) {
              err_txt("Sorry, I can't add this column. Please specify a unique column name.")
            } else {
              cc <- rbind(cc, data.frame("ID"=paste0("U",n), "Name"=input$tmp))
              nc <- matrix(rep(0,nrow(mt)), ncol=1, dimnames=list(rownames(mt), paste0("U",n))) # new data column
              cp <- which(colnames(mt)=="u_com") # column position where to include the new data
              mt <- cbind(mt[,1:(cp-1)], nc, mt[,cp:ncol(mt)])
              attr(mt, "col_code") <- cc
              mater_table(mt)
            }
          }
        }
      )
    })
    # remove a uncertainty factor column
    shiny::observeEvent(input$c_remU, {
      if (!silent) message("materialtabelle: remove a uncertainty factor column")
      cc <- attr(mater_table(), "col_code")
      if (any(substr(cc[,"ID"],1,1)=="U")) {
        choices <- cc[substr(cc[,"ID"],1,1)=="U","Name"]
        shinyalert::shinyalert(
          html = TRUE, text = shiny::tagList(shiny::selectInput(inputId = session$ns("tmp"), label = "Select to remove", choices = choices)),
          cancelButtonText = "Cancel", confirmButtonText = "Rem", showCancelButton = TRUE, size = "s",
          callbackR = function(value) {
            if (value) {
              mt <- mater_table()
              mt <- mt[,!(colnames(mt)==cc[input$tmp==cc[,"Name"],"ID"])]
              cc <- cc[!(substr(cc[,"ID"],1,1)=="U" & cc[,"Name"]==input$tmp),,drop=FALSE]
              attr(mt, "col_code") <- cc
              mater_table(mt)
            }
          }
        )
      } else {
        shinyalert::shinyalert(text = "No Uncertainty Term defined. You have to add one before you can remove it.", type = "info")
      }
    })
    # rename a uncertainty factor column
    shiny::observeEvent(input$c_renU, {
      cc <- attr(mater_table(), "col_code")
      if (any(substr(cc[,"ID"],1,1)=="U")) {
        choices <- cc[substr(cc[,"ID"],1,1)=="U","Name"]
        shinyalert::shinyalert(
          html = TRUE, text = shiny::tagList(
            shiny::selectInput(inputId = session$ns("tmp"), label = "Select U column", choices = choices),
            shiny::textInput(inputId = session$ns("tmp2"), label = "New Column Name")
          ),
          cancelButtonText = "Cancel", confirmButtonText = "Rename", showCancelButton = TRUE, size = "s",
          callbackR = function(value) {
            if (value) {
              mt <- mater_table()
              cc[substr(cc[,"ID"],1,1)=="U" & cc[,"Name"]==input$tmp,"Name"] <- input$tmp2
              attr(mt, "col_code") <- cc
              mater_table(mt)
            }
          }
        )
      } else {
        shinyalert::shinyalert(text = "No Uncertainty Factor defined. You have to add one before you can rename it.", type = "info")
      }
    })

    # data frame of selected analyte
    c_fltData <- shiny::reactive({
      shiny::req(getValue(rv,c("Certification","data")), getValue(rv,c("General","apm")), rv$cur_an)
      rv$c_fltData(recalc=TRUE)
    })

    # number of items (either labs or measurements)
    n <- shiny::reactive({
      shiny::req(c_fltData())
      x <- c_fltData()
      return(ifelse(
        test = pooling(),
        yes = sum(!x[,"L_flt"]),
        no = length(unique(as.character(x[!x[,"L_flt"],"Lab"])))
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
    shiny::observeEvent(cert_mean(),{
      setValue(rv, c("Certification_processing","cert_mean"), cert_mean())
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
    shiny::observeEvent(cert_sd(),{
      setValue(rv, c("Certification_processing","cert_sd"), cert_sd())
    })

    # update mt
    shiny::observeEvent(mater_table(),{
      # set result as new value in the R6 object
      mt <- recalc_mat_table(mt=mater_table())
      if (!identical(getValue(rv, c("General","materialtabelle")), mt)) {
        setValue(rv, c("General","materialtabelle"), mt)
      }
    })

    # when an Analyte-tab was selected --> update materialtabelle
    # TODO Check that analyte-column is unique
    # in case mater table has been initiated...
    # shiny::observeEvent(c_fltData(),{
    shiny::observe({
      shiny::req(c_fltData())
      if(!is.null(mater_table())) {
        an <- c_fltData()[1,"analyte"]
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
      non_conf <- is.na(mt[,"mean"])
      if (any(non_conf)) {
        for (i in which(non_conf)) {
          for (j in 2:ncol(mt)) {
            mt[i,j] <- NA
          }
        }
      }
      # rename column header for temporary display
      cc <- attr(mt, "col_code")
      if (nrow(cc)>=1) {
        for (k in 1:nrow(cc)) {
          colnames(mt)[colnames(mt) == cc[k, "ID"]] <- cc[k, "Name"]
        }
      }
      if (!silent) message("[materialtabelle] Check if analytes are confirmed and rename F and U cols from mater_table()")
      return(mt)
    })

    # the rendered, editable mat_table as seen by user
    selected_row_idx <- shiny::reactiveValues("row"=1, "redraw"=0)
    output$matreport <- DT::renderDT({
      selected_row_idx$redraw
      dt <- mater_table_print()
      styleTabC3(x = dt, apm = getValue(rv, c("General","apm")), selected_row = selected_row_idx$row)
    }, server = TRUE)

    shiny::observeEvent(input$matreport_rows_selected, {
      shiny::req(mater_table())
      i <- input$matreport_rows_selected
      if (is.null(i)) {
        message("input$matreport_rows_selected - [ToDo] implement automatic (re)selection of rows")
        # $$ToDo$$ user deselected row --> reselect previous
        # use proxy <- DT::dataTableProxy('tab') and than
        # DT::selectRows(proxy, selected=selected_row_idx$row

        # once the user starts cell edit the rows_selected property is changed to NULL
        # unfortunately which can not be differentiated from an accidental deselection of the
        # active row. The below solution to redraw tabC3 upon deselection therefore
        # prevents editing :(
        #selected_row_idx$redraw <- selected_row_idx$redraw+1
      } else {
        an <- as.character(mater_table()[i,"analyte"])
        if (!getValue(rv, c("General", "apm"))[[an]][["confirmed"]]) {
          # mark analyte as confirmed
          tmp <- getValue(rv, c("General", "apm"))
          tmp[[an]][["confirmed"]] <- TRUE
          setValue(rv, c("General", "apm"), tmp)
        }
        if (i!=selected_row_idx$row) {
          # update index
          message("input$matreport_rows_selected - setting selected_row_idx")
          selected_row_idx$row <- i
        } else {
          if (is.null(rv$cur_an) || rv$cur_an != an) {
            message("[materialtabelle] setting rv$cur_an")
            # set current analyte in rv to trigger calculation of lab_means, c_mean, c_sd etc.
            rv$cur_an <- an
          }
        }
      }
    }, ignoreNULL = FALSE)

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

    check_stability_Server(id = "post_cert_stab", rv = rv)

    shiny::observeEvent(input$tabC3head, {
      show_help("certification_materialtabelle")
    })

    shiny::observeEvent(input$tabC3opt, {
      show_help("certification_materialtabelle_opt")
    })

  })
}
