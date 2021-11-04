#' @name mod_materialtabelle
#' @aliases m_materialtabelleUI
#' @aliases m_materialtabelleServer
#'
#' @title Modul "Materialtabelle"
#'
#'@description
#'\code{m_materialtabelle}
#'
#'@details
#'not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv The whole R6 object.
#'
#'@return The analyte name of the currently selected row of  mat_tab.
#'
#'@examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(shinyalert::useShinyalert(),
#'    m_materialtabelleUI(id = "test")),
#'  server = function(input, output, session) {
#'  rv <- reactiveClass$new(init_rv())
#'  shiny::isolate({
#'  eCerto::setValue(rv, c("Certification","data"),
#'    eCerto:::test_certification()[["data"]])
#'  eCerto::setValue(rv, c("General","apm"),
#'    eCerto::init_apm(eCerto:::test_certification()[["data"]]))
#'  an <- names(eCerto::getValue(rv, c("General","apm")))
#'  eCerto::setValue(rv, c("General","materialtabelle"),
#'    eCerto::init_materialTabelle(an))
#'  })
#'  out <- m_materialtabelleServer(id = "test", rv=rv)
#'  observeEvent(out(), {print(out())})
#'  }
#' )
#' }
#'
#' @rdname mod_materialtabelle
#' @export
#'
m_materialtabelleUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 10,
      shiny::strong(shiny::actionLink(inputId = ns("materheadline"), label = "Tab.3 Material Certification")),
      DT::DTOutput(shiny::NS(id, "matreport"))
    ),
    shiny::column(
      width = 2,
      shiny::wellPanel(
        shiny::fluidRow(
          shiny::column(
            width = 6, align = "center", "F",
            shiny::actionButton(inputId = ns("c_addF"), label = "Add", width = "110%"),
            shiny::actionButton(inputId = ns("c_remF"), label = "Remove", width = "110%")
          ),
          shiny::column(
            width = 6, align = "center", "U",
            shiny::actionButton(inputId = ns("c_addU"), label = "Add", width = "110%"),
            shiny::actionButton(inputId = ns("c_remU"), label = "Remove", width = "110%")
          ),
        )
      )
    )
  )
}

#' @rdname mod_materialtabelle
#' @export
m_materialtabelleServer <- function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    silent = FALSE # messages

    out <- shiny::reactiveVal(NULL)

    pooling <- shiny::reactive({
      shiny::req(out())
      getValue(rv, c("General","apm"))[[out()]][["pooling"]]
    })

    precision_export <- shiny::reactive({
      shiny::req(out())
      getValue(rv, c("General","apm"))[[out()]][["precision_export"]]
    })

    # define table as reactiveVal to update it at different places within the module
    mater_table <- shiny::reactiveVal(NULL)

    shiny::observeEvent(getValue(rv, c("General", "materialtabelle")), {
      #browser()
      mt <- getValue(rv, c("General", "materialtabelle"))
      #mt <- remove_unused_cols(mt=mt)
      if (!identical(mater_table(), mt)) {
        mater_table(mt)
      }
    })

    # this is a fixed value to round the uncertainty columns; 4 should be appropriate here
    precision_U <- 4

    # helper function to remove unused user columns
    remove_unused_cols <- function(mt=NULL) {
      # strip unused F and U columns from 'mater_table'
      cc <- attr(mt, "col_code")
      if (nrow(cc)>=1) {
        flt <- sapply(1:nrow(cc), function(i) {
          cc[i, "ID"] == cc[i, "Name"] &&
            # only proceed of Name and ID of the attribute are equal
            (all(mt[, cc[i, "Name"]] == 1) | all(mt[, cc[i, "Name"]] == 0))
        })
        if (any(flt)) {
          mt <- mt[,!(colnames(mt) %in% cc[flt,"Name"])]
          cc <- cc[!flt,,drop=FALSE]
          attr(mt, "col_code") <- cc
        }
      }
      return(mt)
    }

    # helper function to update calculations
    recalc_mat_table <- function(mt=NULL) {
      # message("materialtabelle: recalculate table")
      # recalculate all cert_mean values including correction factors
      f_cols <- unlist(sapply(c("mean", paste0("F", 1:9)), function(x) { grep(x, colnames(mt)) }))
      mt[,"cert_val"] <- apply(mt[,f_cols,drop=FALSE], 1, prod, na.rm = T)
      update_reactivecell(
        r = mater_table,
        colname = "cert_val",
        value = mt[,"cert_val"]
      )

      # update the 'char'acteristic uncertainty
      mt[,"char"] <- mt[, "sd"] / (sqrt(mt[, "n"]) * mt[, "mean"])
      update_reactivecell(
        r = mater_table,
        colname = "char",
        value = mt[,"char"]
      )

      # update the 'com'bined uncertainty
      u_cols <- unlist(sapply(c("char", paste0("U", 1:9)), function(x) { grep(x, colnames(mt)) }))
      mt[,"com"] <- apply(mt[,u_cols,drop=FALSE], 1, function(x) { sqrt(sum(x ^ 2, na.rm = T)) })
      update_reactivecell(
        r = mater_table,
        colname = "com",
        value = mt[,"com"]
      )

      # update the overall uncertainty
      mt[,"U"] <- mt[, "k"] * mt[, "com"]
      update_reactivecell(
        r = mater_table,
        colname = "U",
        value = mt[,"U"]
      )

      invisible(mt)
    }

    # add a correction factor column
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
            cc <- rbind(cc, data.frame("ID"=paste0("F",n), "Name"=input$tmp))
            nc <- matrix(rep(1,nrow(mt)), ncol=1, dimnames=list(rownames(mt), paste0("F",n)))
            cp <- which(colnames(mt)=="cert_val")
            mt <- cbind(mt[,1:(cp-1)], nc, mt[,cp:ncol(mt)])
            attr(mt, "col_code") <- cc
            mater_table(mt)
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
          cancelButtonText = "Cancel", confirmButtonText = "Rem", showCancelButton = TRUE, size = "xs",
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
    # add a uncertainty factor column
    shiny::observeEvent(input$c_addU, {
      cc <- attr(mater_table(), "col_code")
      # get smallest index number available
      n <- min(which(!(1:9 %in% as.numeric(substr(cc[substr(cc[,"ID"],1,1)=="U","ID"],2,2)))))
      shinyalert::shinyalert(
        html = TRUE, text = shiny::tagList(shiny::textInput(inputId = session$ns("tmp"), label = "Type name to add", value = paste0("U",n))),
        cancelButtonText = "Cancel", confirmButtonText = "Add", showCancelButton = TRUE, size = "xs",
        callbackR = function(value) {
          if (value) {
            mt <- mater_table()
            cc <- rbind(cc, data.frame("ID"=paste0("U",n), "Name"=input$tmp))
            nc <- matrix(rep(0,nrow(mt)), ncol=1, dimnames=list(rownames(mt), paste0("U",n))) # new data column
            cp <- which(colnames(mt)=="com") # column position where to include the new data
            mt <- cbind(mt[,1:(cp-1)], nc, mt[,cp:ncol(mt)])
            attr(mt, "col_code") <- cc
            mater_table(mt)
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
          cancelButtonText = "Cancel", confirmButtonText = "Rem", showCancelButton = TRUE, size = "xs",
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

    # data frame of selected analyte
    selectedAnalyteDataframe <- shiny::reactive({
      shiny::req(getValue(rv,c("Certification","data")), getValue(rv,c("General","apm")), out())
      fnc_filter_data(rv=rv, an=out())
    })

    n <- shiny::reactive({
      shiny::req(selectedAnalyteDataframe())
      x <- selectedAnalyteDataframe()
      return(ifelse(
        test = pooling(),
        yes = sum(!x[,"L_flt"]),
        no = length(unique(as.character(x[!x[,"L_flt"],"Lab"])))
      ))
    })

    cert_mean <- shiny::reactive({
      shiny::req(selectedAnalyteDataframe())
      if (!silent) message("---materialtabelle: cert_mean---")
      data <- selectedAnalyteDataframe()[!selectedAnalyteDataframe()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs from calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      ifelse(pooling(),
             roundMT(mean(data[, "value"], na.rm = T), precision_export()),
             roundMT(mean(sapply(
               split(data[, "value"], data[, "Lab"]), mean, na.rm = T
             )), precision_export())
      )
    })

    cert_sd <- shiny::reactive({
      shiny::req(selectedAnalyteDataframe())
      if (!silent) message("---materialtabelle: cert_sd---")
      data <- selectedAnalyteDataframe()[!selectedAnalyteDataframe()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs from
      # calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      # build either standard deviation of all values or standard deviation of
      # average per lab
      ifelse(pooling(),
             roundMT(stats::sd(data[, "value"], na.rm = T), precision_export()),
             roundMT(stats::sd(sapply(
               split(data[, "value"], data[, "Lab"]), mean, na.rm = T
             )), precision_export()))
    })

    shiny::observeEvent(cert_mean(),{
      setValue(rv, c("Certification_processing","cert_mean"), cert_mean())
    })
    shiny::observeEvent(cert_sd(),{
      setValue(rv, c("Certification_processing","cert_sd"), cert_sd())
    })
    shiny::observeEvent(mater_table(),{
      # set result as new value in the R6 object
      #browser()
      mt <- recalc_mat_table(mt=mater_table())
      if (!identical(getValue(rv, c("General","materialtabelle")), mt)) {
        setValue(rv, c("General","materialtabelle"), mt)
      }
    })

    # when an Analyte-tab was selected --> update materialtabelle
    # TODO Check that analyte-column is unique
    # in case mater table has been initiated...
    # shiny::observeEvent(selectedAnalyteDataframe(),{
    shiny::observe({
      shiny::req(selectedAnalyteDataframe(), n())
      if(!is.null(mater_table())) {
        if (!silent) message("materialtabelleServer: update initiated for ", selectedAnalyteDataframe()[1,"analyte"])
        update_reactivecell(
          r = mater_table,
          colname = "mean",
          analyterow = selectedAnalyteDataframe()[1,"analyte"],
          value = cert_mean()
        )
        update_reactivecell(
          r = mater_table,
          colname = "sd",
          analyterow = selectedAnalyteDataframe()[1,"analyte"],
          value = cert_sd()
        )
        update_reactivecell(
          r = mater_table,
          colname = "n",
          analyterow = selectedAnalyteDataframe()[1,"analyte"],
          value = n()
        )
        # recalc_mat_table(mt=mater_table())
      }
    })


    # monitor table editing and update if necessary
    tmp_mater_table <- shiny::eventReactive(mater_table(),{
      if (!silent) message("materialtabelle: mater_table() has been updated; create visible table")
      mt <- mater_table()
      u_cols <- unlist(sapply(c("char", paste0("U", 1:9), "com", "U"), function(x) {
        grep(x, colnames(mt))
      }))
      for (k in u_cols) {
        # apply precision_export() only for the current analyte i
        mt[, k] <- roundMT(mt[, k], precision_U)
      }
      # rename column header for temporary display
      cc <- attr(mt, "col_code")
      if (nrow(cc)>=1) {
        for (k in 1:nrow(cc)) {
          colnames(mt)[colnames(mt) == cc[k, "ID"]] <- cc[k, "Name"]
        }
      }
      return(mt)
    })

    # the rendered, editable mat_table as seen by user
    selected_row_idx <- shiny::reactiveVal(1)
    output$matreport <- DT::renderDT(
      DT::datatable(
        data = tmp_mater_table(),
        editable = list(
          target = "cell",
          disable = list(columns = which(!(colnames(tmp_mater_table()) %in% c("k", attr(tmp_mater_table(), "col_code")[,"Name"])))-1)
        ),
        options = list(dom="t"), rownames = NULL, selection = list(mode="single", target="row", selected=selected_row_idx())
      ),
      server = TRUE
    )

    shiny::observeEvent(input$matreport_rows_selected, {
      shiny::req(mater_table())
      if (is.null(input$matreport_rows_selected)) {
        # $$ToDo$$ user deselected row --> reselect previous
        # use proxy <- DT::dataTableProxy('tab') and than
        # selectRows(proxy, selected=selected_row_idx())
      } else {
        an <- as.character(mater_table()[input$matreport_rows_selected,"analyte"])
        if (!getValue(rv, c("General", "apm"))[[an]][["confirmed"]]) {
          # mark analyte as confirmed
          tmp <- getValue(rv, c("General", "apm"))
          tmp[[an]][["confirmed"]] <- TRUE
          setValue(rv, c("General", "apm"), tmp)
        }
        if (input$matreport_rows_selected==selected_row_idx()) {
          if (is.null(out()) || out()!=an) {
            out(an)
          }
        } else {
          selected_row_idx(input$matreport_rows_selected)
        }
      }
    }, ignoreNULL = FALSE)

    # ensure update of mater_table() on user input
    shiny::observeEvent(input$matreport_cell_edit, {
      if (!silent) message("materialtabelle: user input")
      # convert value to numeric
      x <- as.numeric(gsub("[^[:digit:].]", "", input$matreport_cell_edit$value))

      # replace in correct position
      mt <- mater_table()
      mt[input$matreport_cell_edit$row, input$matreport_cell_edit$col + 1] <- x

      # update 'mater_table'
      mater_table(mt)
    })

    shiny::observeEvent(input$materheadline, {
      help_the_user("materialtabelle")
    })

    return(out)
  })
}
