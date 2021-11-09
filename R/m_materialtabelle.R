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
            shiny::actionButton(inputId = ns("c_remF"), label = "Remove", width = "110%"),
            shiny::actionButton(inputId = ns("c_renF"), label = "Rename", width = "110%")
          ),
          shiny::column(
            width = 6, align = "center", "U",
            shiny::actionButton(inputId = ns("c_addU"), label = "Add", width = "110%"),
            shiny::actionButton(inputId = ns("c_remU"), label = "Remove", width = "110%"),
            shiny::actionButton(inputId = ns("c_renU"), label = "Rename", width = "110%")
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

    silent <- get_golem_config("silent") # messages

    # analyte name of the currently selected row of  mat_tab
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
      mt <- getValue(rv, c("General", "materialtabelle"))
      # add a column for absolute uncertainty if not yet present
      if (!("U_abs" %in% colnames(mt))) {
        cc <- attr(mt, "col_code")
        mt <- cbind(mt, "U_abs"=NA)
        attr(mt, "col_code") <- cc
      }
      # removal of unsed columns works for legacy data but is also removing just added columns if they have a standard name (e.g. 'F1')
      #mt <- remove_unused_cols(mt=mt)
      if (!identical(mater_table(), mt)) {
        if (!silent) message("[materialtabelle] set local 'mt' from 'rv'")
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
          # only proceed of Name and ID of the attribute are equal && set to default values
          cc[i, "ID"] == cc[i, "Name"] && (all(mt[, cc[i, "Name"]] == 1) | all(mt[, cc[i, "Name"]] == 0))
        })
        if (any(flt)) {
          mt <- mt[,!(colnames(mt) %in% cc[flt,"Name"])]
          cc <- cc[!flt,,drop=FALSE]
          attr(mt, "col_code") <- cc
        }
      }
      return(mt)
    }

    # helper function to get column indexes for U and F columns
    get_UF_cols <- function(mt=NULL, type=c("U","F")[1]) {
      switch(
        type,
        "U" = unlist(sapply(c("char", paste0("U", 1:9)), function(x) { which(colnames(mt)==x) })),
        "U_round" = unlist(sapply(c("char", "com", "U", paste0("U", 1:9)), function(x) { which(colnames(mt)==x) })),
        "F" = unlist(sapply(c("mean", paste0("F", 1:9)), function(x) { which(colnames(mt)==x) }))
      )
    }

    # helper function to update calculations
    recalc_mat_table <- function(mt=NULL) {
      if (any(is.finite(mt[,"mean"])) & any(is.finite(mt[,"sd"]))) {
        if (!silent) message("[materialtabelle] recalculate table")

        # recalculate all cert_mean values including correction factors
        mt[,"cert_val"] <- apply(mt[,get_UF_cols(mt, "F"),drop=FALSE], 1, prod, na.rm = T)
        update_reactivecell(r = mater_table, colname = "cert_val", value = mt[,"cert_val"])

        # update the 'char'acteristic uncertainty
        mt[,"char"] <- mt[, "sd"] / (sqrt(mt[, "n"]) * mt[, "mean"])
        update_reactivecell(r = mater_table, colname = "char", value = mt[,"char"])

        # update the 'com'bined uncertainty
        mt[,"com"] <- apply(mt[,get_UF_cols(mt, "U"),drop=FALSE], 1, function(x) { sqrt(sum(x ^ 2, na.rm = T)) })
        update_reactivecell(r = mater_table, colname = "com", value = mt[,"com"])

        # update the overall uncertainty
        mt[,"U"] <- mt[, "k"] * mt[, "com"]
        update_reactivecell(r = mater_table, colname = "U", value = mt[,"U"])

        # update the absolute uncertainty
        mt[,"U_abs"] <- mt[, "U"] * mt[, "cert_val"]
        update_reactivecell(r = mater_table, colname = "U_abs", value = mt[,"U_abs"])
      }
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
          cancelButtonText = "Cancel", confirmButtonText = "Rename", showCancelButton = TRUE, size = "xs",
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
          cancelButtonText = "Cancel", confirmButtonText = "Rename", showCancelButton = TRUE, size = "xs",
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
      if (!silent) message("[materialtabelle] recalc cert_mean")
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
      if (!silent) message("[materialtabelle] recalc cert_sd")
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
        an <- selectedAnalyteDataframe()[1,"analyte"]
        if (!silent) message("[materialtabelle] update initiated for ", an)
        update_reactivecell(r = mater_table, colname = "mean", analyterow = an, value = cert_mean())
        update_reactivecell(r = mater_table, colname = "sd", analyterow = an, value = cert_sd())
        update_reactivecell(r = mater_table, colname = "n", analyterow = an, value = n())
        # recalc_mat_table(mt=mater_table())
      }
    })

    # monitor table editing and update if necessary
    mater_table_print <- shiny::eventReactive(mater_table(), {
      mt <- mater_table()
      for (k in get_UF_cols(mt, "U_round")) {
        # apply precision_U to all relative uncertainty columns
        mt[, k] <- roundMT(mt[, k], precision_U)
      }
      # apply analyte specific precision for U_abs
      prec_exp <- try(sapply(getValue(rv, c("General","apm")), function(x) {x[["precision_export"]]} ))
      if (class(prec_exp)!="try-error" && is.numeric(prec_exp) && all(is.finite(prec_exp)) && length(prec_exp)==nrow(mt)) {
        mt[,"U_abs"] <- sapply(1:nrow(mt), function(i) { round(mt[i,"U_abs"], prec_exp[i]) })
      }
      # set rows with non-confirmed analytes to NA
      # non_conf <- try(sapply(getValue(rv, c("General","apm")), function(x) {x[["confirmed"]]} ))
      # if (class(non_conf)!="try-error" && is.logical(non_conf) && length(non_conf)==nrow(mt) && any(!non_conf)) {
      #   mt[!non_conf,-1,drop=FALSE] <- NA
      # }
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
      if (!silent) message("[materialtabelle] Rename and round U cols from mater_table()")
      return(mt)
    })

    # the rendered, editable mat_table as seen by user
    selected_row_idx <- shiny::reactiveVal(1)
    output$matreport <- DT::renderDT(
      DT::datatable(
        data = mater_table_print(),
        editable = list(
          target = "cell",
          disable = list(columns = which(!(colnames(mater_table_print()) %in% c("k", attr(mater_table_print(), "col_code")[,"Name"])))-1)
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
      if (!silent) message("[materialtabelle] user edited table cell")
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
