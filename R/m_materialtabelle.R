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
#' @param rdataUpload if uploaded via RData - reactive({rv$Certification$materialtabelle})
#' @param datreturn the session data (R6) object (ID Lab analyte replicate  value unit S_flt L_flt)
#'
#'@return nothing directly
#'
#'@examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(shinyalert::useShinyalert(), m_materialtabelleUI(id = "test")),
#'  server = function(input, output, session) {
#'  fpath <- system.file("extdata", "SR3_Fe_v26chs.RData", package="ecerto")
#'  load(fpath)
#'  rdataUpload <- reactive({res[[1]][["cert_vals"]]})
#'  datreturn <- ecerto:::test_datreturn()
#'  m_materialtabelleServer(id = "test", rdataUpload=rdataUpload, datreturn=datreturn);
#'   observe({ecerto::getValue(datreturn,"selectedAnalyteDataframe")})
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
      width = 2,
      shiny::strong(shiny::actionLink(inputId = ns("materheadline"), label = "Tab.3 Material Certification")),
      shiny::br(),
      shiny::checkboxInput(
        inputId = ns("pooling"),
        label = "pooling",
        value = FALSE
      ),
      shiny::fluidRow(
        shiny::column(width = 6,align = "center",
                      "F",
                      shiny::actionButton(inputId = ns("c_addF"), label = "Add", width = "110%"),
                      shiny::actionButton(inputId = ns("c_remF"), label = "Remove", width = "110%")
        ),
        shiny::column(width = 6,align = "center",
                      "U",
                      shiny::actionButton(inputId = ns("c_addU"), label = "Add", width = "110%"),
                      shiny::actionButton(inputId = ns("c_remU"), label = "Remove", width = "110%")
        ),
      )
    ),
    shiny::column(10, DT::DTOutput(shiny::NS(id,"matreport")))
  )
}

#' @rdname mod_materialtabelle
#' @export
m_materialtabelleServer <- function(id, rdataUpload, datreturn) {
  stopifnot(R6::is.R6(datreturn))
  stopifnot(shiny::is.reactivevalues(getValue(datreturn,NULL)))
  shiny::moduleServer(id, function(input, output, session) {

    silent = FALSE # messages

    # define table as reactiveVal to update it at different places within the module
    mater_table <- shiny::reactiveVal(NULL)
    shiny::observeEvent(getValue(datreturn, "mater_table"), {
      if (!identical(mater_table(), getValue(datreturn, "mater_table"))) {
        mater_table(getValue(datreturn, "mater_table"))
      }
    })

    precision_export <- shiny::reactive({
      current_apm = getValue(datreturn, "current_apm")
      current_apm$precision_export
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

    shiny::observeEvent(rdataUpload(),{

      if(!silent)message("m_materialtabelleServer: RData Uploaded, insert materialtabelle")
      mt <- rdataUpload()
      mt <- remove_unused_cols(mt=mt)
      mater_table(mt) # save materialtabelle
    }, ignoreNULL = TRUE)

    # data frame of selected analyte
    selectedAnalyteDataframe <- shiny::reactive({ getValue(datreturn,"selectedAnalyteDataframe") })
    n <- shiny::reactive({
      shiny::req(selectedAnalyteDataframe())
      x <- selectedAnalyteDataframe()
      return(ifelse(
        test = input$pooling,
        yes = sum(!x[,"L_flt"]),
        no = length(unique(as.character(x[!x[,"L_flt"],"Lab"])))
      ))
    })

    shiny::observeEvent(selectedAnalyteDataframe(), {
      if(!silent) message("materialtabelle: selectedAnalyteDataframe updated")

    })

    # get all availables analytes
    availableAnalytes = shiny::reactive({levels(selectedAnalyteDataframe()[["analyte"]])})

    # the data table should be created only once, since the levels shouldn't
    # change after certification upload
    shiny::observeEvent(availableAnalytes(), once = TRUE, {

      # initiate empty materialtabelle only if nothing has yet been uploaded via RData
      if(is.null(rdataUpload())) {
        if (!silent) message("m_materialtabelleServer: initiate empty materialtabelle")
        mt <- init_materialTabelle(availableAnalytes())
        mt <- remove_unused_cols(mt=mt)
        mater_table(mt) # write to reactiveValue
      }
    })

    cert_mean <- shiny::reactive({
      shiny::req(selectedAnalyteDataframe())
      if (!silent) message("---materialtabelle: cert_mean---")
      data <- selectedAnalyteDataframe()[!selectedAnalyteDataframe()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs from calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      #getValue(datreturn, "mater_table")
      #current_apm()
      ifelse(input$pooling,
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
      ifelse(input$pooling,
             roundMT(stats::sd(data[, "value"], na.rm = T), precision_export()),
             roundMT(stats::sd(sapply(
               split(data[, "value"], data[, "Lab"]), mean, na.rm = T
             )), precision_export()))
    })

    shiny::observeEvent(cert_mean(),{
      setValue(datreturn, "cert_mean", cert_mean())
    })
    shiny::observeEvent(cert_sd(),{
      setValue(datreturn, "cert_sd", cert_sd())
    })
    shiny::observeEvent(mater_table(),{
      # set result as new value in the R6 object
      if (!identical(getValue(datreturn, "mater_table"), mater_table())) {
        setValue(datreturn, "mater_table", mater_table())
        if(!silent) message("materialtabelle: recalc_mat_table; set datreturn.mater_table")
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

    # Whenever the reactiveVal gets changed, recalculate...
    shiny::observeEvent(mater_table(), {
      if (!silent) message("m_materialtabelleServer: mater_table updated, recalc")
      recalc_mat_table(mt=mater_table())
    }) # , ignoreInit = TRUE

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

    # export materialtabelle for testing. Since it perhaps hasn't created yet:
    # use try(), see https://github.com/rstudio/shinytest/issues/350
    # shiny::exportTestValues(materialtabelle = { try(tmp_mater_table()) })

    # the rendered, editable mat_table as seen by user
    output$matreport <- DT::renderDT(
      DT::datatable(
        data = tmp_mater_table(),
        editable = list(
          target = "cell",
          disable = list(columns = which(!(colnames(tmp_mater_table()) %in% c("k", attr(tmp_mater_table(), "col_code")[,"Name"])))-1)
        ),
        options = list(dom="t"), rownames = NULL, selection = list(mode="single", target="row")
      ),
      server = TRUE
    )

    shiny::observeEvent(input$matreport_rows_selected, {
      # Possibly we can omit the analyte_tabs in the future and allow the user to select analytes
      # based on the row selection in the mat_table.
      # to achieve this we would need to manipulate the R6 object (datreturn) and ensure that
      # all necessary observers point to this R6 object
      #datreturn$set("selectedAnalyteDataframe", mater_table()[input$matreport_rows_selected,"analyte"])
    })

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
  })
}
