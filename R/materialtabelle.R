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
#' @param rdataUpload if uploaded via RData - reactive({rv$Certifications$materialtabelle})
#' @param datreturn the session data (R6) object
#'
#'@return nothing
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
#'   #observeEvent(out(), {print(out())})
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
      shiny::strong("Material Certification"), shiny::br(),
      shiny::checkboxInput(
        inputId = ns("pooling"),
        label = "pooling",
        value = FALSE
      ),
      shiny::fluidRow(
        shiny::column(width = 3, shiny::actionButton(inputId = ns("c_addF"), label = "Add F")),
        shiny::column(width = 3, shiny::actionButton(inputId = ns("c_remF"), label = "Rem F")),
        shiny::column(width = 3, shiny::actionButton(inputId = ns("c_addU"), label = "Add U")),
        shiny::column(width = 3, shiny::actionButton(inputId = ns("c_remU"), label = "Rem U"))
      ),
      shiny::helpText(
        "In this interactive table you can add columns of correction factors of the mean as well as uncertainty contributions.",
        shiny::tags$br(),
        "You can modify values in these columns by double click on the respective cells in the table (please note that other columns are protected from editing)."
      )
    ),
    shiny::column(10, DT::DTOutput(shiny::NS(id,"matreport")))
  )
}

#' @rdname mod_materialtabelle
#' @export
m_materialtabelleServer <- function(id, rdataUpload, datreturn) {

  stopifnot(R6::is.R6(datreturn))
  stopifnot(shiny::is.reactivevalues(ecerto::getValue(datreturn,NULL)))

  shiny::moduleServer(id, function(input, output, session) {

    whereami::cat_where(where = "Materialtabelle")
    ns <- shiny::NS(id)

    # define table as reactiveVal to update it at different places within the module
    mater_table <- shiny::reactiveVal(NULL)

    # create and test precision2. Since it hasn't created yet use try(), see https://github.com/rstudio/shinytest/issues/350
    precision2 <- 4
    shiny::exportTestValues(precision2 = { try(precision2) })

    # helper function to remove unused user columns
    # @ Jan: ich würde die Funktion gerne auch auslagern um Tests zu schreiben. OK?
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
      # recalculate all cert_mean values including correction factors
      f_cols <- unlist(sapply(c("mean", paste0("F", 1:9)), function(x) { grep(x, colnames(mt)) }))
      mt[,"cert_val"] <- apply(mt[,f_cols,drop=FALSE], 1, prod, na.rm = T)
      ecerto::update_reactivecell(r = mater_table, colname = "cert_val", value = mt[,"cert_val"])

      # update the 'char'acteristic uncertainty
      mt[,"char"] <- mt[, "sd"] / (sqrt(mt[, "n"]) * mt[, "mean"])
      ecerto::update_reactivecell(mater_table, "char", value = mt[,"char"])

      # update the 'com'bined uncertainty
      u_cols <- unlist(sapply(c("char", paste0("U", 1:9)), function(x) { grep(x, colnames(mt)) }))
      mt[,"com"] <- apply(mt[,u_cols,drop=FALSE], 1, function(x) { sqrt(sum(x ^ 2, na.rm = T)) })
      update_reactivecell(r = mater_table, colname = "com", value = mt[,"com"])

      # update the overall uncertainty
      mt[,"U"] <- mt[, "k"] * mt[, "com"]
      ecerto::update_reactivecell(r = mater_table, colname = "U", value = mt[,"U"])

      # set result as new value in the R6 object
      ecerto::setValue(datreturn, "mater_table", mt)
      message("materialtabelle: set datreturn.mater_table")
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
              cc <- cc[!(substr(cc[,"ID"],1,1)=="F" & cc[,"Name"]==input$tmp),,drop=FALSE]
              mt <- mater_table()
              mt <- mt[,!(colnames(mt)==input$tmp)]
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
      cc <- attr(mater_table(), "col_code")
      if (any(substr(cc[,"ID"],1,1)=="U")) {
        choices <- cc[substr(cc[,"ID"],1,1)=="U","Name"]
        shinyalert::shinyalert(
          html = TRUE, text = shiny::tagList(shiny::selectInput(inputId = session$ns("tmp"), label = "Select to remove", choices = choices)),
          cancelButtonText = "Cancel", confirmButtonText = "Rem", showCancelButton = TRUE, size = "xs",
          callbackR = function(value) {
            if (value) {
              cc <- cc[!(substr(cc[,"ID"],1,1)=="U" & cc[,"Name"]==input$tmp),,drop=FALSE]
              mt <- mater_table()
              mt <- mt[,!(colnames(mt)==input$tmp)]
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
    sAnData = shiny::reactive({ ecerto::getValue(datreturn,"selectedAnalyteDataframe") })

    # get current lab statistics
    lab_statistics = shiny::reactive({ ecerto::getValue(datreturn,"lab_statistics") })

    # Homogeneity transfer
     shiny::observeEvent(getValue(datreturn,"t_H") ,{
      # if(!is.null(mater_table())){

        transferred_array = ecerto::getValue(datreturn,"t_H")
        mergeby = names(transferred_array)
        mater_table_tmp = mater_table()
        mater_table_tmp[,mergeby] = transferred_array
        mater_table(mater_table_tmp)
        if(length(mergeby)>1) stop("transferred columns should be 1")
        # merged = merge(mater_table(), transferedDF, by=mergeby)
      #   # mater_table(merged)
      # } else {
      #   stop(
      #     "There is an error here! Before the materialtable is initiated,
      #     it should not be possible to transfer Homogeneity data."
      #   )
      # }

    }, ignoreNULL = TRUE)

    # in case backup data
    shiny::observeEvent(rdataUpload(),{
      message("m_materialtabelleServer: RData Uploaded, insert materialtabelle")
      mt <- rdataUpload()
      mt <- remove_unused_cols(mt=mt)
      mater_table(mt) # save materialtabelle
    }, ignoreNULL = TRUE)

    # get all availables analytes
    availableAnalytes = shiny::reactive({levels(sAnData()[["analyte"]])})

    # the data table should be created only once, since the levels shouldn't change after certification upload
    shiny::observeEvent(availableAnalytes(), once = TRUE, {
      # initiate empty materialtabelle only if nothing has yet  been uploaded via RData
      if (is.null(rdataUpload())) {
        message("m_materialtabelleServer: initiate empty materialtabelle")
        mt <- ecerto::init_materialTabelle(availableAnalytes())
        mt <- remove_unused_cols(mt=mt)
        mater_table(mt) # write to reactiveValue
      }
    })

    cert_mean <- shiny::reactive({
      shiny::req(sAnData())
      data <- sAnData()[!sAnData()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs from calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      ifelse(input$pooling,
             ecerto::roundMT(mean(data[, "value"], na.rm = T), precision2),
             ecerto::roundMT(mean(sapply(
               split(data[, "value"], data[, "Lab"]), mean, na.rm = T
             )), precision2)
      )
    })

    cert_sd <- shiny::reactive({
      # req(input$precision2)
      data <- sAnData()[!sAnData()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs from calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      ifelse(input$pooling,
             ecerto::roundMT(stats::sd(data[, "value"], na.rm = T), precision2),
             ecerto::roundMT(stats::sd(sapply(
               split(data[, "value"], data[, "Lab"]), mean, na.rm = T
             )), precision2))
    })

    # when a Analyte-tab was selected --> update materialtabelle
    # TODO Check that analyte-column is unique
    # in case mater table has been initiated...
    shiny::observeEvent({ cert_mean() }, {
      if(!is.null(mater_table())) {
        message(paste0("m_materialtabelleServer: update initiated for ", sAnData()[1,"analyte"]))
        ecerto::update_reactivecell(
          r = mater_table,
          colname = "mean",
          analyterow = sAnData()[1,"analyte"],
          value = cert_mean()
        )
        ecerto::update_reactivecell(
          r = mater_table,
          colname = "sd",
          analyterow = sAnData()[1,"analyte"],
          value = cert_sd()
        )
        n <- ifelse(
          test = input$pooling,
          yes = sum(lab_statistics()[!(lab_statistics()[, "Lab"] %in% input$flt_labs), "n"]),
          no = nrow(lab_statistics()) - length(input$flt_labs)
        )
        ecerto::update_reactivecell(
          r = mater_table,
          colname = "n",
          analyterow = sAnData()[1,"analyte"],
          value = n
        )
        recalc_mat_table(mt=mater_table())
      }
    })

    # Whenever the reactiveVal gets changed, recalculate...
    shiny::observeEvent(mater_table(), {
      message("m_materialtabelleServer: datreturn updated, recalc")
      recalc_mat_table(mt=mater_table())
    }, ignoreInit = TRUE)

    # monitor table editing and update if necessary
    tmp_mater_table <- shiny::eventReactive(mater_table(),{
      message("m_materialtabelle: mater_table() updated")
      mt <- mater_table()
      u_cols <- unlist(sapply(c("char", paste0("U", 1:9), "com", "U"), function(x) {
        grep(x, colnames(mt))
      }))
      for (k in u_cols) {
        mt[, k] <- ecerto::roundMT(mt[, k], precision2)
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
    shiny::exportTestValues(materialtabelle = { try(tmp_mater_table()) })

    # the rendered, editable mat_table as seen by user
    output$matreport <- DT::renderDT(
      DT::datatable(
        data = tmp_mater_table(),
        editable = list(
          target = "cell",
          disable = list(columns = which(!(colnames(tmp_mater_table()) %in% c("k", attr(tmp_mater_table(), "col_code")[,"Name"])))-1)
        ),
        options = list(paging = FALSE, searching = FALSE),
        rownames = NULL
      ),
      server = TRUE
    )

    # ensure update of mater_table() on user input
    shiny::observeEvent(input$matreport_cell_edit, {
     # convert value to numeric
     x <- as.numeric(gsub("[^[:digit:].]", "", input$matreport_cell_edit$value))

     # replace in correct position
     mt <- mater_table()
     mt[input$matreport_cell_edit$row, input$matreport_cell_edit$col + 1] <- x

     # update 'mater_table'
     mater_table(mt)
   })

  })
}