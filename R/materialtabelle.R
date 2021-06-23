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
#'  ui = shiny::fluidPage(m_materialtabelleUI(id = "test")),
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

  shiny::fluidRow(
    shiny::column(12, strong("Overview")),
    shiny::column(
      2,
      strong("Material Certification"),
      br(),
      shiny::actionButton(inputId = NS(id,"show_table"), label = "recalculate"),
      shiny::checkboxInput(
        inputId = shiny::NS(id, "pooling"),
        label = "pooling",
        value = FALSE
      ),
      shiny::uiOutput(NS(id,"c_fix_col_names")),
      shiny::uiOutput(NS(id,"c_displayed_col_name")),
      # validate(need(input$c_fix_col_names, message = "please select col name"))
      # mater_table <- getData("mater_table")
      shiny::helpText(
        "In this interactive table you can:",
        tags$br(),
        "(1) Modify values by double click on the respective cells in the table (please note that some columns are protected).",
        tags$br(),
        "(2) Click 'recalculate' to update calculations.",
        tags$br(),
        "(3) Modify the column name for editable columns.",
        tags$br(),
        "(4) Delete editable columns completely by selecting 'delete' as column name (Caution! irreversible)."
      )
    ),
    shiny::column(10,
           DT::DTOutput(NS(id,"matreport"))
    )
  )
  # )
}

#' @rdname mod_materialtabelle
#' @export
m_materialtabelleServer <- function(id, rdataUpload, datreturn) {
  stopifnot(R6::is.R6(datreturn))
  stopifnot(shiny::is.reactivevalues(ecerto::getValue(datreturn,NULL)))
  shiny::moduleServer(id, function(input, output, session) {

    # data frame of selected analyte
    sAnData = shiny::reactive({
      ecerto::getValue(datreturn,"selectedAnalyteDataframe")
    })

    lab_statistics = shiny::reactive({
      ecerto::getValue(datreturn,"lab_statistics")
      # datreturn$get("lab_statistics")
      })

    precision2 = 4
    # export precision2 for testing. Since it hasn't created yet
    # use try(), see https://github.com/rstudio/shinytest/issues/350
    shiny::exportTestValues(precision2 = { try(precision2) })

    # table creation:
    # define table as reactiveVal to update it at different places
    # within the module
    mater_table = shiny::reactiveVal(NULL)

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
    # observe({mater_table(datreturn$t_H)})

    # in case backup data
    shiny::observeEvent(rdataUpload(),{
      message("m_materialtabelleServer: insert materialtabelle")
      mater_table(rdataUpload()) # save materialtabelle
    },ignoreNULL = TRUE)

    # get all availables analytes
    availableAnalytes = shiny::reactive({levels(sAnData()[["analyte"]])})

    shiny::observeEvent(
      availableAnalytes(),
      once = TRUE,   # the data table should be created only once,
      # since the levels shouldn't
      # change after certification upload
      {
        # initiate empty materialtabelle only if nothing has yet
        # been uploaded via RData
        if(is.null(rdataUpload())) {
          message("m_materialtabelleServer: initiate empty materialtabelle")
          c = init_materialTabelle(availableAnalytes())
          mater_table(c) # save materialtabelle
        }

      })

    cert_mean <- shiny::reactive({
      shiny::req(sAnData())
      data <- sAnData()[!sAnData()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs from calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      ifelse(input$pooling,
             roundMT(mean(data[, "value"], na.rm = T), precision2),
             roundMT(mean(sapply(
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
             roundMT(stats::sd(data[, "value"], na.rm = T), precision2),
             roundMT(stats::sd(sapply(
               split(data[, "value"], data[, "Lab"]), mean, na.rm = T
             )), precision2))
    })



    # when a Analyte-tab was selected --> update materialtabelle
    # TODO Check that analyte-column is unique
    shiny::observeEvent({
      cert_mean()
      },{
      # in case mater table has been initiated...
        if(!is.null(mater_table())) {
          message(paste0("m_materialtabelleServer: update initiated for ", sAnData()[1,"analyte"]))
          update_reactivecell(
            r = mater_table,
            colname = "mean",
            analyterow = sAnData()[1,"analyte"],
            value = cert_mean()
          )
          update_reactivecell(
            r = mater_table,
            colname = "sd",
            analyterow = sAnData()[1,"analyte"],
            value = cert_sd()
          )

          n = ifelse(
            test = input$pooling,
            yes = sum(lab_statistics()[!(lab_statistics()[, "Lab"] %in% input$flt_labs), "n"]),
            no= nrow(lab_statistics()) - length(input$flt_labs)
          )

          update_reactivecell(
            r = mater_table,
            colname = "n",
            analyterow = sAnData()[1,"analyte"],
            value = n
          )
        }


    })

    # manipulate material tabelle
    output$c_fix_col_names = shiny::renderUI({
      shiny::selectInput(
        inputId = session$ns("c_fix_col_names"),
        label = "select internal column names",
        selectize = TRUE,
        choices = attr(mater_table(), "col_code")[, "ID"]
      )
    })
    output$c_displayed_col_name = shiny::renderUI({
      shiny::textInput(
        inputId = session$ns("c_displayed_col_name"),
        label = "modify displayed column name",
        value = attr(mater_table(), "col_code")[attr(mater_table(), "col_code")[, "ID"] ==
                                                  input$c_fix_col_names, "Name"]
      )
    })
    # change or delete column name:
    shiny::observeEvent(input$c_displayed_col_name,{
      if (!is.null(input$c_displayed_col_name)){
        # update column name if desired
        if(input$c_displayed_col_name != "") {
          tmp = mater_table()
          new_column_name = input$c_displayed_col_name
          attr(tmp, "col_code")[attr(tmp, "col_code")[, "ID"] == input$c_fix_col_names, "Name"] <- new_column_name
          mater_table(tmp)
        }
        # delete
        if (input$c_displayed_col_name == "delete") {
          k <- which(colnames(mater_table()) == isolate(input$c_fix_col_names))
          tmp_cert_vals <- mater_table()[, -k]
          attr(tmp_cert_vals, "disable") <-
            sapply(attr(mater_table(), "disable"), function(x) {
              ifelse(x >= k, x - 1, x)
            })
          attr(tmp_cert_vals, "col_code") <-
            attr(mater_table(), "col_code")[!attr(mater_table(), "col_code")[, "Name"] == "delete", ]
          mater_table(tmp_cert_vals)
          shiny::updateTextInput(session, inputId = "c_displayed_col_name", value = "")
          shiny::updateSelectInput(session,
                            inputId = "c_fix_col_names",
                            choices = attr(mater_table(), "col_code")[, "ID"])
        }
      }
    })



    # Whenever the reactiveVal gets changed, recalculate...
    shiny::observeEvent(mater_table(), {
      message("m_materialtabelleServer: datreturn updated, recalc")
      # recalculate all cert_mean values including correction factors
      f_cols <- unlist(sapply(c("mean", paste0("F", 1:3)), function(x) {
        grep(x, colnames(mater_table()))
      }))
      cert_val <- apply(mater_table()[,f_cols,drop=FALSE], 1, prod, na.rm = T)
      update_reactivecell(
        r = mater_table,
        colname = "cert_val",
        value = cert_val
      )

      char <- mater_table()[, "sd"] / (sqrt(mater_table()[, "n"]) * mater_table()[, "mean"])
      update_reactivecell(mater_table, "char", value = char)

      u_cols <- unlist(sapply(c("char", paste0("U", 2:7)), function(x) {
        grep(x, colnames(mater_table()))
      }))
      com <- apply(mater_table()[, u_cols], 1, function(x) {
        sqrt(sum(x ^ 2, na.rm = T))
      })
      update_reactivecell(r = mater_table,colname = "com",value = com)

      U <- mater_table()[, "k"] * mater_table()[, "com"]
      update_reactivecell(r = mater_table, colname = "U", value = U)
      ecerto::setValue(datreturn, "mater_table", mater_table())

    }, ignoreInit = TRUE)

    # monitor table editing and update if necessary
    # rename column header for temporary display
    tmp_mater_table = shiny::eventReactive(mater_table(),{
      # validate(need(datreturn$mater_table, "Certification data hasn't been uploaded yet"))
      message(".materialtabelle updated")
      a <- mater_table()
      for (k in unlist(sapply(c("char", paste0("U", 2:7), "com", "U"), function(x) {
        grep(x, colnames(a))
      }))){
        a[, k] <- roundMT(a[, k], precision2)
      }
      # Update column names if changed
      for (k in 1:nrow(attr(mater_table(), "col_code"))) {
        colnames(a)[colnames(a) == attr(mater_table(), "col_code")[k, "ID"]] <-
          attr(mater_table(), "col_code")[k, "Name"]
      }
      a
    })

    # export materialtabelle for testing. Since it hasn't created yet
    # use try(), see https://github.com/rstudio/shinytest/issues/350
    shiny::exportTestValues(materialtabelle = { try(tmp_mater_table()) })

    # the rendered, editable mat_table as seen by user
    output$matreport <- DT::renderDT(
      DT::datatable(
        data = tmp_mater_table(),
        editable = list(
          target = "cell",
          disable = list(columns = attr(tmp_mater_table(), "disable"))
        ),
        options = list(paging = FALSE, searching = FALSE),
        rownames = NULL
      ),
      server = TRUE
    )
  })
}