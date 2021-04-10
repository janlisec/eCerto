# MATERIAL CERTIFICATION MODULE ------

.materialtabelleUI <- function(id) {

  fluidRow(
    column(12, strong("Overview")),
    column(
      2,
      strong("Material Certification"),
      br(),
      actionButton(inputId = NS(id,"show_table"), label = "recalculate"),
      checkboxInput(
        inputId = NS(id, "pooling"),
        label = "pooling",
        value = FALSE
      ),
      uiOutput(NS(id,"c_fix_col_names")),
      # validate(need(input$sel_analyt, message = "please select analyte"))
      # mater_table <- getData("mater_table")
      # selectInput(
      #   inputId = "c_fix_col_names",
      #   label = "select internal column names",
      #   selectize = TRUE,
      #   choices = attr(mater_table, "col_code")[, "ID"]
      # )
      uiOutput(NS(id,"c_displayed_col_name")),
      # validate(need(input$c_fix_col_names, message = "please select col name"))
      # mater_table <- getData("mater_table")
      # textInput(
      #   inputId = "c_displayed_col_name",
      #   label = "modify displayed column name",
      #   value = attr(mater_table, "col_code")[attr(mater_table, "col_code")[, "ID"] ==
      #                                         input$c_fix_col_names, "Name"]
      # )
      helpText(
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
    column(10, 
           DT::DTOutput(NS(id,"matreport"))
    )
  )
  # )
}

.materialtabelleServer = function(id, datreturn) {
  moduleServer(id, function(input, output, session) {
    
    # rv = reactiveValues(v = 0)
   
    # data frame of selected analyte
    sAnData = reactive({
      # rv$v
      datreturn$selectedAnalyteDataframe
    }) 
    
    lab_statistics = reactive({datreturn$lab_statistics})
    
    precision2 = 4
    # export precision2 for testing. Since it hasn't created yet
    # use try(), see https://github.com/rstudio/shinytest/issues/350
    exportTestValues(precision2 = { try(precision2) })

    # 1) table creation
    # define table as reactiveVal to update it at different places
    # within the module
    mater_table = reactiveVal(NULL)
    observe({mater_table(datreturn$t_H)})
  
    
   
    # get all availables analytes
    availableAnalytes = reactive({levels(sAnData()[["analyte"]])})
    # the data table should be created only once, since the levels shouldn't
    # change after certification upload
    observeEvent(availableAnalytes(),{
      # message(".materialtabelleServer -- levels of sAnData():")
      # message(availableAnalytes())
      c <-
        data.frame(
          "analyte" =  availableAnalytes(), # a,
          "mean" = NA,
          "F1" = 1,
          "F2" = 1,
          "F3" = 1,
          "cert_val" = NA,
          "sd" = NA,
          "n" = NA,
          "char" = 0,
          "U2" = 0,
          "U3" = 0,
          "U4" = 0,
          "U5" = 0,
          "U6" = 0,
          "U7" = 0,
          "com" = NA,
          "k" = 2,
          "U" = NA
        )
      attr(c, "col_code") <-
        data.frame(
          "ID" = c(paste0("F", 1:3), paste0("U", 2:7)),
          "Name" = c(paste0("F", 1:3), paste0("U", 2:7)),
          stringsAsFactors = FALSE
        )
      mater_table(c) # save materialtabelle
    },ignoreInit = TRUE, once = TRUE)
    
    cert_mean <- reactive({
      req(sAnData())

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
    
    cert_sd <- reactive({
      # req(input$precision2)
      data <- sAnData()[!sAnData()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs from calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      ifelse(input$pooling,
             roundMT(sd(data[, "value"], na.rm = T), precision2),
             roundMT(sd(sapply(
               split(data[, "value"], data[, "Lab"]), mean, na.rm = T
             )), precision2))
    })
    
    
    
    # when another Analyte-tab was selected --> update materialtabelle
    observeEvent(sAnData(),{
      # TODO Check that analyte-column is unique
      # if(length(unique(sAnData()[["analyte"]]))) warning("selected contains more than one unique analyte")
      # console log
      message(paste0("materialTabelle - update initiated for: ", sAnData()[1,"analyte"]))

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
      
      n = ifelse(input$pooling,
        sum(lab_statistics()[!(lab_statistics()[, "Lab"] %in% input$flt_labs), "n"]),
        nrow(lab_statistics()) - length(input$flt_labs))
      update_reactivecell(
        r = mater_table,
        colname = "n",
        analyterow = sAnData()[1,"analyte"],
        value = n
      )
    
      # # recalc all cert_mean values including correction factors
      # cert_val =
      #     apply(mater_table()[, unlist(sapply(c("mean", paste0("F", 1:3)), function(x) {
      #       grep(x, colnames(mater_table()))
      #     }))], 1, prod, na.rm = T)
      # message("materialtabelle - cert_val: ", cert_val)
      # update_reactivecell(
      #   r = mater_table,
      #   colname = "cert_val",
      #   value = cert_val
      # )
      # 
      # char = mater_table()[, "sd"] / (sqrt(mater_table()[, "n"]) * mater_table()[, "mean"])
      # update_reactivecell(mater_table,"char",value = char)
      # 
      # com <-
      #   apply(mater_table()[, unlist(sapply(c("char", paste0("U", 2:7)), function(x) {
      #     grep(x, colnames(mater_table()))
      #   }))], 1, function(x) {
      #     sqrt(sum(x ^ 2, na.rm = T))
      #   })
      # message("materialtabelle - com: ", com)
      # update_reactivecell(r = mater_table,colname = "com",value = com)
      # 
      # U <- mater_table()[, "k"] * mater_table()[, "com"]
      # message("materialtabelle - U: ", U)
      # update_reactivecell(r = mater_table,colname = "U",value = U)
      # 
      # message("materialtabelle - mater_table: ", mater_table())
    })
    
    # observe({datreturn$mater_table = mater_table()})
    observeEvent(mater_table(),{
      message("materialtabelle - datreturn updated, recalc")
      # recalc all cert_mean values including correction factors
      cert_val =
        apply(mater_table()[, unlist(sapply(c("mean", paste0("F", 1:3)), function(x) {
          grep(x, colnames(mater_table()))
        }))], 1, prod, na.rm = T)
      message("materialtabelle - cert_val: ", cert_val)
      update_reactivecell(
        r = mater_table,
        colname = "cert_val",
        value = cert_val
      )
      
      char = mater_table()[, "sd"] / (sqrt(mater_table()[, "n"]) * mater_table()[, "mean"])
      update_reactivecell(mater_table,"char",value = char)
      
      com <-
        apply(mater_table()[, unlist(sapply(c("char", paste0("U", 2:7)), function(x) {
          grep(x, colnames(mater_table()))
        }))], 1, function(x) {
          sqrt(sum(x ^ 2, na.rm = T))
        })
      message("materialtabelle - com: ", com)
      update_reactivecell(r = mater_table,colname = "com",value = com)
      
      U <- mater_table()[, "k"] * mater_table()[, "com"]
      message("materialtabelle - U: ", U)
      update_reactivecell(r = mater_table,colname = "U",value = U)
      
      message("materialtabelle - mater_table: ", mater_table())
      
      datreturn$mater_table = mater_table()
    })
    
    # monitor table editing and update if necessary
    # rename column header for temporary display
    tmp_mater_table = eventReactive(mater_table(),{
      # validate(need(datreturn$mater_table, "Certification data hasn't been uploaded yet"))
      message(".materialtabelle updated")
      a <- mater_table()
      for (k in unlist(sapply(c("char", paste0("U", 2:7), "com", "U"), function(x) {
        grep(x, colnames(a))
      }))){
        a[, k] <- roundMT(a[, k], precision2)
      }
      a
      # for (k in 1:nrow(attr(mater_table(), "col_code"))) {
      #   colnames(tmp_mater_table)[colnames(tmp_mater_table) == attr(mater_table(), "col_code")[k, "ID"]] <-
      #     attr(mater_table(), "col_code")[k, "Name"]
      # }
    },ignoreInit = TRUE)
    
    
    # export materialtabelle for testing. Since it hasn't created yet
    # use try(), see https://github.com/rstudio/shinytest/issues/350
    exportTestValues(materialtabelle = { try(tmp_mater_table()) })
    
    output$matreport = DT::renderDT({
      # # TODO validate not working here
      # validate(
      #   need(tmp_mater_table(), "Certification data hasn't been uploaded yet")
      # )
      DT::datatable(tmp_mater_table())
    })
    # output$matreport <-
    #   DT::renderDT(
    #     DT::datatable(
    #       data = tmp_mater_table,
    #       editable = list(target = "cell", disable = list(
    #         columns = attr(tmp_mater_table, "disable")
    #       )),
    #       options = list(paging = FALSE, searching = FALSE),
    #       rownames = NULL
    #     ),
    #     server = TRUE
    #   )
  })
}
