# MATERIAL CERTIFICATION MODULE ------

.materialtabelleUI <- function(id) {
  # ns <- NS(id)
  # tagList(
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
      # cert_vals <- getData("cert_vals")
      # selectInput(
      #   inputId = "c_fix_col_names",
      #   label = "select internal column names",
      #   selectize = TRUE,
      #   choices = attr(cert_vals, "col_code")[, "ID"]
      # )
      uiOutput(NS(id,"c_displayed_col_name")),
      # validate(need(input$c_fix_col_names, message = "please select col name"))
      # cert_vals <- getData("cert_vals")
      # textInput(
      #   inputId = "c_displayed_col_name",
      #   label = "modify displayed column name",
      #   value = attr(cert_vals, "col_code")[attr(cert_vals, "col_code")[, "ID"] ==
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

.materialtabelleServer <- function(id, dat) {
  # stopifnot(is.reactive(dat))
  # req(dat)
  moduleServer(id, function(input, output, session) {
    #dat = reactive(d()$data)
  
    precision2 = NULL
    print("materialtabelle, neuer Tab gedrückt?")
    cert_mean <- reactive({
      # req(dat(), input$precision2)
      data <- dat()[!dat()[, "L_flt"], ]
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
      data <- dat()[!dat()[, "L_flt"], ]
      # re-factor Lab because user may have excluded one or several labs from calculation of cert mean while keeping it in Figure
      data[, "Lab"] <- factor(data[, "Lab"])
      ifelse(input$pooling,
             roundMT(sd(data[, "value"], na.rm = T), precision2),
             roundMT(sd(sapply(
               split(data[, "value"], data[, "Lab"]), mean, na.rm = T
             )), precision2))
    })
    
    a = levels(dat()[["analyte"]])
    cert_vals <-
      data.frame(
        "analyte" = a,
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
    
    tmp_cert_vals = cert_vals
    output$matreport = DT::renderDT(tmp_cert_vals)
    # output$matreport <-
    #   DT::renderDT(
    #     DT::datatable(
    #       data = tmp_cert_vals,
    #       editable = list(target = "cell", disable = list(
    #         columns = attr(tmp_cert_vals, "disable")
    #       )),
    #       options = list(paging = FALSE, searching = FALSE),
    #       rownames = NULL
    #     ),
    #     server = TRUE
    #   )
    
  })
}
