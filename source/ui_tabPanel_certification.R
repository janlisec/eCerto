div(
  # HEADER (show only until data is uploaded via Excel or RData)
  conditionalPanel(
    condition = "output.c_fileUploaded_message == ''",
    # panel only appears as long as nothing is loaded
    wellPanel(
      fluidRow(
        #column(4, wellPanel(fileInput(inputId = "c_input_files", label = "Import Excel Files", multiple = TRUE, accept = c("xls","xlsx")))),
        # TODO is this necessary to have on server? ####
        column(4, uiOutput("c_input_files_ui")),
        # dynamic placeholder for server output
        column(4,
               fluidRow(
                 textInput(
                   inputId = "Excel_Sheet_Name",
                   label = "Name of source Excel-sheet or number",
                   value = "Ergebnisse"
                 )
               )),
        #checkboxInput(inputId="opt_input_files", label="Show complete input data", value = FALSE)
        column(4,
               fluidRow(
                 column(
                   6,
                   numericInput(
                     inputId = "start_row",
                     label = "first row",
                     value = 7
                   )
                 ),
                 column(6, numericInput(
                   inputId = "end_row",
                   label = "last row",
                   value = 14
                 ))
               ),
               fluidRow(
                 column(
                   6,
                   numericInput(
                     inputId = "start_col",
                     label = "first col",
                     value = 1
                   )
                 ),
                 column(6, numericInput(
                   inputId = "end_col",
                   label = "last col",
                   value = 8
                 ))
               )),
        style = "margin-bottom:-25px;"
      ),
      br(),
      helpText("Example Table"),
      imageOutput("myImage02a", inline = TRUE),
      br()
      #conditionalPanel('input["opt_input_files"]', DT::dataTableOutput("full_Input_Data"))
    )
  ),
  textOutput("c_fileUploaded_message"),
  
  # Once data files are loaded show downstream analyses modules
  conditionalPanel(
    condition = "output.c_fileUploaded_message != ''",
    
    # ANALYT AUSWAHL und SAMPLES FILTER
    wellPanel(
      fluidRow(
        column(3, uiOutput("sel_analyt_ui")),
        column(3, uiOutput("flt_samples")),
        column(
          3,
          numericInput(
            inputId = "precision",
            label = "Precision",
            value = 4
          )
        ),
        column(
          3,
          selectInput(
            inputId = "opt_show_files",
            label = "Data view",
            choices = c("none", "kompakt", "standard", "boxplot"),
            selected = "none"
          )
        )
      ),
      conditionalPanel(condition = "input.opt_show_files!='none'", DT::dataTableOutput("flt_Input_Data")),
      conditionalPanel(condition = "input.opt_show_files=='boxplot'", plotOutput("overview_boxplot"))
    ),
    
    # Stats
    wellPanel(
      fluidRow(column(
        9,
        strong(
          "Statistics regarding lab means, lab variances and outlier detection"
        )
      ),
      column(
        3,
        align = "right",
        checkboxInput(
          inputId = "show_stats",
          label = "hide",
          value = TRUE
        )
      )),
      conditionalPanel('!input.show_stats', DT::dataTableOutput("overview_stats"))
    ),
    
    # Stats_mean
    wellPanel(
      fluidRow(column(
        9,
        strong(
          "Statistics regarding overall mean distribution and variance testing"
        )
      ),
      column(
        3,
        align = "right",
        checkboxInput(
          inputId = "show_mstats",
          label = "hide",
          value = TRUE
        )
      )),
      conditionalPanel(
        '!input.show_mstats',
        DT::dataTableOutput("overview_mstats"),
        hr(),
        fluidRow(
          column(9, textOutput("normality_statement")),
          column(
            3,
            align = "right",
            checkboxInput(
              inputId = "show_qqplot",
              label = "show qqplot",
              value = FALSE
            )
          )
        ),
        conditionalPanel(condition = "input.show_qqplot", plotOutput("qqplot"))
      )
    ),
    
    # CertValPlot and Export section
    fluidRow(column(10,
                    wellPanel(
                      fluidRow(
                        column(
                          3,
                          fluidRow(strong("Certified Value Plot")),
                          fluidRow(uiOutput("flt_labs")),
                          fluidRow(column(
                            6,
                            numericInput(
                              inputId = "Fig01_width",
                              label = "width",
                              value = 400
                            )
                          ), column(
                            6,
                            numericInput(
                              inputId = "Fig01_height",
                              label = "height",
                              value = 400
                            )
                          )),
                          fluidRow(
                            column(
                              6,
                              numericInput(
                                inputId = "precision2",
                                label = "Precision",
                                value = 4
                              )
                            ),
                            column(
                              6,
                              strong("Download"),
                              br(),
                              downloadButton('Fig01', label = "Figure")
                            )
                          ),
                          fluidRow(column(6, strong("mean")), column(6, strong("sd"))),
                          fluidRow(column(6, textOutput("cert_mean")), column(6, textOutput("cert_sd"))),
                          fluidRow(
                            checkboxInput(
                              inputId = "pooling",
                              label = "pooling",
                              value = FALSE
                            )
                          )
                        ),
                        column(9, plotOutput("overview_CertValPlot", inline = TRUE))
                      )
                    )),
             column(
               2,
               wellPanel(
                 fluidRow(strong("Download Report")),
                 fluidRow(
                   radioButtons(
                     inputId = 'output_file_format',
                     label = NULL,
                     choices = c('PDF', 'HTML', 'Word'),
                     inline = TRUE
                   )
                 ),
                 fluidRow(
                   column(6, align = "center", downloadButton('FinalReport', label = "Analyte")),
                   column(6, align = "center", downloadButton('MaterialReport', label =
                                                                "Material"))
                 ),
                 fluidRow(
                   checkboxInput(inputId = "show_code", label = "Show Code in Report")
                 )
               )
             )),
    
    # Overall Certification
    wellPanel(fluidRow(
      column(
        2,
        strong("Material Certification"),
        br(),
        actionButton(inputId = "show_table", label = "recalculate"),
        uiOutput("c_fix_col_names"),
        uiOutput("c_displayed_col_name"),
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
      column(10, DT::DTOutput(outputId = "matreport"))
    ))
  )
)