wellPanel(
  conditionalPanel(
    condition="output.h_fileUploaded == false",
    fileInput(inputId = "h_input_files", label = "Import Excel File", multiple = FALSE, accept = c("xls","xlsx")),
    style = "margin-bottom:-25px;",
    helpText("Example Table"), imageOutput("myImage08a", inline = TRUE)
  ),
  h3(strong(textOutput("h_error_message"))),
  # analyte selection + stats
  conditionalPanel(condition="output.h_fileUploaded == true",
      fluidRow(
        column(10, DT::dataTableOutput("h_vals")),
        column(2, conditionalPanel(
          condition="output.c_fileUploaded_message != ''", 
          fluidRow(HTML("<p style=margin-bottom:-2%;><strong>Transfer s_bb of H_type</strong></p>"), align="right"),
          fluidRow(uiOutput("h_transfer_H_type")), 
          fluidRow(HTML("<p style=margin-bottom:-2%;><strong>to Certification table column</strong></p>"), align="right"),
          fluidRow(uiOutput("h_transfer_ubb")), 
          fluidRow(actionButton(inputId = "h_transfer_ubb_button", label = "Transfer Now!"), align="right")))
      ), 
      hr(),
      fluidRow(
      column(3, DT::dataTableOutput("h_overview_stats")),
      column(9, 
        fluidRow(
          column(2, uiOutput("h_sel_analyt")),
          column(2, numericInput(inputId="h_Fig_width", label="Figure Width", value=850)),
          column(2, numericInput(inputId="h_precision", label="Precision", value=4)),
          column(6, 
            fluidRow(HTML("<p style=margin-bottom:2%;><strong>Save Table/Figure</strong></p>")),
            fluidRow(downloadButton('h_Report', label="Download")), align = "right"
          )
        ),
        fluidRow(
          column(12, plotOutput("h_boxplot", inline=TRUE), offset = 0.1)
        ),
        fluidRow(
          column(12, textOutput("h_statement"), offset = 0.1),
          tags$style(type="text/css", "#h_statement {margin-top:2%;}")
        ),
        fluidRow(
          column(12, verbatimTextOutput("h_anova"), offset = 0.1),
          tags$style(type="text/css", "#h_anova {margin-top:2%;}")
        )
      )
    )
  )
)