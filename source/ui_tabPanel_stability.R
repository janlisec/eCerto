wellPanel(
  conditionalPanel(condition="output.s_fileUploaded == false",
                   fileInput(inputId = "s_input_file", label = "Import Excel File", multiple = FALSE, accept = c("xls","xlsx")),
                   helpText("Example Table"), imageOutput("myImage10a", inline = TRUE), helpText("or use LTS format table as alternative"), imageOutput("myImage11b", inline = TRUE)
  ),
  conditionalPanel(
    condition="output.s_fileUploaded == true",
    fluidRow(
      column(10, DT::dataTableOutput("s_vals")),
      column(
        2, 
        conditionalPanel(
          condition="output.c_fileUploaded_message != ''",
          fluidRow(HTML("<p style=margin-bottom:-2%;><strong>Transfer U_stab to Certification table column</strong></p>"), align="right"),
          fluidRow(uiOutput("s_transfer_ubb")), 
          fluidRow(actionButton(inputId = "s_transfer_ubb_button", label = "Transfer Now!"), align="right")
        )
      )
    ),
    fluidRow(
      column(4, DT::dataTableOutput("s_overview")),
      column(
        8, 
        fluidRow(column(6,uiOutput("s_sel_analyte")), column(6,uiOutput("s_sel_dev"))),
        fluidRow(plotOutput("s_plot")),uiOutput("s_info"))
    )
  )
)