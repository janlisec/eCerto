wellPanel(
  conditionalPanel(
    condition="output.LTS_fileUploaded == false",
    fileInput(inputId = "LTS_input_file", label = "Import Excel/RData File", multiple = FALSE, accept = c("xls","xlsx","RData")),
    helpText("Example Table"), imageOutput("myImage11a", inline = TRUE)
  ),
  conditionalPanel(
    condition="output.LTS_fileUploaded == true",
    fluidRow(
      column(4, DT::dataTableOutput("LTS_vals")),
      column(8, fluidRow(column(12, DT::dataTableOutput("LTS_def")), style = "margin-bottom:15px;"),
             fluidRow(
               column(2, uiOutput("LTS_sel_KW")),
               column(2, strong("Save LTS Data"), p(), downloadButton("LTS_Save", label="Backup")),
               tags$style(type="text/css", "#LTS_Save {margin-top:-3%;}"),
               column(6, DT::dataTableOutput("LTS_NewVal")),
               tags$style(type="text/css", "#LTS_NewVal {margin-top:-3%;}"),
               column(2, strong("New Entry"), p(), actionButton(inputId = "LTS_ApplyNewValue", label = "Add data")),
               tags$style(type="text/css", "#LTS_ApplyNewValue {margin-top:-1%;}")
             ),
             fluidRow(column(12, plotOutput("LTS_plot", height = "900px"))),
             fluidRow(column(12, plotOutput("LTS_plot2", height = "450px")))
      )
    )
  )
)
