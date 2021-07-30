#' @title STABILITY MODULE
#'
#' @param id 
#'
#' @return
#' @export
#'
#' @examples
m_StabilityUI = function(id) {
  shiny::tagList(
    shiny::tabsetPanel(
      id = shiny::NS(id, "StabilitPanel"),
      type = "hidden", 
      # when nothing is loaded
      shiny::tabPanel(
        title = "standby-Panel",
        value  = "standby",
        "nothing has uploaded yet"),
      # when something is loaded
      shiny::tabPanel(
        title = "active-Panel",
        value = "loaded",
        wellPanel(
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
    )
  )
}

m_StabilityServer = function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
    
    # Upload Notification. Since "uploadsource" is invalidated also when other
    # parameters within Stability are changed (because of the reactiveValues
    # thing), it has to be checked if it has changed value since the last change
    # to verify an upload
    uploadsource <- shiny::reactiveVal(NULL)
    shiny::observeEvent(getValue(rv,c("Stability","uploadsource")),{
      o = getValue(rv,c("Stability","uploadsource"))
      # assign upload source if (a) hasn't been assigned yet or (b), if not
      # null, has changed since the last time, for example because other data
      # source has been uploaded
      if(is.null(uploadsource()) || uploadsource() != o ){
        uploadsource(o)
      }
    })
    
    
    observeEvent(uploadsource(),{
      browser()
      test_format = getValue(rv,c("Stability","data"))
      # # TODO Wie kann das erste If-Statement umgebaut werden, dass hier keine
      # # Excel hochgeladen wird?
      # if (ncol(test_format)>=3 && "KW" %in% colnames(test_format)) {
      #   s_dat <- read_lts_input(file = input$s_input_file$datapath[1], simplify=TRUE)
      #   colnames(s_dat)[colnames(s_dat)=="KW"] <- "analyte"
      # } else {
      #   s_dat <- plyr::ldply(openxlsx::getSheetNames(file = input$s_input_file$datapath[1]), function(x) { 
      #     cbind("analyte"=x, openxlsx::read.xlsx(xlsxFile = input$s_input_file$datapath[1], sheet = x, detectDates=TRUE))
      #   }, .id = NULL)
      # }
      
      validate(need(c("analyte","Value","Date") %in% colnames(s_dat), "No all required input columns found in input file."))
      validate(need(is.numeric(s_dat[,"Value"]), "Column 'Value' in input file contains non-numeric values."))
      if (class(s_dat[,"Date"])!="Date") { s_dat[,"Date"] <- as.Date.character(s_dat[,"Date"],tryFormats = c("%Y-%m-%d","%d.%m.%Y","%Y/%m/%d")) }
      validate(need(class(s_dat[,"Date"])=="Date", "Sorry, could not convert column 'Date' into correct format."))
      s_dat[,"analyte"] <- factor(s_dat[,"analyte"])
      s_vals <- plyr::ldply(split(s_dat, s_dat[,"analyte"]), function(x) {
        x_lm <- lm(Value ~ Date, data=x)
        mon_diff <- mondf(min(x[,"Date"]),max(x[,"Date"]))
        x_slope <- summary(x_lm)$coefficients[2,1:2]
        data.frame("mon_diff"=mon_diff, "slope"=x_slope[1], "SE_slope"=x_slope[2], "U_Stab"=x_slope[1]*x_slope[2])
      }, .id="analyte")
      assign("s_dat", value = s_dat, envir = env_perm)
      assign("s_vals", value = s_vals, envir = env_perm)
    })
  })
}