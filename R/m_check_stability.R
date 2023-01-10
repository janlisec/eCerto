#' @title check_stability
#' @description \code{check_stability} is a Shiny module which provides
#'     tabular copy paste from Excel to a Shiny-App via a textAreaInput
#'     element.
#' @details The module will render a button initially. This button (when
#'     clicked) will open a textAreaInput. Here the user can paste a
#'     tabular range from i.e. Excel and either upload this data as
#'     data.frame to the app or cancel the operation.
#' @param id id.
#' @param rv rv.
#' @return A data.frame containing the converted string from the textAreaInput.
#' @examples
#' \dontrun{
#' rv <- eCerto:::test_rv()
#' shinyApp(
#'   ui = shiny::fluidPage(
#'     shinyjs::useShinyjs(),
#'     check_stability_UI(id = "test")
#'   ),
#'   server = function(input, output, session) {
#'     gargoyle::init("update_c_analyte")
#'     check_stability_Server(id = "test", rv = rv)
#'   }
#' )
#' }
#' @importFrom stats sd
#' @keywords internal
#' @noRd
check_stability_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    sub_header(shiny::actionLink(inputId = ns("tabC3postcert"), label = "Post Certification Test")),
    shiny::actionButton(inputId = ns("btn_main"), label = "Check"),
    shiny::uiOutput(outputId = ns("area_input")),
    shiny::uiOutput(outputId = ns("res_output"))
  )
}

#' @keywords internal
#' @noRd
check_stability_Server <- function(id, rv = NULL) {
  #ns <- shiny::NS(id)
  shiny::moduleServer(id, function(input, output, session) {
    shinyjs::hide(id = "area_input")
    shinyjs::hide(id = "res_output")

    m_c <- shiny::reactiveVal(0)
    u_c<- shiny::reactiveVal(1)
    m_m <- reactiveVal(NA)
    u_m <- reactiveVal(NA)
    sk <- reactiveVal(NA)
    ra <- reactiveVal(4)
    mt <- reactive({
      req(rv)
      gargoyle::watch("update_c_analyte")
      tmp <- getValue(rv, c("General","materialtabelle"))
      tmp <- tmp[tmp[,"analyte"]==rv$c_analyte, c("analyte","cert_val","k","U_abs")]
      m_c(tmp[,"cert_val"])
      u_c(tmp[,"U_abs"]/tmp[,"k"])
      #browser()
      ra(getValue(rv, c("General","apm"))[[rv$c_analyte]]$precision_export)
      m_m(NA)
      u_m(NA)
      sk(NA)
      return(tmp)
    })

    out <- shiny::reactiveValues(d = NULL, counter = 0)
    Err_Msg <- function(test=FALSE, message="Open Error Modal when test==FALSE", type=c("Error", "Info")[1]) {
      if (!test) {
        shiny::showModal(shiny::modalDialog(HTML(message), title = type, easyClose = TRUE))
        if (type=="Error") shiny::validate(shiny::need(expr = test, message = message, label = "Err_Msg"))
      } else {
        invisible(NULL)
      }
    }

    output$area_input <- shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          shiny::textAreaInput(
            inputId = session$ns("txt_textAreaInput"),
            label = "",
            placeholder = paste("copy/paste or enter numeric values (one per row) and press calculate afterwards"),
            width="100%",
            rows=6
          )
        ),
        shiny::fluidRow(
          shiny::actionButton(session$ns("btn_textAreaInput"), "Calculate"),
          shiny::actionButton(session$ns("btn_textAreaInput2"), "Close")
        )
      )
    })

    output$res_output <- shiny::renderUI({
      req(mt())
      shiny::tagList(
        p(),
        HTML("analyte ", mt()$analyte), br(),
        HTML("m_c = ", round(m_c(), ra()), ", u_c = ", round(u_c(), ra())), br(),
        HTML("m_m = ", round(m_m(), ra()), ", u_m = ", round(u_m(), ra())), br(),
        HTML("SK = ", round(sk(), 2))
      )
    })

    shiny::observeEvent(input$btn_main, {
      shiny::updateTextAreaInput(inputId = "txt_textAreaInput", value = "")
      shinyjs::show(id = "area_input")
      shinyjs::show(id = "res_output")
    })

    shiny::observeEvent(input$btn_textAreaInput2, {
      shinyjs::hide(id = "area_input")
      shinyjs::hide(id = "res_output")
    })

    shiny::observeEvent(input$btn_textAreaInput, {
      # read clipboard
      tmp <- strsplit(input$txt_textAreaInput, "\n")[[1]]
      # correct potential error for last col being empty
      tmp <- gsub("\t$", "\t\t", tmp)
      # remove empty rows
      tmp <- tmp[tmp!=""]
      # split at "\t" and ensure equal length
      tmp <- strsplit(tmp, "\t")
      Err_Msg(test = length(unique(sapply(tmp, length)))==1, message = "The clipboard content appears to have differing number of columns")
      # convert to numeric (what is expected by downstream functions)
      tmp <- plyr::laply(tmp, function(x) {
        x <- try(as.numeric(x))
      }, .drop = FALSE)
      Err_Msg(test = all(is.finite(tmp)), message = "The clipboard content did contain missing values or non-numeric cells<br>(now converted to NA)", type="Info")
      out$d <- tmp
      out$counter <- out$counter+1
    }, ignoreInit = TRUE)

    shiny::observeEvent(out$counter, {
      m_m(mean(out$d, na.rm=TRUE))
      u_m(stats::sd(out$d, na.rm=TRUE))
      sk(abs(m_c()-m_m())/sqrt(u_c()^2+u_m()^2))
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$tabC3postcert, {
      show_help("certification_materialtabelle_postcert")
    })

  })
}