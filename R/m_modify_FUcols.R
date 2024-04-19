#' @title modify_FUcols.
#' @description \code{modify_FUcols} is a Shiny module which provides
#'     functionality to modify column names of Tab.C3.
#' @details tbd.
#' @param id id.
#' @param rv mt.
#' @return Nothing. Module will modify the reactiveVal 'mt' provided by the user.
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   shinyApp(
#'     ui = shiny::fluidPage(
#'       shinyjs::useShinyjs(),
#'       shiny:::modify_FUcols_UI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       # rv <- eCerto:::test_rv()
#'       rv <- eCerto:::test_rv("SR3")
#'       mt <- shiny::reactiveVal()
#'       shiny::isolate(mt(eCerto::getValue(rv, c("General", "materialtabelle"))))
#'       shiny:::modify_FUcols_Server(id = "test", mt = mt)
#'       shiny::observeEvent(mt(), {
#'         print(mt)
#'       })
#'     }
#'   )
#' }
#' }
#' @importFrom shinyWidgets dropdownButton show_alert
#' @keywords internal
#' @noRd
modify_FUcols_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::dropdown(
      inputId = ns("dropdown_modifyFUcols"),
      label = "Modify F/U cols",
      width = "440px",
      circle = FALSE,
      shiny::tagList(
        bslib::layout_columns(
          shiny::selectInput(inputId = ns("selinp"), label = "Create/modify column", choices = c("<new F>", "<new U>")),
          shiny::textInput(inputId = ns("txtinp"), label = "Edit column name", value = "")
        ),
        bslib::layout_columns(
          shiny::div(
            shiny::div(id = ns("msg_type")),
            shiny::div(id = ns("msg_name"))
          ),
          shiny::div(
            shiny::actionButton(inputId = ns("btn"), label = "Apply"),
            shiny::actionLink(inputId = ns("tabC3opt"), label = "Show Help")
          )
        )
      )
    )
  )
}

#' @keywords internal
#' @noRd
modify_FUcols_Server <- function(id, mt = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    new_cols <- c("<new U>", "<new F>")

    observeEvent(mt(), {
      cc <- attr(mt(), "col_code")
      choices <- c(cc[, "Name"], new_cols)
      shiny::updateSelectInput(inputId = "selinp", choices = choices)
    })

    col_type <- shiny::reactiveVal()
    observeEvent(input$selinp, {
      cc <- attr(mt(), "col_code")
      type <- switch(input$selinp,
        "<new F>" = "F",
        "<new U>" = "U",
        substr(cc[cc[, "Name"] == input$selinp, "ID"], 1, 1)
      )
      col_type(type)
      shinyjs::html(id = "msg_type", html = shiny::HTML("[Info] col-type is: <strong>", type, "</strong>"))
      new_val <- ifelse(input$selinp %in% new_cols, "", input$selinp)
      shiny::updateTextInput(inputId = "txtinp", value = new_val, placeholder = "Enter new column name")
    })

    btn_action <- shiny::reactiveVal()
    observeEvent(input$txtinp, {
      shinyjs::html(id = "msg_name", html = shiny::HTML("[Info] displayed as: <strong>", input$txtinp, "</strong>"))
      cc <- attr(mt(), "col_code")
      shinyjs::show("btn")
      if (input$selinp %in% new_cols) {
        btn_lab <- "Add"
        if (input$txtinp == "") shinyjs::hide("btn")
      } else {
        btn_lab <- ifelse(input$txtinp == "", "Delete", "Rename")
        if (input$txtinp == input$selinp) shinyjs::hide("btn")
      }
      btn_action(btn_lab)
      shiny::updateActionButton(session = session, inputId = "btn", label = btn_lab)
    })

    observeEvent(input$btn, {
      x <- mt()
      cc <- attr(x, "col_code")
      ct <- col_type()
      cn <- input$txtinp
      if (cn %in% c(colnames(x), cc[, "Name"])) {
        shinyWidgets::show_alert(
          title = NULL,
          text = paste("Sorry, I can't", tolower(btn_action()), "this column. Please specify a unique column name.")
        )
      } else {
        if (btn_action() == "Add") {
          n <- min(which(!(1:9 %in% as.numeric(substr(cc[substr(cc[, "ID"], 1, 1) == ct, "ID"], 2, 2))))) # get smallest index number available
          cc <- rbind(cc, data.frame("ID" = paste0(ct, n), "Name" = cn))
          nc <- matrix(rep(ifelse(ct == "U", 0, 1), nrow(x)), ncol = 1, dimnames = list(rownames(x), paste0(ct, n))) # new data column
          cp <- which(colnames(x) == ifelse(ct == "U", "u_com", "cert_val")) # column position where to include the new data
          x <- cbind(x[, 1:(cp - 1)], nc, x[, cp:ncol(x)])
        }
        if (btn_action() == "Delete") {
          x <- x[, !(colnames(x) == cc[input$selinp == cc[, "Name"], "ID"])]
          cc <- cc[!(substr(cc[, "ID"], 1, 1) == ct & cc[, "Name"] == input$selinp), , drop = FALSE]
        }
        if (btn_action() == "Rename") {
          cc[substr(cc[, "ID"], 1, 1) == ct & cc[, "Name"] == input$selinp, "Name"] <- cn
        }
        attr(x, "col_code") <- cc
        mt(x)
        shinyjs::hide("btn")
      }
    })

    # Help section -------------------------------------------------------------
    shiny::observeEvent(input$tabC3opt, {
      show_help("certification_materialtabelle_opt")
    })
  })
}
