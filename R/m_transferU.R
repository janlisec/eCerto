#' @title m_TransferU.
#' @description \code{m_TransferU} will provide a module to transfer the
#'     uncertainty data in the correct format to the 'materialtabelle'.
#'     After Certification data (initiating the material table) and
#'     Homogeneity or Stability data has been uploaded, it shows the
#'     possible columns of 'materialtabelle' to transfer to.
#' @details not yet
#' @param id Name when called as a module in a shiny app.
#' @param rv eCerto object.
#' @param type Modul can be used in 'H' or 'S' modul as specified by type parameter.
#' @return The materialtabelle within the eCerto object is modified in place.
#' @examples
#' if (interactive()) {
#' x <- eCerto:::read_zenodo(id = "8380870")
#' rv <- eCerto:::list2rv(x)
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_TransferUUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    m_TransferUServer(id = "test", rv = rv, type = "H")
#'    observeEvent(getValue(rv, c("General", "materialtabelle")), {
#'      print(getValue(rv, c("General", "materialtabelle")))
#'    }, ignoreInit = TRUE)
#'  }
#' )
#' }
#'
#' @importFrom shinyWidgets dropdownButton show_alert
#' @noRd
#' @keywords internal
m_TransferUUI = function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shinyWidgets::dropdown(
      inputId = ns("dropdown_transferU"),
      label = "Transfer uncertainty",
      shiny::uiOutput(outputId = ns("content"))
    )
  )
}

#' @noRd
#' @keywords internal
m_TransferUServer = function(id, rv, type = c("H", "S")) {

  type <- match.arg(type)

  shiny::moduleServer(id, function(input, output, session) {

    # local copy of data
    dat <- shiny::reactive({
      out <- NULL
      if (type=="H") { out <- getValue(rv, c("Homogeneity", "h_vals")) }
      if (type=="S") { out <- getValue(rv, c("Stability", "s_vals")) }
      return(out)
    })

    mat_tab <- shiny::reactive({
      getValue(rv, c("General", "materialtabelle"))
    })

    check_mt <- function(mt) {
      out <- ""
      if (is.null(mt)) out <- shiny::HTML("Please upload certification data to initiate Tab.C3 where all uncertainty components are combined.")
      if (!is.null(mt) && !any(substr(attr(mat_tab(), "col_code")[,"ID"], 1, 1) == "U" )) {
        out <- shiny::HTML("Please specify a new <i>u<sub>i</sub></i> column in Tab.C3. Values can only be transfered to user defined columns.")
      }
      return(out)
    }

    # source type 'st' (homogeneity data can be simple or with different sub-types)
    st <- shiny::reactive({
      shiny::req(dat())
      st <- type
      if (st=="H" && length(unique(dat()[,"H_type"]))==1) { st <- "H_simple" }
      return(st)
    })

    # The Dropdown-Menu to select the column of materialtabelle to transfer to
    modal_content <- function() {
      test <- check_mt(mt = mat_tab())
      if (test=="") {
        cc <- attr(mat_tab(), "col_code")
        h_choices <- switch(st(), "H"=levels(dat()[,"H_type"]), "H_simple"="hom", "S"="u_stab")
        u_choices <- cc[substr(cc[,"ID"],1,1)=="U","Name"]
        shiny::tagList(
          sub_header(txt=paste("Transfer ", switch(st(), "H"="max(s_bb, s_bb_min) of H_type", "H_simple"="max(s_bb, s_bb_min)", "S"="values from column 'u_stab'"))),
          if (length(h_choices)>=2) {
            shiny::selectInput(inputId = session$ns("H_Type"), label = NULL, width = '100%', selectize = TRUE, choices = h_choices)
          } else {
            shinyjs::hidden(shiny::selectInput(inputId = session$ns("H_Type"), label = NULL, width = '100%', selectize = TRUE, choices = h_choices))
          },
          shiny::selectizeInput(
            inputId = session$ns("U_cols"), label = "to Tab.C3 column", width = '100%', selected = u_choices[length(u_choices)], choices = u_choices,
            options = list(render = I('{
              item: function(item, escape) {
                return "<div>" + item.value + "</div>";
              },
              option: function(item, escape) {
                return "<div>" + item.value + "</div>";
              }
            }'))
          ),
          shiny::actionButton(inputId = session$ns("btn"), label = "Transfer")
        )
      } else {
        test
      }
    }

    output$content <- shiny::renderUI({
      modal_content()
    })

    shiny::observeEvent(input$btn, {
      mt <- mat_tab()
      cc <- attr(mt, "col_code")
      U_col <- cc[cc[, "Name"] == input$U_cols,"ID"]
      T_col <- switch(st(),"H"=c("s_bb","s_bb_min"), "H_simple"=c("s_bb","s_bb_min"), "S"="u_stab")
      T_row <- switch(st(),"H"=which(dat()[,"H_type"]==input$H_Type), "H_simple"=1:nrow(dat()), "S"=1:nrow(dat()))
      for (i in T_row) {
        j <- which(as.character(mt[,"analyte"])==as.character(dat()[i,"analyte"]))
        if (length(j)==1 & is.finite(max(dat()[i, T_col], na.rm=TRUE))) {
          mt[j, U_col] <- max(dat()[i, T_col], na.rm=TRUE)
        }
      }
      if (!identical(mat_tab()[, U_col], mt[, U_col])) {
        setValue(rv, c("General", "materialtabelle"), mt)
      } else {
        shinyWidgets::show_alert(title = NULL, text = "Sorry, this transfer did not change Tab.C3 (column already contains similar values).")
      }
    })

  })
}
