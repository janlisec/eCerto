#' @name m_transferU
#' @aliases m_TransferUUI
#' @aliases m_TransferUServer
#'
#' @title m_TransferU
#'
#' @description \code{m_TransferU} will provide a module to transfer the Uncertainty
#'    data in the correct format to the 'materialtabelle'. After Certification data
#'    (initiating the material table) and Homogeneity or Stability data has been uploaded,
#'    it shows the possible columns of materialtabelle to transfer to.
#'
#' @details not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param dat Homogeneity or Stability data (reactive).
#' @param mat_tab materialtabelle to be transferred to (reactive).
#'
#' @return
#' A modified materialtabelle where values in specified U column are merged with U source.
#'
#' @examples
#' if (interactive()) {
#' mt <- data.frame("analyte"=LETTERS[1:6], "U1"=NA)
#' attr(mt, "col_code") <- data.frame("ID"=c("F1","U1"), "Name"=c("bla","U_test"))
#' ud <- data.frame("analyte"=rep(c("A","C","D"),2), "H_type"=gl(2,3),
#'                  "s_bb"=rep(c(0,1,0.5),2), "s_bb_min"=rep(c(1,NA,0.5),2))
#' test <- eCerto::eCerto$new(list("ud"=ud, "mt"=mt))
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    eCerto:::m_TransferUUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    eCerto:::m_TransferUServer(id = "test",
#'      dat = shiny::reactive({eCerto::getValue(test, "ud")}),
#'      mat_tab = shiny::reactive({eCerto::getValue(test, "mt")})
#'    )
#'  }
#' )
#' }
#'
#' @noRd
#' @keywords internal
m_TransferUUI = function(id) {
  shiny::uiOutput(shiny::NS(id, "transfer"))
}

#' @noRd
#' @keywords internal
m_TransferUServer = function(id, dat = shiny::reactive({NULL}), mat_tab = shiny::reactive({NULL})) {

  stopifnot(shiny::is.reactive(dat))
  stopifnot(shiny::is.reactive(mat_tab))

  shiny::moduleServer(id, function(input, output, session) {

    # return value == modified mat_tab including U values
    mt_out <- shiny::reactiveValues("value"=NULL, "changed"=0)

    # source type 'st' (is it homogeneity or stability data we want to transfer?)
    st <- shiny::reactive({
      shiny::req(dat())
      st <- ifelse("H_type" %in% colnames(dat()), "H", "S")
      if (st=="H") {
        if (length(unique(dat()[,"H_type"]))==1) st <- "H_simple"
      }
      return(st)
    })

    # The Dropdown-Menu to select the column of materialtabelle to transfer to
    output$transfer <- shiny::renderUI({

      shiny::validate(shiny::need(dat(), message = paste("Please upload", switch(st(), "S"="stability", "homogeneity"), "data")))
      shiny::validate(shiny::need(mat_tab(), message = "Please upload certification data to enable transfer of uncertainty values"))

      cc <- attr(mat_tab(), "col_code")
      test <- nrow(cc)>0 && any(substr(cc[, "ID"], 1, 1) == "U")
      shiny::validate(shiny::need(test, message = "Please specify a new U column in material table to transfer uncertainty values"))

      h_choices <- switch(st(), "H"=levels(dat()[,"H_type"]), "H_simple"="hom", "S"="u_stab")
      u_choices <- cc[substr(cc[,"ID"],1,1)=="U","Name"]
      if (length(h_choices)>=2) {
        shiny::tagList(
          sub_header(txt=paste("Transfer ", switch(st(), "H"="max(s_bb, s_bb_min) of H_type", "H_simple"="max(s_bb, s_bb_min)", "S"="values from column 'u_stab'")), b=3),
          shiny::selectInput(inputId = session$ns("H_Type"), label = NULL, width = '100%', selectize = TRUE, choices = h_choices),
          shiny::selectInput(inputId = session$ns("U_cols"), label = "to material table column", width = '100%', selectize = TRUE, selected = u_choices[length(u_choices)], choices = u_choices),
          shiny::actionButton(inputId = session$ns("transfer_button"), label = "Transfer Now!")
        )
      } else {
        shiny::tagList(
          sub_header(txt=paste("Transfer ", switch(st(), "H"="max(s_bb, s_bb_min) of H_type", "H_simple"="max(s_bb, s_bb_min)", "S"="values from column 'u_stab'")), b=3),
          shinyjs::hidden(shiny::selectInput(inputId = session$ns("H_Type"), label = NULL, width = '100%', selectize = TRUE, choices = h_choices)),
          shiny::selectInput(inputId = session$ns("U_cols"), label = "to Tab.C3 column", width = '100%', selectize = TRUE, selected = u_choices[length(u_choices)], choices = u_choices),
          shiny::actionButton(inputId = session$ns("transfer_button"), label = "Transfer Now!")
        )
      }
    })

    shiny::observeEvent(input$transfer_button, {
      mt <- mat_tab()
      cc <- attr(mt, "col_code")
      U_col <- cc[cc[, "Name"] == input$U_cols,"ID"]
      T_col <- switch(st(),"H"=c("s_bb","s_bb_min"), "H_simple"=c("s_bb","s_bb_min"), "S"="u_stab")
      T_row <- switch(st(),"H"=which(dat()[,"H_type"]==input$H_Type), "H_simple"=1:nrow(dat()), "S"=1:nrow(dat()))
      for (i in T_row) {
        j <- which(as.character(mt[,"analyte"])==as.character(dat()[i,"analyte"]))
        if (length(j)==1) {
          mt[j, U_col] <- max(dat()[i, T_col], na.rm=TRUE)
        }
      }
      mt_out$value <- mt
      mt_out$changed <- mt_out$changed + 1
    }, ignoreInit = TRUE)

    return(mt_out)

  })
}
