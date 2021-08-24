#' @name m_transferU
#' @aliases m_TransferUUI
#' @aliases m_TransferUServer
#'
#'@title m_TransferU
#'
#'@description
#'\code{m_TransferU} will provide a module to transfer the Uncertainty
#'data in the correct format to the 'materialtable'. After Certification data
#'(initiating the material table) and Homogeneity or Stability data has been uploaded, it shows
#'the possible columns of materialtable to transfer to.
#'
#'@details
#'not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param dat Homogeneity or Stability data (reactive).
#' @param mat_tab materialtabelle to be transferred to (reactive).
#'
#' @return
#' A modified materialtabelle where values in specified U column are merged with U source.
#'
#' @rdname m_TransferU
#' @export
#' @examples
#' if (interactive()) {
#' mt <- data.frame("analyte"=LETTERS[1:6], "U1"=NA)
#' attr(mt, "col_code") <- data.frame("ID"=c("F1","U1"), "Name"=c("bla","U_test"))
#' ud <- data.frame("analyte"=rep(c("A","C","D"),2), "H_type"=gl(2,3),
#'                  "s_bb"=rep(c(0,1,0.5),2), "s_bb_min"=rep(c(1,NA,0.5),2))
#' test <- ecerto::reactiveClass$new(list("ud"=ud, "mt"=mt))
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_TransferUUI(id = "test")
#'  ),
#'  server = function(input, output, session) {
#'    m_TransferUServer(id = "test",
#'      dat = shiny::reactive({ecerto::getValue(test, "ud")}),
#'      mat_tab = shiny::reactive({ecerto::getValue(test, "mt")})
#'    )
#'  }
#' )
#' }
#'
m_TransferUUI = function(id) {
  shiny::uiOutput(shiny::NS(id, "transfer"))
}

#' @rdname m_TransferU
#' @export
m_TransferUServer = function(id, dat = shiny::reactive({NULL}), mat_tab = shiny::reactive({NULL})) {

  stopifnot(shiny::is.reactive(dat))
  stopifnot(shiny::is.reactive(mat_tab))

  shiny::moduleServer(id, function(input, output, session) {

    # return value == modified mat_tab including U values
    mt_out <- shiny::reactiveValues("value"=NULL, "changed"=0)

    # source type 'st' (is it homogeneity or stability data we want to transfer?)
    st <- shiny::reactive({
      shiny::req(dat())
      ifelse("H_type" %in% colnames(dat()), "H", "S")
    })

    # The Dropdown-Menu to select the column of materialtabelle to transfer to
    output$transfer <- shiny::renderUI({
      #browser()
      shiny::validate(shiny::need(dat(), message = paste("Please upload", switch(st(),"H"="homogeneity", "S"="stability"), "data")))
      shiny::validate(shiny::need(mat_tab(), message = "Please upload certification data to enable transfer of uncertainty values"))

      cc <- attr(mat_tab(), "col_code")
      test <- nrow(cc)>0 && any(substr(cc[, "ID"], 1, 1) == "U")
      shiny::validate(shiny::need(test, message = "Please specify a U column in material table to transfer uncertainty values"))

      shiny::tagList(
        shiny::fluidRow(shiny::HTML(paste0(
          "<p style=margin-left:6%;margin-bottom:-", switch(st(),"H"=3, "S"=0), "%><strong>Transfer ",
          switch(st(),"H"="maximum from (s_bb, s_bb_min) of H_type", "S"="values from column U_stab"),
          "</strong></p>"
        ))),
        shiny::selectInput(
          inputId=session$ns("H_Type"),
          label="",
          width='100%',
          selectize=TRUE,
          choices=switch(st(),"H"=levels(dat()[,"H_type"]), "S"="")
        ),
        shiny::selectInput(
          inputId=session$ns("U_cols"),
          label="to material table column",
          width='100%',
          selectize=TRUE,
          choices=cc[substr(cc[,"ID"],1,1)=="U","Name"]
        ),
        shiny::actionButton(inputId = session$ns("transfer_button"), label = "Transfer Now!")
      )
    })

    shiny::observeEvent(input$U_cols, {
      #browser()
      shinyjs::toggleElement(id = "H_Type", condition = st()=="H")
    })

    shiny::observeEvent(input$transfer_button, {
      mt <- mat_tab()
      cc <- attr(mt, "col_code")
      U_col <- cc[cc[, "Name"] == input$U_cols,"ID"]
      T_col <- switch(st(),"H"=c("s_bb","s_bb_min"), "S"="U_Stab")
      T_row <- switch(st(),"H"=which(dat()[,"H_type"]==input$H_Type), "S"=1:nrow(dat()))
      for (i in T_row) {
        j <- which(as.character(mt[,"analyte"])==as.character(dat()[i,"analyte"]))
        if (length(j)==1) {
          mt[j, U_col] <- max(dat()[i, T_col], na.rm=TRUE)
        }
      }
      #print(mt)
      mt_out$value <- mt
      mt_out$changed <- mt_out$changed + 1
    }, ignoreInit = TRUE)

    return(mt_out)

  })
}