#' @name mod_TransferHomogeneity
#' @aliases m_TransferHomogeneityUI
#' @aliases m_TransferHomogeneityServer
#'
#'@title m_TransferHomogeneity
#'
#'@description
#'\code{m_TransferHomogeneity} will provide a module to transfer the Homogeneity
#'Data in the correct format to the 'materialtable'. After Certification data
#'(initiating the material table) and Homogeneity data has been uploaded, it shows
#'the possible columns of materialtable to transfer to.
#'
#'@details
#'not yet
#'
#' @param id Name when called as a module in a shiny app.
#' @param homogData reactive of Homogeneity data, if present.
#' @param matTab_col_code columns of materialtable to be transferred to
#' @param matTab_analytes available analytes of materialtable
#'
#'@return
#'A reactive and one-column dataframe, containing the data to be merged.
#'
#' @rdname mod_TransferHomogeneity
#' @export
#'
m_TransferHomogeneityUI = function(id) {
  shinyjs::disabled(
    shiny::fluidRow(id = shiny::NS(id,"transferPanel"),
             #fluidRow(HTML("<p style=margin-bottom:-2%;><strong>Transfer s_bb of H_type</strong></p>"), align="right"),
             shiny::column(4,
                           shiny::selectInput(
                      inputId=shiny::NS(id,"h_transfer_H_type"),
                      label="",
                      selectize=TRUE,
                      choices=NULL
                    )
             ),
             #fluidRow(HTML("<p style=margin-bottom:-2%;><strong>to Certification table column</strong></p>"), align="right"),
             shiny::column(4,
                           shiny::selectInput(inputId=shiny::NS(id,"h_transfer_ubb"),
                                label="",
                                selectize=TRUE,
                                choices=NULL
                    )
             ),
             shiny::column(4, shiny::actionButton(inputId = shiny::NS(id,"h_transfer_ubb_button"), label = "Transfer Now!"))
    )
  )
}

#' @rdname mod_TransferHomogeneity
#' @export
m_TransferHomogeneityServer = function(id, homogData, matTab_col_code, matTab_analytes) {
  shiny::moduleServer(id, function(input, output, session) {

    cert_vals = shiny::reactiveVal()

    # activate transfer panel only, when (1) materialtabelle was created after
    # certification upload and (2) homogeneity data was uploaded
    shiny::observeEvent({
      matTab_col_code()
      homogData()
    }
    ,{
      if(!is.null(homogData()) &&!is.null(matTab_col_code())){

        shinyjs::enable(id = "transferPanel")
        message("Transfer Homogeneity Panel activated")

        cert_vals(data.frame(rep(0,length(matTab_analytes()))))

        shiny::updateSelectInput(
          session = session,
          inputId = "h_transfer_H_type",
          choices = levels(homogData()[,"H_type"]))

        shiny::updateSelectInput(
          session = session,
          inputId = "h_transfer_ubb",
          choices=matTab_col_code()[substr(matTab_col_code()[,"ID"],1,1)=="U","Name"]
        )
      }

    })

    # TODO
    return_reactive = shiny::eventReactive(input$h_transfer_ubb_button, {
      shiny::req(
        input$h_transfer_ubb,
        input$h_transfer_H_type
      )
      message("TRANSFER BUTTON clicked")
      h_vals <- homogData()
      cert_vals(stats::setNames(cert_vals(),as.character(shiny::isolate(input$h_transfer_ubb))))
      for (i in 1:length(matTab_analytes())) {
        # select cell of same analyte and Homogeneity type matTab()
        j <-
          which(
            as.character(h_vals[, "analyte"]) == matTab_analytes()[i]
            & as.character(h_vals[, "H_type"]) == shiny::isolate(input$h_transfer_H_type)
          )
        # if cell exists
        if (length(j) == 1) {
          c_name = which(matTab_col_code()[, "Name"] ==
                           shiny::isolate(input$h_transfer_ubb))
          newDF = cert_vals()
          newDF[i, matTab_col_code()[c_name, "ID"]] <-
            max(h_vals[j, c("s_bb", "s_bb_min")])
          cert_vals(newDF)
        }
      }
      return(cert_vals())
    })
    return(return_reactive)
  })
}