# TRANSFER HOMOGENEITY MODULE -------------------------
.TransferHomogeneityUI = function(id) {
  shinyjs::disabled(
    fluidRow(id = NS(id,"transferPanel"),
             #fluidRow(HTML("<p style=margin-bottom:-2%;><strong>Transfer s_bb of H_type</strong></p>"), align="right"),
             column(4,
                    selectInput(
                      inputId=NS(id,"h_transfer_H_type"), 
                      label="", 
                      selectize=TRUE, 
                      choices=NULL
                    )
             ),
             #fluidRow(HTML("<p style=margin-bottom:-2%;><strong>to Certification table column</strong></p>"), align="right"),
             column(4,
                    selectInput(inputId=NS(id,"h_transfer_ubb"), 
                                label="", 
                                selectize=TRUE,
                                choices=NULL
                    )           
             ),
             column(4, actionButton(inputId = NS(id,"h_transfer_ubb_button"), label = "Transfer Now!"))
    )
  )
}

# This module is called in lowest server module. It transfers values from the
# Homogeneity module into the materialtabelle
.TransferHomogeneityServer = function(id, datreturn) {
  moduleServer(id, function(input, output, session) {
    
    # activate transfer panel only, when (1) materialtabelle was created after 
    # certification upload and (2) homogeneity data was uploaded
    observeEvent({
      datreturn$mater_table
      datreturn$h_vals
    }
    ,{
      if(!is.null(datreturn$h_vals) && !is.null(datreturn$mater_table)){
        shinyjs::enable(id = "transferPanel")
        message("Transfer Homogeneity Panel activated")
        
        updateSelectInput(
          session = session,
          inputId = "h_transfer_H_type",
          choices = levels(datreturn$h_vals[,"H_type"]))
        
        updateSelectInput(
          session = session,
          inputId = "h_transfer_ubb",
          choices=attr(datreturn$mater_table, "col_code")[substr(attr(datreturn$mater_table, "col_code")[,"ID"],1,1)=="U","Name"]
        )
      }
      
    }) 
    
    # TODO
    observeEvent(input$h_transfer_ubb_button, {
      req(
        # h_Data(), 
        datreturn$h_vals, 
        datreturn$mater_table, 
        input$h_transfer_ubb, 
        input$h_transfer_H_type
      )
      message("TRANSFER BUTTON clicked")
      h_vals <- datreturn$h_vals
      cert_vals <- datreturn$mater_table
      for (i in 1:nrow(cert_vals)) {
        # select cell of same analyte and Homogeneity type
        j <-
          which(
            as.character(h_vals[, "analyte"]) ==  as.character(cert_vals[i, "analyte"]) 
            &
              as.character(h_vals[, "H_type"]) == isolate(input$h_transfer_H_type)
          )
        # if cell exists
        if (length(j) == 1) {
          c_name = which(attr(cert_vals, "col_code")[, "Name"] ==
                           isolate(input$h_transfer_ubb))
          cert_vals[i, attr(cert_vals, "col_code")[c_name, "ID"]] <-
            max(h_vals[j, c("s_bb", "s_bb_min")])
        }
      }
      print(cert_vals)
      datreturn$t_H <- cert_vals 
      # assign("cert_vals", value=cert_vals, envir = env_perm)
      # shinyjs::click(id="show_table")
    })
  })
}