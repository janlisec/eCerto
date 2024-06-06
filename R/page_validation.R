#' @title Validation-Page
#' @description \code{page_start} is the module for eCerto startup.
#'
#' @details Providing the backup/restore modules as well as the Excel upload.
#'
#' @param id Name when called as a module in a shiny app.
#'
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = shiny::fluidPage(
#'       eCerto:::page_startUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       rv <- eCerto::eCerto$new(eCerto:::init_rv()) # initiate persistent variables
#'       eCerto:::page_startServer(id = "test", rv = rv)
#'     }
#'   )
#' }
#'
#' @return Nothing
#' @noRd

page_validationUI <- function(id) {
  ns <- shiny::NS(id)
  fig_V1_card <- bslib::card(
    id = ns("fig_V1_panel"),
    style = "resize:vertical;",
    full_screen = TRUE,
    bslib::card_header(
      shiny::actionLink(inputId = ns("Help_figV1"), "Fig.V1 - Working range")
    ),
    bslib::card_body(
      fill = TRUE,
      bslib::layout_sidebar(
        padding = 0,
        sidebar = bslib::sidebar(
          position = "right", open = "open", width = "280px",
          shiny::div(
            shinyWidgets::pickerInput(inputId = ns("opt_V1_anal"), label = "Analyte(s)", multiple = TRUE, choices = ""),
            shinyWidgets::pickerInput(inputId = ns("opt_V1_k"), label = "Calibration Level(s)", multiple = TRUE, choices = "")
          )
        ),
        shiny::plotOutput(outputId = ns("fig_V1"))
      )
    )
  )

  anal_V_details_card <- bslib::card(
    bslib::card_header(
      class = "d-flex justify-content-between",
      "Fig.V3 - Working range (details)",
      shinyWidgets::pickerInput(inputId = ns("opt_V2_vals"), label = NULL, multiple = FALSE, choices = c("Area_Analyte","Area_IS","Analyte/IS","relative(Analyte/IS)"))
    ),
    bslib::card_body(
      fill = TRUE,
      shiny::plotOutput(outputId = ns("fig_V3")),
      DT::DTOutput(outputId = ns("tab_V2"))
    )
  )

  tab_V1_card <- bslib::card(
    fill = TRUE,
    bslib::card_header(shiny::actionLink(inputId = ns("Help_tabV1"), "Tab.V1 - Linearity test")),
    bslib::card_body(
      fill = TRUE,
      bslib::layout_sidebar(
        padding = 0,
        sidebar = bslib::sidebar(
          position = "right", open = "open", width = "280px",
          shiny::div(
            bslib::layout_columns(
              shinyWidgets::pickerInput(inputId = ns("opt_tabV1_alpha"), label = "alpha", multiple = FALSE, choices = c(0.01, 0.05), selected = 0.05),
              shinyWidgets::pickerInput(inputId = ns("opt_tabV1_k"), label = "k", multiple = FALSE, choices = 2:4, selected = 3)
            ),
            bslib::layout_columns(
              shinyWidgets::pickerInput(inputId = ns("opt_tabV1_dec"), label = "dec sep", multiple = FALSE, choices = c(",", "."), selected = ","),
              shinyWidgets::pickerInput(inputId = ns("opt_tabV1_precision"), label = "digits", multiple = FALSE, choices = 0:6, selected = 3)
            ),
            shiny::hr(),
            shiny::checkboxInput(inputId = ns("opt_tabV1_fltLevels"), label = shiny::HTML("Omit Out<sub>F</sub> Levels"), value = FALSE),
            shiny::checkboxInput(inputId = ns("opt_tabV1_useAnalytes"), label = "Use Analytes from Fig.V1", value = TRUE),
            shiny::checkboxInput(inputId = ns("opt_tabV1_useLevels"), label = "Use Level range of Fig.V1", value = FALSE),
            shiny::hr(),
            DT::DTOutput(outputId = ns("tab_V1_detail"))
            #shiny::actionButton(inputId = ns("opt_tabV1_datamodal"), label = shiny::HTML("Show<br>data"))
          )
        ),
        shiny::div(DT::DTOutput(outputId = ns("tab_V1")))
      )
    )
  )

  fig_V2_card <- bslib::card(
    id = ns("fig_V2_panel"),
    full_screen = TRUE,
    min_height = "960px",
    bslib::card_header("Fig.V2 Linearity"),
    bslib::card_body(shiny::plotOutput(outputId = ns("fig_V2")))
  )

  tab_V3_card <- bslib::card(
    id = ns("tab_V3_panel"),
    bslib::card_header("Tab.V3 Imported data"),
    bslib::card_body(DT::DTOutput(outputId = ns("tab_V3")))
  )

  v_report_card <- bslib::card(
    bslib::card_header("Method Validation Report"),
    bslib::card_body(shiny::downloadButton(outputId = ns("v_report"), label = "Validation Report"))
  )


  shiny::tagList(
    shiny::conditionalPanel(
      condition = "output.V_fileUploaded == false",
      ns = ns, # namespace of current module
      shiny::fileInput(
        inputId = ns("inp_file"),
        label = shiny::actionLink(inputId = ns("InputHelp"), "Import Excel"),
        multiple = F,
        placeholder = "xlsx",
        accept = c("xls", "xlsx")
      ),
      shiny::p(shiny::helpText("Example Table (Agilent MassHunter Export format)")),
      shiny::img(src = "www/rmd/fig/V_Modul_Import.png")
    ),
    shiny::conditionalPanel(
      condition = "output.V_fileUploaded == true",
      ns = ns, # namespace of current module
      bslib::layout_columns(
        fig_V1_card,
        anal_V_details_card,
        tab_V1_card,
        fig_V2_card,
        tab_V3_card,
        v_report_card,
        col_widths =  bslib::breakpoints(
          sm = c(12, 12),
          xl = c(8, 4)
        )
      )
    )
  )

}

#' @noRd
page_validationServer <- function(id, rv, msession = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    output$v_report <- shiny::downloadHandler(
      filename = function() {"Validation Report.html"},
      content = function(file) {
        # Copy the report file to a temporary directory before processing it
        rmdfile <- get_local_file("report_vorlage_validation.[Rr][Mm][Dd]$")
        logofile <- "BAMLogo2015.png"
        # render the markdown file
        shiny::withProgress(
          expr = {
            incProgress(0.5)
            rmarkdown::render(
              input = rmdfile,
              output_file = file,
              output_format = rmarkdown::html_document(),
              params = list(
                "inp_data" = style_tabV3(tab()),
                "tab_V1" = style_tabV1(df = tab_V1(), precision = as.numeric(input$opt_tabV1_precision), selected = tab_V1_rows_selected()),
                "fig_V1" = function() { prepFigV1(ab())},
                "fig_V1_width" = calc_bxp_width(n = length(input$opt_V1_anal)*length(input$opt_V1_k), w_point = 28, w_axes = 120),
                "logo_file" = logofile
              ),
              envir = new.env(parent = globalenv())
            )
          },
          message = "Rendering Validation Report.."
        )
      }
    )

    # upload info used in UI part
    output$V_fileUploaded <- shiny::reactive({
      return(!is.null(input$inp_file$datapath))
    })
    shiny::outputOptions(output, "V_fileUploaded", suspendWhenHidden = FALSE)

    # prep_tabV1 <- function(tab, alpha, k, flt_outliers) {
    #   plyr::ldply(levels(tab[,"Analyte"]), function(a) {
    #     prepTabV1(tab = tab, a = a, alpha = alpha, k = k, flt_outliers = flt_outliers)
    #   })
    # }

    tab <- shiny::reactive({
      req(input$inp_file$datapath)
      read_Vdata(file = input$inp_file$datapath, fmt = c("Agilent"))
    })

    shiny::observeEvent(tab(), {
      shinyWidgets::updatePickerInput(
        session = session, inputId = "opt_V1_anal", choices = levels(tab()$Analyte), selected = levels(tab()$Analyte),
        choicesOpt = list(subtext = paste0("(",1:length(levels(tab()$Analyte)),")")),
        options = list('container' = "body", 'actions-box' = TRUE, 'deselect-all-text' = "None", 'select-all-text' = "All", 'none-selected-text' = "None selected")
      )
      shinyWidgets::updatePickerInput(
        session = session, inputId = "opt_V1_k", choices = levels(tab()$Level), selected = levels(tab()$Level)[c(1,length(levels(tab()$Level)))],
        options = list(container = "body", 'actions-box' = TRUE, 'deselect-all-text' = "None", 'select-all-text' = "All", 'none-selected-text' = "None selected")
      )
    })

    tab_V1_rows_selected <- shiny::reactiveVal(1)
    shiny::observeEvent(input$tab_V1_rows_selected, {
      tab_V1_rows_selected(input$tab_V1_rows_selected)
    }, ignoreNULL = TRUE)


    output$tab_V1_detail <- DT::renderDT({
      req(tab_flt())
      df <- attr(prepTabV1(tab = tab_flt(), a = tab_V1()[tab_V1_rows_selected(),"Analyte"]), "df")
      DT::datatable(
        data = df, rownames = FALSE, extensions = "Buttons",
        options = list(dom = "Bt", ordering = FALSE, buttons = list(list(extend = "copy", text = "Copy", title = NULL, header = NULL)))
      ) |> DT::formatRound(columns = c("Conc", "Area_norm"), dec.mark = input$opt_tabV1_dec, digits = as.numeric(input$opt_tabV1_precision))
    })

    ab <- shiny::reactive({
      ab <- prepDataV1(tab=tab(), a = input$opt_V1_anal, l = input$opt_V1_k, fmt = "rel_norm")
    })

    fig_V1_width <- shiny::reactive({ calc_bxp_width(n = length(input$opt_V1_anal)*length(input$opt_V1_k), w_point = 28, w_axes = 120) })
    output$fig_V1 <- shiny::renderPlot({
      req(ab())
      prepFigV1(ab = ab())
    }, width = fig_V1_width)

    tab_flt <- shiny::reactive({
      req(tab())
      x <- tab()
      if (input$opt_tabV1_useLevels) {
        req(input$opt_V1_k)
        l_rng <- range(which(levels(x[,"Level"]) %in% input$opt_V1_k))
        l_rng <- seq(min(l_rng), max(l_rng))
        x <- x[as.numeric(x[,"Level"]) %in% l_rng,]
        x[,"Level"] <- factor(x[,"Level"])
      }
      if (input$opt_tabV1_useAnalytes) {
        req(input$opt_V1_anal)
        x <- x[as.character(x[,"Analyte"]) %in% input$opt_V1_anal,]
        x[,"Analyte"] <- factor(x[,"Analyte"])
      }
      return(x)
    })

    output$fig_V2 <- shiny::renderPlot({
      req(tab_flt(), tab_V1())
      prepFigV2(tab = tab_flt(), a = tab_V1()[tab_V1_rows_selected(),"Analyte"], flt_outliers = input$opt_tabV1_fltLevels)
    })

    V2_dat <- reactive({
      req(tab(), tab_V1(), input$opt_V1_k, input$opt_V2_vals)
      # $$ check if prepDataV1 function can be extended to generate the same output as this function
      x <- tab()
      x <- split(x, x$Analyte)[[tab_V1()[tab_V1_rows_selected(),"Analyte"]]]
      x <- split(x, x$Level)[input$opt_V1_k]
      x <- lapply(x, function(y) {
        y[,"Value"] <- NA
        if (input$opt_V2_vals == "Area_Analyte") { y[,"Value"] <- y[,"Area_Analyte"] }
        if (input$opt_V2_vals == "Area_IS") { y[,"Value"] <- y[,"Area_IS"] }
        if (input$opt_V2_vals == "Analyte/IS") { y[,"Value"] <- y[,"Area_Analyte"]/y[,"Area_IS"] }
        if (input$opt_V2_vals == "relative(Analyte/IS)") {
          y[,"Value"] <- y[,"Area_Analyte"]/y[,"Area_IS"]
          y[,"Value"] <- y[,"Value"]/mean(y[,"Value"], na.rm=TRUE)
        }
        return(y)
      })
      return(x)
    })


    output$fig_V3 <- shiny::renderPlot({
      req(V2_dat())
      prepFigV3(x = V2_dat(), fix_ylim = input$opt_V2_vals == "relative(Analyte/IS)")
    })

    output$tab_V2 <- DT::renderDT({
      req(V2_dat())
      x <- V2_dat()
      df <- matrix(NA, nrow = max(sapply(x, nrow)), ncol = length(x), dimnames = list(1:max(sapply(x, nrow)), names(x)))
      for (i in 1:length(x)) df[1:length(x[[i]][,"Value"]),i] <- x[[i]][,"Value"]
      dt <- DT::datatable(
        data = df, rownames = FALSE, extensions = "Buttons",
        options = list(dom = "Bt", ordering = FALSE, buttons = list(list(extend = "copy", text = "Copy to clipboard", title = NULL, header = NULL)))
      )
      dt <- DT::formatCurrency(table = dt, columns = names(x), currency = "", digits = as.numeric(input$opt_tabV1_precision))
      return(dt)
    })

    tab_V1 <- shiny::reactive({
      req(tab_flt())
      #prep_tabV1(tab = tab_flt(), alpha = as.numeric(input$opt_tabV1_alpha), k = as.numeric(input$opt_tabV1_k), flt_outliers = input$opt_tabV1_fltLevels)
      prepTabV1(tab = tab_flt(), alpha = as.numeric(input$opt_tabV1_alpha), k = as.numeric(input$opt_tabV1_k), flt_outliers = input$opt_tabV1_fltLevels)
    })

    output$tab_V1 <- DT::renderDT({
      req(tab_V1(), input$opt_tabV1_k, input$opt_tabV1_alpha, input$opt_tabV1_precision)
      style_tabV1(df = tab_V1(), precision = as.numeric(input$opt_tabV1_precision), selected = tab_V1_rows_selected())
    })

    style_tabV3 <- function(df) {
      DT::datatable(data = df, rownames = FALSE, extensions = "Buttons", options = list(dom = "Bt", pageLength = -1, buttons = list(list(extend = "excel", text = "Excel", title = NULL))))
    }

    output$tab_V3 <- DT::renderDT({
      req(tab())
      style_tabV3(tab())
    })

    # Help section
    shiny::observeEvent(input$InputHelp, {
      show_help("v_dataupload")
    })
    shiny::observeEvent(input$Help_tabV1, {
      show_help("v_tab_V1")
    })
    shiny::observeEvent(input$Help_figV1, {
      show_help("v_fig_V1")
    })

  })
}

