#' @title Validation-Page
#' @description \code{page_validation} is the module for eCerto method validation.
#' @details Not yet.
#' @param id Name when called as a module in a shiny app.
#' @examples
#' if (interactive()) {
#'   shiny::shinyApp(
#'     ui = bslib::page_fluid(
#'       eCerto:::page_validationUI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       #fl <- "C:/Users/jlisec/Documents/Projects/Thomas Sommerfeld/Validierung_Excel/2024_05_22_B003_Arbeitsbereich_neu.xlsx"
#'       fl <- "C:/Users/jlisec/Documents/Projects/Thomas Sommerfeld/Validierung_Excel/2024_06_13_B003_NG-BG.xlsx"
#'       td <- eCerto:::read_Vdata(file = fl)
#'       eCerto:::page_validationServer(id = "test", test_data = td)
#'     }
#'   )
#' }
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
            shinyWidgets::pickerInput(inputId = ns("opt_figV1_anal"), label = "Analyte(s)", multiple = TRUE, choices = ""),
            shinyWidgets::pickerInput(inputId = ns("opt_figV1_level"), label = "Calibration Level(s)", multiple = TRUE, choices = "")
          )
        ),
        shiny::plotOutput(outputId = ns("fig_V1"))
      )
    )
  )

  anal_V_details_card <- bslib::card(
    min_height = "780px",
    bslib::card_header("Working range (details for metabolite selected in Tab.V1)"),
    bslib::card_body(
      fill = TRUE,
      bslib::layout_sidebar(
        padding = 0,
        sidebar = bslib::sidebar(
          position = "right", open = "open", width = "280px",
          shiny::div(
            shinyWidgets::pickerInput(inputId = ns("opt_V2_vals"), label = NULL, multiple = FALSE, choices = c("Area_Analyte","Area_IS","Analyte/IS","relative(Analyte/IS)")),
            #shiny::hr(),
            DT::DTOutput(outputId = ns("tab_V2"))
          )
        ),
        shiny::plotOutput(outputId = ns("fig_V3"))
      )
    )
  )

  tab_V1_card <- bslib::card(
    fill = TRUE,
    bslib::card_header(shiny::actionLink(inputId = ns("Help_tabV1"), "Tab.V1 - Linearity")),
    bslib::card_body(
      fill = TRUE,
      bslib::layout_sidebar(
        padding = 0,
        sidebar = bslib::sidebar(
          position = "right", open = "open", width = "280px",
          shiny::div(
            DT::DTOutput(outputId = ns("tab_V1_detail"))
          )
        ),
        shiny::div(
          shinyWidgets::dropdownButton(
            shiny::checkboxGroupInput(
              inputId = ns("opt_tabV1_colflt"), label = "Column filter",
              choices = list("Linear model"="lm", "Working range"="wr", "LOx"="lo"),
              selected = "lm"
            ),
            label = "Column sets", circle = FALSE, width = "100%", inline = TRUE
          ),
          shinyWidgets::dropdownButton(
            shiny::textInput(inputId = ns("opt_tabV1_unitcali"), label = "unit calibration", placeholder = "ng/mL"),
            shiny::numericInput(inputId = ns("opt_tabV1_convfac"), label = "conversion factor", value = NA),
            shiny::textInput(inputId = ns("opt_tabV1_unitsmpl"), label = "unit samples", placeholder = "mg/kg"),
            label = "Unit specification", circle = FALSE, width = "100%", inline = TRUE
          ),
          shinyWidgets::dropdownButton(
            shinyWidgets::pickerInput(inputId = ns("opt_tabV1_alpha"), label = "alpha", multiple = FALSE, choices = c(0.01, 0.05), selected = 0.05),
            shinyWidgets::pickerInput(inputId = ns("opt_tabV1_k"), label = "k", multiple = FALSE, choices = 2:4, selected = 3),
            shinyWidgets::pickerInput(inputId = ns("opt_tabV1_precision"), label = "digits", multiple = FALSE, choices = 0:6, selected = 3),
            label = "Parameters", circle = FALSE, width = "100%", inline = TRUE
          ),
          shinyWidgets::dropdownButton(
            shiny::checkboxInput(inputId = ns("opt_tabV1_fltLevels"), label = shiny::HTML("Omit Out<sub>F</sub> Levels"), value = FALSE),
            shiny::checkboxInput(inputId = ns("opt_tabV1_useAnalytes"), label = "Analytes Fig.V1", value = FALSE),
            shiny::checkboxInput(inputId = ns("opt_tabV1_useLevels"), label = "Level range Fig.V1", value = FALSE),
            label = "Analyte/Level filters", circle = FALSE, width = "100%", inline = TRUE
          ),
        ),
        shiny::div(
          DT::DTOutput(outputId = ns("tab_V1"))
        )
      )
    )
  )

  fig_V2_card <- bslib::card(
    id = ns("fig_V2_panel"),
    full_screen = TRUE,
    min_height = "960px",
    bslib::card_header("Linearity (details for metabolite selected in Tab.V1)"),
    bslib::card_body(shiny::plotOutput(outputId = ns("fig_V2")))
  )

  tab_V3_card <- bslib::card(
    id = ns("tab_V3_panel"),
    bslib::card_header("Tab.V3 - Imported data (including calculated values and filtering information)"),
    bslib::card_body(DT::DTOutput(outputId = ns("tab_V3")))
  )

  v_report_card <- bslib::card(
    bslib::card_header("Method Validation Report"),
    bslib::card_body(
      bslib::layout_columns(
        shiny::radioButtons(inputId = ns("v_report_fmt"), label = "Validation Report Format", choices = list("HTML"="html", "docx"="docx")),
        shiny::downloadButton(outputId = ns("v_report"), label = "Validation Report")
      )
    )
  )

  placeholder_default <- function(x) {
    paste("This is a placeholder for method", x,  "calculation. You can use any markdown syntax to format the text consistently for HTML or Word export. In general it will be sufficient to know the formatting tags for *italic* and **bold** as well as superscript for ^13^C and subscript like in H~2~O. Some templates can be loaded using the icon in the box header.")
  }

  V_card_trueness <- bslib::card(
    id = ns("v_panel_trueness"),
    bslib::card_header(
      class = "d-flex justify-content-between",
      shiny::actionLink(inputId = ns("Help_trueness"), "Trueness"),
      shinyWidgets::dropdown(
        shiny::selectInput(inputId = ns("opt_trueness_template"), label = "Select template", choices = c("none", "spike")),
        style = "bordered", icon = icon("gear"), right = TRUE, size = "xs", status = "primary"
      )
    ),
    bslib::card_body(
      shiny::textAreaInput(
        inputId = ns("txt_trueness"), label = NULL, rows = 7, width = "100%",
        placeholder = placeholder_default("trueness")
      )
    )
  )

  V_card_precision <- bslib::card(
    id = ns("v_panel_precision"),
    bslib::card_header(shiny::actionLink(inputId = ns("Help_precision"), "Precision")),
    bslib::card_body(
      shiny::textAreaInput(
        inputId = ns("txt_precision"), label = NULL, rows = 7, width = "100%",
        placeholder = placeholder_default("precision")
      )
    )
  )

  V_card_uncertainty <- bslib::card(
    id = ns("v_panel_uncertainty"),
    bslib::card_header(shiny::actionLink(inputId = ns("Help_uncertainty"), "Measurement uncertainty")),
    bslib::card_body(
      shiny::textAreaInput(
        inputId = ns("txt_uncertainty"), label = NULL, rows = 7, width = "100%",
        placeholder = placeholder_default("uncertainty")
      )
    )
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
        bslib::layout_columns(
          V_card_trueness,
          V_card_precision
        ),
        V_card_uncertainty,
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
page_validationServer <- function(id, test_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # Reports ====
    output$v_report <- shiny::downloadHandler(
      filename = function() {
        paste0("Validation Report.", input$v_report_fmt)
      },
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
              output_format = {
                if (input$v_report_fmt=="html") rmarkdown::html_document() else rmarkdown::word_document()
              },
              params = list(
                "inp_data" = tab(),
                "logo_file" = logofile,
                "V_pars" = shiny::reactiveValuesToList(V_pars),
                "helptext_v_fig_V1" = readLines(get_local_file("v_fig_V1.[Rr][Mm][Dd]$")),
                "helptext_v_tab_V1" = readLines(get_local_file("v_tab_V1.[Rr][Mm][Dd]$")),
                "helptext_v_formula_collection" = readLines(get_local_file("v_formula_collection.[Rr][Mm][Dd]$"))
              ),
              envir = new.env(parent = globalenv())
            )
          },
          message = "Rendering Validation Report.."
        )
      }
    )

    # User pars for V module ====
    V_pars <- shiny::reactiveValues(
      "opt_figV1_anal" = "",
      "opt_figV1_level" = "",
      "opt_tabV1_unitcali" = "",
      "opt_tabV1_unitsmpl" = "",
      "opt_tabV1_convfac" = 1,
      "opt_tabV1_colflt" = "",
      "opt_tabV1_precision" = 3,
      "opt_tabV1_alpha" = 0.05,
      "opt_tabV1_k" = 3,
      "opt_tabV1_useAnalytes" = FALSE,
      "opt_tabV1_fltLevels" = FALSE,
      "opt_exp_dec_sep" = ".",
      "txt_trueness" = "",
      "txt_precision" = "",
      "txt_uncertainty" = ""
    )

    shiny::observeEvent(input$opt_tabV1_useAnalytes, {
      V_pars$opt_tabV1_useAnalytes <- input$opt_tabV1_useAnalytes
    })
    shiny::observeEvent(input$opt_tabV1_fltLevels, {
      V_pars$opt_tabV1_fltLevels <- input$opt_tabV1_fltLevels
    })
    shiny::observeEvent(input$opt_figV1_anal, {
      V_pars$opt_figV1_anal <- input$opt_figV1_anal
    })
    shiny::observeEvent(input$opt_figV1_level, {
      V_pars$opt_figV1_level <- input$opt_figV1_level
    })
    shiny::observeEvent(input$opt_tabV1_unitcali, {
      V_pars$opt_tabV1_unitcali <- input$opt_tabV1_unitcali
    })
    shiny::observeEvent(input$opt_tabV1_unitsmpl, {
      V_pars$opt_tabV1_unitsmpl <- input$opt_tabV1_unitsmpl
    })
    shiny::observeEvent(input$opt_tabV1_convfac, {
      V_pars$opt_tabV1_convfac <- input$opt_tabV1_convfac
    })
    shiny::observeEvent(input$opt_tabV1_useLevels, {
      V_pars$opt_tabV1_useLevels <- input$opt_tabV1_useLevels
    })
    shiny::observeEvent(input$opt_tabV1_colflt, {
      V_pars$opt_tabV1_colflt <- input$opt_tabV1_colflt
    })
    shiny::observeEvent(input$opt_tabV1_precision, {
      V_pars$opt_tabV1_precision <- as.numeric(input$opt_tabV1_precision)
    })
    shiny::observeEvent(input$opt_tabV1_alpha, {
      V_pars$opt_tabV1_alpha <- as.numeric(input$opt_tabV1_alpha)
    })
    shiny::observeEvent(input$opt_tabV1_k, {
      V_pars$opt_tabV1_k <- as.numeric(input$opt_tabV1_k)
    })
    shiny::observeEvent(input$txt_trueness, {
      V_pars$txt_trueness <- as.character(input$txt_trueness)
    })
    shiny::observeEvent(input$txt_precision, {
      V_pars$txt_precision <- as.character(input$txt_precision)
    })
    shiny::observeEvent(input$txt_uncertainty, {
      V_pars$txt_uncertainty <- as.character(input$txt_uncertainty)
    })

    trueness_template_spike <- "The trueness was determined by testing the recovery rate **W [%]** at 3 different concentration levels covering the working area range and using *n=6* replicates at each level. We determined **W=...%** which is within the acceptable range of 80..100%."


    shiny::observeEvent(input$opt_trueness_template, {
      if (input$opt_trueness_template=="none") { shiny::updateTextAreaInput(inputId = "txt_trueness", value = "") }
      if (input$opt_trueness_template=="spike") { shiny::updateTextAreaInput(inputId = "txt_trueness", value = trueness_template_spike) }
    })

    # Upload & Data preparation ====
    # upload info used in UI part
    output$V_fileUploaded <- shiny::reactive({
      return(!is.null(input$inp_file$datapath) | !is.null(test_data))
    })
    shiny::outputOptions(output, "V_fileUploaded", suspendWhenHidden = FALSE)

    tab <- shiny::reactive({
      if (!is.null(test_data)) {
        return(test_data)
      } else {
        req(input$inp_file$datapath)
        read_Vdata(file = input$inp_file$datapath, fmt = c("Agilent"))
      }
    })

    shiny::observeEvent(tab(), {
      shinyWidgets::updatePickerInput(
        session = session, inputId = "opt_figV1_anal", choices = levels(tab()$Analyte), selected = levels(tab()$Analyte),
        choicesOpt = list(subtext = paste0("(",1:length(levels(tab()$Analyte)),")")),
        options = list('container' = "body", 'actions-box' = TRUE, 'deselect-all-text' = "None", 'select-all-text' = "All", 'none-selected-text' = "None selected")
      )
      shinyWidgets::updatePickerInput(
        session = session, inputId = "opt_figV1_level", choices = levels(tab()$Level), selected = levels(tab()$Level)[c(1,length(levels(tab()$Level)))],
        options = list(container = "body", 'actions-box' = TRUE, 'deselect-all-text' = "None", 'select-all-text' = "All", 'none-selected-text' = "None selected")
      )
    })

    tab_flt <- shiny::reactive({
      req(tab())
      flt_Vdata(x = tab(), l = if (V_pars$opt_tabV1_useLevels) V_pars$opt_figV1_level else NULL, a = if (V_pars$opt_tabV1_useAnalytes) V_pars$opt_figV1_anal else NULL)
    })

    current_analyte <- shiny::reactiveValues("name" = NULL, "row" = NULL)

    # Tables ====
    # Table V1 ====
    tab_V1 <- shiny::reactive({
      req(tab_flt())
      prepTabV1(
        tab = tab_flt(),
        alpha = V_pars$opt_tabV1_alpha,
        k = V_pars$opt_tabV1_k,
        flt_outliers = V_pars$opt_tabV1_fltLevels,
        unit_cali = V_pars$opt_tabV1_unitcali,
        unit_smpl = V_pars$opt_tabV1_unitsmpl,
        conv_fac = V_pars$opt_tabV1_convfac)
    })

    output$tab_V1 <- DT::renderDT({
      req(tab_V1(), V_pars$opt_tabV1_k, V_pars$opt_tabV1_alpha, V_pars$opt_tabV1_precision)
      a_name <- shiny::isolate(current_analyte$name)
      a_row <- shiny::isolate(current_analyte$row)
      # correct current row of tab V1 in case that analyte filter is applied
      if (!is.null(a_name) && a_name %in% tab_V1()[,"Analyte"] && a_row != which(tab_V1()[,"Analyte"] == a_name)) {
        current_analyte$row <- a_row <- which(tab_V1()[,"Analyte"] == a_name)
      }
      style_tabV1(df = tab_V1(), precision = V_pars$opt_tabV1_precision, selected = a_row, show_colgroups = V_pars$opt_tabV1_colflt)
    })

    shiny::observeEvent(input$tab_V1_rows_selected, {
      i <- input$tab_V1_rows_selected
      current_analyte$row <- i
      current_analyte$name <- tab_V1()[i,"Analyte"]
    }, ignoreNULL = TRUE)

    output$tab_V1_detail <- DT::renderDT({
      req(tab_flt(), current_analyte$name %in% tab_flt()[,"Analyte"])
      df <- attr(prepTabV1(tab = tab_flt(), a = current_analyte$name), "df")
      DT::datatable(
        data = df, rownames = FALSE, extensions = "Buttons",
        options = list(
          dom = 'Bt', ordering = FALSE,
          buttons = list(
            list(extend = "copy", text = '<i class="fa-solid fa-copy"></i>', title = NULL, titleAttr = 'Copy to clipboard', header = NULL),
            list(
              extend = "collection",
              text = '<i class="fa-solid fa-gears"></i>',
                buttons = list(
                text = paste0('set dec sep as "', ifelse(V_pars$opt_exp_dec_sep==".", ",", "."), '"'),
                action = DT::JS(paste0("function ( e, dt, node, config ) { Shiny.setInputValue('", session$ns("opt_exp_dec_sep"), "', 1, {priority: 'event'}); }"))
              )
            )
          )
        )
      ) |> DT::formatRound(columns = c("Conc", "Area_norm"), dec.mark = V_pars$opt_exp_dec_sep, mark = ifelse(V_pars$opt_exp_dec_sep==".", ",", "."), digits = V_pars$opt_tabV1_precision)
    })

    # Table V2 ====
    output$tab_V2 <- DT::renderDT({
      req(V2_dat())
      x <- V2_dat()
      df <- matrix(NA, nrow = max(sapply(x, nrow)), ncol = length(x), dimnames = list(1:max(sapply(x, nrow)), names(x)))
      for (i in 1:length(x)) df[1:length(x[[i]][,"Value"]),i] <- x[[i]][,"Value"]
      dt <- DT::datatable(
        data = df, rownames = FALSE, extensions = "Buttons",
        options = list(
          dom = "Bt", ordering = FALSE,
          buttons = list(
            list(extend = "copy", text = '<i class="fa-solid fa-copy"></i>', title = NULL, titleAttr = 'Copy to clipboard', header = NULL),
            list(
              extend = "collection",
              text = '<i class="fa-solid fa-gears"></i>',
              buttons = list(
                text = paste0('set dec sep as "', ifelse(V_pars$opt_exp_dec_sep==".", ",", "."), '"'),
                action = DT::JS(paste0("function ( e, dt, node, config ) { Shiny.setInputValue('", session$ns("opt_exp_dec_sep"), "', 1, {priority: 'event'}); }"))
              )
            )
          )
        )
      )
      dt <- DT::formatRound(table = dt, columns = names(x), dec.mark = V_pars$opt_exp_dec_sep, mark = ifelse(V_pars$opt_exp_dec_sep==".", ",", "."), digits = V_pars$opt_tabV1_precision)
      return(dt)
    })

    shiny::observeEvent(input$opt_exp_dec_sep, {
      V_pars$opt_exp_dec_sep <- ifelse(V_pars$opt_exp_dec_sep==".", ",", ".")
    })

    # Table V3 ====
    output$tab_V3 <- DT::renderDT({
      req(tab())
      out <- tab()
      # add units from parameter list if provided
      if (nchar(V_pars$opt_tabV1_unitcali) >= 1) {
        out$unit_cali <- V_pars$opt_tabV1_unitcali
      }
      if (is.numeric(V_pars$opt_tabV1_convfac) &&  is.finite(V_pars$opt_tabV1_convfac)) {
        out$conv_fac <- V_pars$opt_tabV1_convfac
        if (nchar(V_pars$opt_tabV1_unitsmpl) >= 1) {
          out$unit_smpl <- V_pars$opt_tabV1_unitsmpl
        }
      }
      DT::datatable(data = out, rownames = FALSE, extensions = "Buttons", options = list(dom = "Bt", pageLength = -1, buttons = list(list(extend = "excel", text = "Excel", title = NULL))))
    })

    # Figures ====
    # Figure V1 ====
    ab <- shiny::reactive({
      req(tab(), any(nchar(V_pars$opt_figV1_anal)>=1), any(nchar(V_pars$opt_figV1_level)>=1))
      prepDataV1(tab=tab(), a = V_pars$opt_figV1_anal, l = V_pars$opt_figV1_level, fmt = "rel_norm")
    })

    fig_V1_width <- shiny::reactive({
      calc_bxp_width(n = length(V_pars$opt_figV1_anal)*length(V_pars$opt_figV1_level), w_point = 28, w_axes = 120)
    })

    output$fig_V1 <- shiny::renderPlot({
      req(ab(), any(nchar(V_pars$opt_figV1_anal)>=1), any(nchar(V_pars$opt_figV1_level)>=1))
      prepFigV1(ab = ab())
    }, width = fig_V1_width)

    # Figure V2 ====
    output$fig_V2 <- shiny::renderPlot({
      req(tab_flt(), current_analyte$name %in% tab_flt()[,"Analyte"])
      prepFigV2(tab = tab_flt(), a = current_analyte$name, flt_outliers = V_pars$opt_tabV1_fltLevels)
    })

    V2_dat <- reactive({
      req(tab(), tab_V1(), V_pars$opt_figV1_level, input$opt_V2_vals, current_analyte$name %in% tab()[,"Analyte"])
      # $$ check if prepDataV1 function can be extended to generate the same output as this function
      x <- tab()
      x <- split(x, x$Analyte)[[current_analyte$name]]
      x <- split(x, x$Level)[V_pars$opt_figV1_level]
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

    # Figure V3 ====
    output$fig_V3 <- shiny::renderPlot({
      req(tab(), current_analyte$name, V_pars$opt_figV1_level)
      prepFigV3(x = flt_Vdata(x = tab(), l = V_pars$opt_figV1_level, a = current_analyte$name, rng = FALSE))
    })

    # Help section ====
    shiny::observeEvent(input$InputHelp, {
      show_help("v_dataupload")
    })
    shiny::observeEvent(input$Help_tabV1, {
      show_help("v_tab_V1")
    })
    shiny::observeEvent(input$Help_figV1, {
      show_help("v_fig_V1")
    })
    shiny::observeEvent(input$Help_trueness, {
      show_help("v_trueness")
    })
    shiny::observeEvent(input$Help_precision, {
      show_help("v_precision")
    })
    shiny::observeEvent(input$Help_uncertainty, {
      show_help("v_uncertainty")
    })

  })
}

