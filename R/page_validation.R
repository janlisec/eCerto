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
              shinyWidgets::pickerInput(inputId = ns("opt_tabV1_precision"), label = "digits", multiple = FALSE, choices = 0:6, selected = 3)
            ),
            shinyWidgets::pickerInput(inputId = ns("opt_tabV1_k"), label = "k", multiple = FALSE, choices = 2:4, selected = 3),
            shiny::hr(),
            shiny::checkboxInput(inputId = ns("opt_tabV1_fltLevels"), label = shiny::HTML("Omit Out<sub>F</sub> Levels"), value = FALSE),
            shiny::checkboxInput(inputId = ns("opt_tabV1_useAnalytes"), label = "Use Analytes from Fig.V1", value = TRUE),
            shiny::checkboxInput(inputId = ns("opt_tabV1_useLevels"), label = "Use Level range from Fig.V1", value = FALSE),
            shiny::hr(),
            bslib::layout_columns(
              shinyWidgets::pickerInput(inputId = ns("opt_tabV1_dec"), label = "sep", multiple = FALSE, choices = c(",", "."), selected = ","),
              shiny::actionButton(inputId = ns("opt_tabV1_datamodal"), label = shiny::HTML("Show<br>data"))
            )
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
                "tab_V1" = style_tabV1(df = tab_V1(), precision = as.numeric(input$opt_tabV1_precision)),
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

    shiny::observeEvent(input$InputHelp, {
      show_help("v_dataupload")
    })
    shiny::observeEvent(input$Help_tabV1, {
      show_help("v_tab_V1")
    })
    shiny::observeEvent(input$Help_figV1, {
      show_help("v_fig_V1")
    })

    # fill all empty values of a vector with the last valid value
    auto_fill <- function(x) {
      stopifnot(length(x)>=2)
      stopifnot(!is.na(x[1]))
      idx <- is.na(x)
      if (any(idx)) {
        for (i in which(idx)) x[i] <- x[i-1]
      }
      return(x)
    }

    data_import <- function(file = NULL, fmt = c("Agilent")) {
      tab <- openxlsx::read.xlsx(xlsxFile = file, sheet = 1, startRow = 2, check.names = FALSE)
      # check/prepare main table
      stopifnot(all(c("Name","Type","Level") %in% colnames(tab)))
      tab_main <- tab[,c("Name","Type","Level")]
      tab_main[,"Type"] <- "Cal"
      stopifnot(!is.na(tab_main[1,"Level"]))
      tab_main[,"Level"] <- auto_fill(tab_main[,"Level"])
      # check/prepare abalyte table
      stopifnot(all(c("Exp..Conc.","Area") %in% colnames(tab)))
      tab_anal <- tab[,min(grep("Exp..Conc.", colnames(tab))):ncol(tab)]
      n <- length(grep("Exp..Conc.", colnames(tab_anal)))
      n_cols <- unique(diff(grep("Exp..Conc.", colnames(tab_anal))))
      stopifnot(length(n_cols)==1)
      stopifnot(n_cols %in% c(2,3))
      # get analyte names
      tab_hd <- unname(unlist(openxlsx::read.xlsx(xlsxFile = file, sheet = 1, rows = 1, colNames = FALSE)))[-1]
      a_names <- gsub(" Method", "", tab_hd[((1:n)-1)*n_cols+1])
      tab_out <- plyr::ldply(1:n, function(i) {
        tmp <- tab_anal[,(i-1)*n_cols+1:n_cols]
        colnames(tmp)[1:2] <- c("Concentration", "Area_Analyte")
        tmp[,1] <- auto_fill(tmp[,1])
        if (n_cols==2) tmp <- cbind(tmp, "Area_IS"=NA) else colnames(tmp)[3] <- "Area_IS"
        out <- cbind(tab_main, "Analyte"=a_names[i], tmp)
        return(out)
      })
      tab_out[,"Analyte"] <- factor(tab_out[,"Analyte"], levels=unique(tab_out[,"Analyte"]))
      tab_out[,"Level"] <- factor(tab_out[,"Level"])
      return(tab_out)
    }

    prep_data <- function(tab = NULL, a = NULL, k = NULL, fmt = c("raw", "norm", "rel_norm")) {
      fmt <- match.arg(fmt)
      stopifnot(all(c("Analyte","Area_Analyte","Area_IS","Level") %in% colnames(tab)))
      if (is.null(a)) {
        a <- switch(
          fmt,
          "raw" = levels(tab[,"Analyte"])[1],
          "norm" = levels(tab[,"Analyte"])[1],
          "rel_norm" = levels(tab[,"Analyte"])
        )
      }
      stopifnot(all(a %in% levels(tab[,"Analyte"])))
      if (is.null(k)) {
        k <- switch(
          fmt,
          "raw" = levels(tab[,"Level"]),
          "norm" = levels(tab[,"Level"]),
          "rel_norm" = {
            k <- levels(tab[,"Level"])
            k[c(1,length(k))]
          }
        )
      }
      stopifnot(all(k %in% tab[,"Level"]))
      tab_analyte <- split(tab, tab[,"Analyte"])
      out <- lapply(tab_analyte[a], function(x) {
        tab_level <- split(x, x[,"Level"])
        lapply(tab_level[k], function (y) {
          switch(
            fmt,
            "raw" = y[,"Area_Analyte"],
            "norm" = y[,"Area_Analyte"]/y[,"Area_IS"],
            "rel_norm" = {
              ratio <- y[,"Area_Analyte"]/y[,"Area_IS"]
              ratio/mean(ratio, na.rm=TRUE)
            }
          )
        })
      })
      out <- unlist(out, recursive = FALSE)
      attr(out, "Analyte") <- factor(rep(a, each=length(k)), levels=levels(tab[,"Analyte"]))
      attr(out, "Level") <- factor(rep(k, times=length(a)), levels=levels(tab[,"Level"]))
      attr(out, "Concentration") <- as.vector(sapply(tab_analyte[a], function(x) { sapply(split(x, x[,"Level"])[k], function(y) { unique(y[,"Concentration"]) }) }))
      return(out)
    }



    tab_linearity <- function(tab = NULL, a = NULL, alpha = 0.05, k = 3, flt_outliers = FALSE) {
      tmp <- prep_data(tab = tab, a = a, fmt = "norm")
      #conc <- rep(attr(tmp, "Concentration"), times = sapply(tmp, length))
      df <- data.frame("Conc"=attr(tmp, "Concentration"), "Area_norm"=sapply(tmp, mean, na.rm=TRUE), row.names = 1:length(tmp))
      # you can export this df for cross checking with DINTest
      #write.table(df, file = "clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)
      df.lm <- lm(Area_norm ~ Conc, data = df)
      # F-test for outliers (checking highest residual)
      idx <- F_test_outlier(df.lm, alpha = alpha)
      if (!is.na(idx)) {
        check_more <- TRUE
        while (check_more) {
          new_F_Test <- F_test_outlier(lm(Area_norm ~ Conc, data=df[-idx, ]), alpha = alpha)
          if (!is.na(new_F_Test)) {
            idx <- c(idx, as.numeric(rownames(df)[-idx][new_F_Test]))
            if (length(idx)>=(nrow(df)-3)) check_more <- FALSE
          } else {
            check_more <- FALSE
          }
        }
        F_Test <- paste(idx, collapse = ", ")
        if (flt_outliers) {
          df <- df[-idx, ]
          df.lm <- lm(Area_norm ~ Conc, data = df)
          F_Test <- paste0("(", F_Test, ")")
        }
      } else {
        F_Test <- idx
      }

      e <- residuals(df.lm)
      N <- length(e)
      s_yx <- sqrt(sum(e^2)/(N-2))
      s_x0 <- s_yx/coef(df.lm)[2]
      V_x0 <- 100*(s_x0/mean(df[,1], na.rm=T))

      # Mandel test
      df2 <- cbind(df, "Conc2" = df$Conc^2)
      df.qm <- lm(Area_norm ~ Conc + Conc2, data=df2)
      e.qm <- residuals(df.qm)
      P_Mandel <- MandelTest(res_lm = e, res_qm = e.qm)


      n <- min(sapply(tmp, length))
      #write.table(data.frame(e), file = "clipboard", sep = "\t", row.names = FALSE, col.names = FALSE)
      ng <- calc_LOD(x = df$Conc, y = df$Area_norm, alpha = alpha, n = n)
      #if (a=="PFBA") browser()
      out <- data.frame(
        "Analyte" = levels(factor(attr(tmp, "Analyte"))),
        "N" = N,
        "n" = n,
        "alpha" = 0.05,
        "k" = round(1/k, 2),
        "b0" = coef(df.lm)[1],
        "b1" = coef(df.lm)[2],
        "P_KS_Res" = ks.test(x = e, y="pnorm", mean=mean(e), sd=sd(e))$p.val,
        "P_Neu_Res" = VonNeumannTest(e, unbiased = FALSE)$p.val,
        "F_Test" = F_Test,
        "LOD" = ng,
        "LOQ" = calc_LOQ(x = df$Conc, y = df$Area_norm, alpha = 0.05, n = n, k = k),
        "s_yx" = s_yx,
        "s_x0" = s_x0,
        "V_x0" = V_x0,
        "P_Mandel" = P_Mandel
      )
      attr(out, "df") <- df
      attr(out, "residuals") <- e
      return(out)
    }

    prep_tabV1 <- function(tab, alpha, k, flt_outliers) {
      plyr::ldply(levels(tab[,"Analyte"]), function(a) {
        tab_linearity(tab = tab, a = a, alpha = alpha, k = k, flt_outliers = flt_outliers)
      })
    }
    style_tabV1 <- function(df, precision) {
      colnames(df) <- gsub("^P_KS_Res$", "P<sub>KS,Res</sub>", colnames(df))
      colnames(df) <- gsub("^P_Neu_Res$", "P<sub>Neu,Res</sub>", colnames(df))
      colnames(df) <- gsub("^P_Mandel$", "P<sub>Mandel</sub>", colnames(df))
      colnames(df) <- gsub("^F_Test$", "Out<sub>F</sub>", colnames(df))
      colnames(df) <- gsub("^s_yx$", "s<sub>y,x</sub>", colnames(df))
      colnames(df) <- gsub("^s_x0$", "s<sub>x0</sub>", colnames(df))
      colnames(df) <- gsub("^V_x0$", "V<sub>x0</sub>", colnames(df))
      colnames(df) <- gsub("^b0$", "b<sub>0</sub>", colnames(df))
      colnames(df) <- gsub("^b1$", "b<sub>1</sub>", colnames(df))
      dt <- DT::datatable(data = df, options = list(dom="t", pageLength = -1), rownames = FALSE, escape = FALSE, selection = list(mode = "single", selected = tab_V1_rows_selected(), target = 'row'))
      round_cols <- c("b<sub>0</sub>", "b<sub>1</sub>", "P<sub>KS,Res</sub>", "P<sub>Neu,Res</sub>", "P<sub>Mandel</sub>", "LOD", "LOQ", "s<sub>y,x</sub>", "s<sub>x0</sub>", "V<sub>x0</sub>")
      dt <- DT::formatCurrency(table = dt, columns = round_cols, currency = "", digits = precision)
      pval_cols <- c("P<sub>KS,Res</sub>", "P<sub>Neu,Res</sub>", "P<sub>Mandel</sub>")
      dt <- DT::formatStyle(
        table = dt,
        columns = pval_cols,
        target = "cell",
        color = DT::styleInterval(cuts = c(0.01, 0.05), values = c("red", "orange", "")),
        fontWeight = DT::styleInterval(cuts = c(0.01, 0.05), values = c("bold", "normal", "normal"))
      )
      return(dt)
    }

    fig_linearity <- function(tab = NULL, a = NULL, alpha = 0.05, k = 3, flt_outliers = flt_outliers) {
      opar <- par(no.readonly = TRUE)
      on.exit(par(opar))
      cex <- 1.5
      vals <- tab_linearity(tab = tab, a = a, alpha = alpha, k = k, flt_outliers = flt_outliers)
      df <- attr(vals, "df")
      e <- attr(vals, "residuals")
      L <- NULL
      if (flt_outliers) { L <- levels(tab[,"Level"])[as.numeric(rownames(df))] }
      tmp <- prep_data(tab = tab, a = a, k = L, fmt = "norm")
      conc <- rep(attr(tmp, "Concentration"), times = sapply(tmp, length))
      # compute quadratic model
      dfq <- cbind(df, "Conc2" = df$Conc^2)
      qm <- lm(Area_norm ~ Conc + Conc2, data=dfq)
      e.qm <- residuals(qm)
      res_rng <- range(c(e, e.qm))
      layout(mat = matrix(1:6, ncol=2), heights = c(0.45,0.2,0.35))
      par(cex=cex)
      # linear model
      par(mar=c(5,4,3,2)+0.1)
      plot(x = conc, y = unlist(tmp), xlab = "Concentration", ylab = "Area/Area_IS", main = a)
      abline(b = vals[,"b1"], a = vals[,"b0"], col = 3)
      points(df, pch=4, col=2, cex=2)
      mtext(text = expression(y==b[0]+b[1]*x), side = 3, line = -1.2, at = 0, adj = 0, cex = cex)
      mtext(text = bquote(b[0]==.(round(vals[,"b0"], 4))), side = 3, line = -2.4, at = 0, adj = 0, cex = cex)
      mtext(text = bquote(b[1]==.(round(vals[,"b1"], 4))), side = 3, line = -3.6, at = 0, adj = 0, cex = cex)
      mtext(text = paste("r =", round(cor((lm(Area_norm ~ Conc, data=df))$fitted.values, df$Area_norm), 4)), side = 3, line = -4.8, at = 0, adj = 0, cex = cex)
      par(mar=c(3,4,0,2)+0.1)
      tmp.x <- barplot(unname(e), ylim = res_rng, ann=F)
      flt <- e>=0
      text(x = tmp.x[flt], y = 0, labels = names(e)[flt], srt=90, adj=1.1, cex = 2/3)
      text(x = tmp.x[!flt], y = 0, labels = names(e)[!flt], srt=90, adj=-0.1, cex = 2/3)
      mtext(text = "Residuals", side = 2, line = 3, cex = cex)
      i <- 1+max(which(df[,1]<=vals[,"LOQ"]))
      par(mar=c(5,4,0,2)+0.1)
      plot(x = conc, y = unlist(tmp), xlab = "Concentration", ylab = "Area/Area_IS", xlim = c(0, df[i,1]), ylim = c(0, df[i,2]))
      abline(b = vals[,"b1"], a = vals[,"b0"], col = 3)
      points(df, pch=4, col=2, cex=2)

      # quadratic model
      par(mar=c(5,4,3,2)+0.1)
      plot(x = conc, y = unlist(tmp), xlab = "Concentration", ylab = "Area/Area_IS", main = a)
      new_conc <- seq(min(c(0, min(conc))), max(conc), length.out=100)
      area_predicted <- predict(object = qm, list(Conc = new_conc, Conc2 = new_conc^2))
      lines(x = new_conc, y = area_predicted, col = 3)
      points(df, pch=4, col=2, cex=2)
      mtext(text = expression(y==b[0]+b[1]*x+b[2]*x^2), side = 3, line = -1.2, at = 0, adj = 0, cex = cex)
      mtext(text = bquote(b[0]==.(round(coef(qm)[1], 4))), side = 3, line = -2.4, at = 0, adj = 0, cex = cex)
      mtext(text = bquote(b[1]==.(round(coef(qm)[2], 4))), side = 3, line = -3.6, at = 0, adj = 0, cex = cex)
      mtext(text = bquote(b[2]==.(round(coef(qm)[3], 4))), side = 3, line = -4.8, at = 0, adj = 0, cex = cex)
      mtext(text = paste("r =", round(cor(qm$fitted.values, dfq$Area_norm), 4)), side = 3, line = -6, at = 0, adj = 0, cex = cex)
      par(mar=c(3,4,0,2)+0.1)
      tmp.x <- barplot(unname(e.qm), ylim = res_rng, ann=F)
      flt <- e.qm>=0
      text(x = tmp.x[flt], y = 0, labels = names(e.qm)[flt], srt=90, adj=1.1, cex = 2/3)
      text(x = tmp.x[!flt], y = 0, labels = names(e.qm)[!flt], srt=90, adj=-0.1, cex = 2/3)
      mtext(text = "Residuals", side = 2, line = 3, cex = cex)
      i <- 1+max(which(df[,1]<=vals[,"LOQ"]))
      par(mar=c(5,4,0,2)+0.1)
      plot(x = conc, y = unlist(tmp), xlab = "Concentration", ylab = "Area/Area_IS", xlim = c(0, df[i,1]), ylim = c(0, df[i,2]))
      lines(x = new_conc, y = area_predicted, col = 3)
      points(df, pch=4, col=2, cex=2)
      invisible(NULL)
    }


    tab <- shiny::reactive({
      # if (file.exists("C:/Users/jlisec/Documents/Projects/Thomas Sommerfeld/Validierung_Excel/2024_05_22_B003_Arbeitsbereich_neu.xlsx")) {
      #   data_import(file = "C:/Users/jlisec/Documents/Projects/Thomas Sommerfeld/Validierung_Excel/2024_05_22_B003_Arbeitsbereich_neu.xlsx", fmt = c("Agilent"))
      # } else {
        req(input$inp_file$datapath)
        data_import(file = input$inp_file$datapath, fmt = c("Agilent"))
      # }
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


    shiny::observeEvent(input$opt_tabV1_datamodal, {
      shiny::showModal(
        shiny::modalDialog(
          shiny::tagList(
            shiny::h4("You can copy/paste this table into DINTEST"),
            DT::renderDT({
              req(tab())
              df <- attr(tab_linearity(tab = tab(), a = tab_V1()[tab_V1_rows_selected(),"Analyte"]), "df")
              if (input$opt_tabV1_dec == ",") df <- apply(df, 2, function(x) { gsub("[.]", ",", x) })
              DT::datatable(
                data = df, rownames = FALSE, extensions = "Buttons",
                options = list(
                  dom = "Bt",
                  ordering = FALSE,
                  buttons = list(
                    list(
                      extend = "copy",
                      text = "Copy to clipboard",
                      title = NULL,
                      header = NULL
                    )
                  )
                )
              )
            })
          )
        )
      )
    }, ignoreInit = TRUE)

    ab <- shiny::reactive({
      ab <- prep_data(tab=tab(), a = input$opt_V1_anal, k = input$opt_V1_k, fmt = "rel_norm")
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
      fig_linearity(tab = tab_flt(), a = tab_V1()[tab_V1_rows_selected(),"Analyte"], flt_outliers = input$opt_tabV1_fltLevels)
    })

    V2_dat <- reactive({
      req(tab(), tab_V1(), input$opt_V1_k, input$opt_V2_vals)
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

    fig_V3 <- function(x, fix_ylim = FALSE) {
      opar <- par(no.readonly = TRUE)
      on.exit(par(opar))
      cex <- 1.5
      par(mfrow=c(1, length(x)))
      par(mar=c(5,4,2,0)+0.1)
      par(cex=1.5)
      if (fix_ylim) ylim <- range(lapply(x, function(y) {y[,"Value"]}), na.rm=TRUE) else ylim <- NULL
      for (i in 1:length(x)) {
        plot(x[[i]][,"Value"], ylab=ifelse(i==1, paste0(as.character(x[[i]][1,"Analyte"]), " (", as.numeric(x[[i]][1,"Analyte"]), ")"), ""), xlab="Replicate", main="", ylim=ylim)
        mtext(text = paste("Level =", names(x)[i]), side = 3, line = 0.2, adj = 0, cex = cex)
      }
    }

    output$fig_V3 <- shiny::renderPlot({
      req(V2_dat())
      fig_V3(x = V2_dat(), fix_ylim = input$opt_V2_vals == "relative(Analyte/IS)")
    })

    output$tab_V2 <- DT::renderDT({
      req(V2_dat())
      x <- V2_dat()
      df <- matrix(NA, nrow = max(sapply(x, nrow)), ncol = length(x), dimnames = list(1:max(sapply(x, nrow)), names(x)))
      for (i in 1:length(x)) df[1:length(x[[i]][,"Value"]),i] <- x[[i]][,"Value"]
      dt <- DT::datatable(
        data = df, rownames = FALSE, extensions = "Buttons",
        options = list(
          dom = "Bt",
          ordering = FALSE,
          buttons = list(
            list(
              extend = "copy",
              text = "Copy to clipboard",
              title = NULL,
              header = NULL
            )
          )
        )
      )
      dt <- DT::formatCurrency(table = dt, columns = names(x), currency = "", digits = as.numeric(input$opt_tabV1_precision))
      return(dt)
    })

    tab_V1 <- shiny::reactive({
      req(tab_flt())
      prep_tabV1(tab = tab_flt(), alpha = as.numeric(input$opt_tabV1_alpha), k = as.numeric(input$opt_tabV1_k), flt_outliers = input$opt_tabV1_fltLevels)
    })

    output$tab_V1 <- DT::renderDT({
      req(tab_V1(), input$opt_tabV1_k, input$opt_tabV1_alpha, input$opt_tabV1_precision)
      style_tabV1(df = tab_V1(), precision = as.numeric(input$opt_tabV1_precision))
    })

    style_tabV3 <- function(df) {
      DT::datatable(data = df, rownames = FALSE, extensions = "Buttons", options = list(dom = "Bt", pageLength = -1, buttons = list(list(extend = "excel", text = "Excel", title = NULL))))
    }

    output$tab_V3 <- DT::renderDT({
      req(tab())
      style_tabV3(tab())
    })

  }
  )
}

