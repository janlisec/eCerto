#' @title Validation-Page DIN 5725-2.
#' @description \code{page_validation57252} is the module for validation according to DIN 5725-2.
#' @details Not yet.
#' @param id Name when called as a module in a shiny app.
#' @param test_data Provide test_data to module.
#' @examples
#' if (interactive()) {
#' testdata <- function() {
#'   # this function returns a dataframe containing Example B1 from DIN ISO 5725-2
#'   # DIN ISO 5725-2
#'   n_p <- 8
#'   n_q <- 4
#'   n_k <- 3
#'   n <- n_p*n_q*n_k
#'   inp <- data.frame(
#'     "Property" = rep("S_content", n),
#'     "Unit" = rep("% (m/m)", n),
#'     "Lab" = rep(1:n_p, each=n_q*n_k),
#'     "Level" = rep(rep(1:n_q, each=n_k), n_p),
#'     "Replicate" = rep(1:n_k, times=n_p*n_q),
#'     "Value" = c(
#'       0.71, 0.71, 0.7, 1.2, 1.18, 1.23, 1.68, 1.7, 1.68, 3.26, 3.26, 3.2,
#'       0.69, 0.67, 0.68, 1.22, 1.21, 1.22, 1.64, 1.64, 1.65, 3.2, 3.2, 3.2,
#'       0.66, 0.65, 0.69, 1.28, 1.31, 1.3, 1.61, 1.61, 1.62, 3.37, 3.36, 3.38,
#'       0.67, 0.65, 0.66, 1.23, 1.18, 1.2, 1.68, 1.66, 1.66, 3.16, 3.22, 3.23,
#'       0.7, 0.69, 0.66, 1.31, 1.22, 1.22, 1.64, 1.67, 1.6, 3.2, 3.19, 3.18,
#'      0.73, 0.74, 0.73, 1.39, 1.36, 1.37, 1.7, 1.73, 1.73, 3.27, 3.31, 3.29,
#'      0.71, 0.71, 0.69, 1.2, 1.26, 1.26, 1.69, 1.7, 1.68, 3.27, 3.24, 3.23,
#'       0.7, 0.65, 0.68, 1.24, 1.22, 1.3, 1.67, 1.68, 1.67, 3.25, 3.26, 3.26
#'     )
#'   )
#'   inp <- rbind(inp, data.frame(
#'     "Property" = rep("S_content", 11),
#'     "Unit" = rep("% (m/m)", 11),
#'     "Lab" = rep(c(1,5), times=c(4,7)),
#'     "Level" = c(1:4,1:4,1,3,4),
#'     "Replicate" = c(4,4,4,4,4,4,4,4,5,5,5),
#'     "Value" = c(
#'       0.71,1.21,1.69,3.24,0.71,1.24,1.66,3.27,0.69,1.68,3.24
#'     )
#'   ))
#'   return(inp)
#' }
#'   shiny::shinyApp(
#'     ui = bslib::page_fluid(
#'       eCerto:::page_validation57252UI(id = "test")
#'     ),
#'     server = function(input, output, session) {
#'       #fl <- "C:/Users/jlisec/Documents/Projects/Thomas Sommerfeld/Validierung_Excel/2024_05_22_B003_Arbeitsbereich_neu.xlsx"
#'       #fl <- "C:/Users/jlisec/Documents/Projects/Thomas Sommerfeld/Validierung_Excel/2024_06_13_B003_NG-BG.xlsx"
#'       #fl <- "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/Testdaten/JS1/BDE47_oberere Kali2_JL.xlsx"
#'       #fl <- system.file("extdata", "eCerto_Testdata_VModule.xlsx", package = "eCerto")
#'       #td <- eCerto:::read_Vdata(file = fl, fmt = eCerto:::check_fmt_Vdata(fl))
#'       td <- testdata()
#'       #td <- openxlsx::read.xlsx(xlsxFile = "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/Validierung/DIN5725-2/testdata.xlsx", sheet = 1)
#'       #td <- NULL
#'       eCerto:::page_validation57252Server(id = "test", test_data = td)
#'     }
#'   )
#' }
#' @return Nothing
#' @noRd

page_validation57252UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::conditionalPanel(
      condition = "output.V_fileUploaded == false",
      ns = ns, # namespace of current module
      shiny::fileInput(
        inputId = ns("inp_file"),
        label = shiny::actionLink(inputId = ns("InputHelp"), "Import Excel/RData File"),
        multiple = FALSE,
        placeholder = "xlsx | Rdata",
        accept = c("xlsx", "RData")
      ),
      shiny::div(
        shiny::p(shiny::helpText("Example Table (generic format)")),
        shiny::uiOutput(outputId = ns("example_table_generic"))
      )
    ),
    shiny::conditionalPanel(
      condition = "output.V_fileUploaded == true",
      ns = ns, # namespace of current module
      bslib::card(
        id = ns("v2_test"),
        fillable = FALSE,
        bslib::card_header("DIN 5725-2 output panel"),
        bslib::card_body(
          fill = FALSE,
          bslib::layout_sidebar(
            sidebar = bslib::sidebar(
              position = "left", open = "open", width = "280px",
              shiny::div(id = ns("ori_inp_file_name"), "This div will show the original Excel File name used upon import."),
              shiny::numericInput(inputId = ns("opt_tab_precision"), value = 3, step = 1, min = 0, max = 6, label = "digits precision"),
              shiny::uiOutput(ns("TabV0")),
            ),
            shiny::uiOutput(ns("TabV1")),
            shiny::plotOutput(ns("FigV1")),
            shiny::uiOutput(ns("TabV2")),
            bslib::layout_columns(
              shiny::plotOutput(ns("FigV2a")),
              shiny::plotOutput(ns("FigV2b"))
            ),
            shiny::uiOutput(ns("TabV3")),
            bslib::layout_column_wrap(
              widths = "450px",
              shiny::plotOutput(ns("FigV3a"), width = "450px", height = "450px"),
              shiny::plotOutput(ns("FigV3b"), width = "450px", height = "450px")
            )
          )
        )
      )
    )
  )

}

#' @noRd
page_validation57252Server <- function(id, test_data = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # DIN ISO 5725-2 R-Script
    # non reactive functions
    endsub <- function(s) {
      sub <- sub("^.*?_(.+)$", "\\1", s)
      main <- sub("_.+$", "", s)
      if (identical(main, s)) return(s)
      as.expression(substitute(MAIN[SUB], list(MAIN = as.name(main), SUB  = as.name(sub))))
    }

    markdown2HTML <- function(x) {
      stopifnot(is.character(x))
      x <- gsub("\\*\\*(.+?)\\*\\*", "<b>\\1</b>", x, perl = TRUE) # bold first to avoid conflicts with italic
      x <- gsub("\\*(.+?)\\*", "<i>\\1</i>", x, perl = TRUE) # Italic
      x <- gsub("~(.+?)~", "<sub>\\1</sub>", x, perl = TRUE) # Subscript
      x <- gsub("\\^(.+?)\\^", "<sup>\\1</sup>", x, perl = TRUE) # Superscript
      return(x)
    }

    markdown2expression <- function(x) {
      stopifnot(is.character(x))
      s <- x[[1]]

      #-----------------------------------------
      # Tokenizer: erzeugt Text- und Markup-Tokens
      #-----------------------------------------
      tokenize <- function(s) {
        tokens <- list()
        i <- 1
        n <- nchar(s)
        getc <- function(i) substr(s, i, i)

        buffer <- ""

        flush <- function() {
          if (nzchar(buffer)) {
            tokens[[length(tokens)+1]] <<- list(type="text", value=buffer)
            buffer <<- ""
          }
        }

        while (i <= n) {
          ch <- getc(i)

          # bold **
          if (ch=="*" && i<n && getc(i+1)=="*") {
            flush()
            j <- i+2
            while (j<n && !(getc(j)=="*" && getc(j+1)=="*")) j <- j+1
            inner <- substr(s, i+2, j-1)
            tokens[[length(tokens)+1]] <- list(type="bold", value=tokenize(inner))
            i <- j+2
            next
          }

          # italic *
          if (ch=="*") {
            flush()
            j <- i+1
            while (j<=n && getc(j)!="*") j <- j+1
            inner <- substr(s, i+1, j-1)
            tokens[[length(tokens)+1]] <- list(type="italic", value=tokenize(inner))
            i <- j+1
            next
          }

          # superscript x^y^
          if (ch=="^") {
            flush()
            if (length(tokens) == 0) stop("Superscript without base")
            base_tok <- tokens[[length(tokens)]]
            tokens <- tokens[-length(tokens)]
            i <- i + 1
            start <- i
            while (i <= n && getc(i) != "^") i <- i + 1
            sup <- substr(s, start, i-1)
            tokens[[length(tokens)+1]] <- list(
              type="sup",
              base=base_tok,
              value=tokenize(sup)
            )
            i <- i + 1
            next
          }

          # subscript x~y~
          if (ch=="~") {
            flush()
            if (length(tokens) == 0) stop("Subscript without base")
            base_tok <- tokens[[length(tokens)]]
            tokens <- tokens[-length(tokens)]
            i <- i + 1
            start <- i
            while (i <= n && getc(i) != "~") i <- i + 1
            subtxt <- substr(s, start, i-1)
            tokens[[length(tokens)+1]] <- list(
              type="sub",
              base=base_tok,
              value=tokenize(subtxt)
            )
            i <- i + 1
            next
          }

          # normal text
          buffer <- paste0(buffer, ch)
          i <- i+1
        }

        flush()
        tokens
      }

      format_text_token <- function(txt) {
        # Einzelbuchstabe → Symbol
        if (grepl("^[A-Za-z]$", txt)) return(txt)
        # Reine Zahl → Zahl
        if (grepl("^[0-9]+$", txt)) return(txt)
        # Ansonsten Text literal
        paste0("'", txt, "'")
      }

      #-----------------------------------------
      # Renderer: erzeugt Plotmath-Ausdruck
      #-----------------------------------------
      render <- function(tokens) {
        parts <- sapply(tokens, function(tok) {
          if (tok$type=="text") {
            return(format_text_token(tok$value))
          }
          if (tok$type=="bold") {
            return(paste0("bold(", render(tok$value), ")"))
          }
          if (tok$type=="italic") {
            return(paste0("italic(", render(tok$value), ")"))
          }
          if (tok$type=="sup") {
            return(paste0(render(list(tok$base)), "^", render(tok$value)))
          }
          if (tok$type=="sub") {
            return(paste0(render(list(tok$base)), "[", render(tok$value), "]"))
          }
        })
        paste(parts, collapse="*", sep="")
      }

      tokens <- tokenize(s)
      expr_string <- render(tokens)
      parse(text = expr_string)[[1]]
    }

    convert_fmt <- function(x, output = c("markdown", "html", "expression")) {
      output <- match.arg(output)
      if (output == "markdown") return(x)
      if (output == "html")     return(markdown2HTML(x))
      if (output == "expression") return(markdown2expression(x))
    }

    # calculate Mandels statistic h and k
    mandel_h <- function(xbar, w = NULL) {
      # Vergleicht den (gewichteten)  Labor‑Mittelwert mit dem Gesamtmittelwert einer Ringuntersuchung.
      # xbar: numeric vector of laboratory means
      # w: weights, the number of replicates per lab, has to be numeric of length(xbar)
      if (is.null(w)) w <- rep(1, length(xbar))
      stopifnot(length(xbar) == length(w))
      # weighted grand mean
      grand_mean <- sum(w * xbar) / sum(w)
      # between-lab SD (DIN 5725-2)
      sL <- sqrt(sum(w * (xbar - grand_mean)^2) / (length(xbar) - 1))
      h <- (xbar - grand_mean) / sL
      return(h)
    }
    qmandel_h <- function(p, alpha = 0.05) {
      ((p - 1)/sqrt(p)) * (2 * qbeta((1-alpha/2), (p - 2)/2, (p - 2)/2, lower.tail = TRUE, log.p = FALSE) - 1)
    }
    mandel_k <- function(s) {
      # Vergleicht die Labor‑Standardabweichung mit der durchschnittlichen Standardabweichung aller Labore.
      # s: numeric vector of laboratory standard deviations
      mean_s <- mean(s)
      k <- s / mean_s
      return(k)
    }
    qmandel_k <- function(k, p, alpha = 0.05) {
      sqrt(p * qbeta((1-alpha), (k - 1)/2, (p - 1) * (k - 1)/2, lower.tail = TRUE, log.p = FALSE))
    }

    # tabulate original data like in DIN Annex B
    prepTabV0 <- function(inp, ...) {
      nms_level <- unique(inp[,"Level"])
      tab0 <- eCerto:::ldply_base(unique(inp[,"Lab"]), function(p) {
        x <- inp[inp[,"Lab"]==p,]
        eCerto:::ldply_base(unique(sort(x[,"Replicate"])), function(k) {
          y <- x[x[,"Replicate"]==k,]
          out <- data.frame(t((setNames(rep(NA, length(nms_level)), nms_level))), check.names = FALSE)
          out[,y[,"Level"]] <- y[,"Value"]
          cbind("Lab"=p, "Rep"=k, out)
        })
      })
      ft <- eCerto:::ft_default(df = tab0, ...)
      ft <- flextable::vline(x = ft, j = 1:ncol(tab0), part = "all")
      ft <- flextable::hline(x = ft, i = (which(!duplicated(tab0[,"Lab"]))-1)[-1])
      ft <- flextable::align(x = ft, j = 1:2, align = "center")
      ft <- flextable::border_outer(x = ft)
      return(ft)
    }

    # table of means and sds
    prepTabV1 <- function(mns, prec = 3, ...) {
      n_p <- length(unique(mns[,"Lab"]))
      n_q <- length(unique(mns[,"Level"]))
      mns_print <- as.data.frame(matrix(NA, nrow = n_p, ncol = 3*n_q, dimnames = list(unique(sort(mns[,"Lab"])), paste0(c("y<sub>i", "s<sub>i", "n<sub>i"), rep(1:n_q, each=3), "</sub>"))))
      for (p in unique(sort(mns[,"Lab"]))) {
        for (q in unique(sort(mns[,"Level"]))) {
          j <- which(unique(sort(mns[,"Level"]))==q)
          mns_print[p,3*j-c(2,1,0)] <- mns[mns[,"Lab"]==p & mns[,"Level"]==q,c("mean","sd","n")]
        }
      }
      mns_print <- cbind("Lab <i>i</i>"=rownames(mns_print), mns_print)
      ft <- eCerto:::ft_default(df = mns_print, ...)
      ft <- flextable::add_header_row(x = ft, values = c("", 1:n_q), colwidths = c(1,rep(3,n_q)))
      ft <- flextable::compose(x = ft, i = 1, j = 1, value = flextable::as_paragraph("Level ", flextable::as_i("j")), part = "header")
      ft <- eCerto:::ft_set_formatter(ft = ft, j_idx = which(substr(ft$col_keys,1,1)%in%c("y","s")), fmt = eCerto:::ft_formatter_fixed_digits, digits = prec)
      ft <- flextable::align(x = ft, i = 1, align = "center", part = "header")
      ft <- flextable::align(x = ft, j = 1, align = "center", part = "all")
      ft <- flextable::vline(x = ft, j = 1+c(0,cumsum(rep(3,n_q))))
      ft <- flextable::border_outer(x = ft)
      return(ft)
    }

    # Grubbs-Test for means and Cochran-Test for sds
    prepTabV2 <- function(inp, q=1, prec=3, fmt="alpha", ...) {
      if (is.numeric(q) && !(q %in% inp[,"Level"])) q <- unique(inp[,"Level"])[q]
      tmp <- inp[inp[,"Level"]==q, c("Lab","Value"), drop=FALSE]
      colnames(tmp) <- gsub("Value", "value", colnames(tmp))
      mns <- V_calc_stats(inp)
      out <- cbind(mns[mns[,"Level"]==q,], eCerto:::Grubbs(lab_means = mns[mns[,"Level"]==q, "mean", drop=FALSE], fmt=fmt), eCerto:::Cochran(data = tmp, fmt=fmt))
      out <- out[order(out[,"mean"]),]
      colnames(out) <- gsub("1$", "<sub>1</sub>", colnames(out))
      colnames(out) <- gsub("2$", "<sub>2</sub>", colnames(out))
      colnames(out) <- gsub("_h$", "<i> h</i>", colnames(out))
      colnames(out) <- gsub("_k$", "<i> k</i>", colnames(out))
      ft <- eCerto:::ft_default(df = out, ...)
      ft <- flextable::align(x = ft, j = 1:2, align = "center")
      ft <- eCerto:::ft_set_formatter(ft = ft, j_idx = which(colnames(out) %in% c("mean", "sd")), fmt = eCerto:::ft_formatter_fixed_digits, digits = prec)
      ft <- eCerto:::ft_set_formatter(ft = ft, j_idx = grep("Mandel", colnames(out)), fmt = eCerto:::ft_formatter_fixed_digits, digits = 4)
      return(ft)
    }

    # tabulate relevant repeatability values
    prepTabV3 <- function(mns) {
      n_q <- unique(mns[,"Level"])
      eCerto:::ldply_base(n_q, function(q) {
        x <- mns[mns[,"Level"]==q,]
        x_n <- sum(is.finite(x[,"mean"]))
        x_mn <- sum(x[,"n"]*x[,"mean"])/sum(x[,"n"])
        s_r <- sqrt(sum((x[,"n"]-1) * x[,"sd"]^2)/sum(x[,"n"]-1))
        #x_S <- sum((x[,"mean"] - x_mn)^2)/(x_n-1)
        #s_L <- x_S-(s_r/mean(x[,"n"]))
        T1 <- sum(x[,"n"]*x[,"mean"])
        T2 <- sum(x[,"n"]*x[,"mean"]^2)
        T3 <- sum(x[,"n"])
        T4 <- sum(x[,"n"]^2)
        s_L <- sqrt(((T2 * T3 - T1^2)/(T3 * (x_n-1)) - s_r^2)*((T3 * (x_n-1))/(T3^2 - T4)))
        s_R <- sqrt(s_r^2 + s_L^2)
        data.frame(
          "p_j" = x_n,
          "m_j" = x_mn,
          # weighted mean of Lab sd's
          "s_rj" = s_r,
          "s_Rj" = s_R,
          check.names = FALSE
        )
      })
    }
    styleTabV3 <- function(x, prec=3, ...) {
      colnames(x) <- gsub("p_j$", "p<sub>j</sub>", colnames(x))
      colnames(x) <- gsub("m_j$", "m<sub>j</sub>", colnames(x))
      colnames(x) <- gsub("s_rj$", "s<sub>rj</sub>", colnames(x))
      colnames(x) <- gsub("s_Rj$", "s<sub>Rj</sub>", colnames(x))
      ft <- eCerto:::ft_default(df = x, ...)
      ft <- eCerto:::ft_set_formatter(ft = ft, j_idx = 2:4, fmt = eCerto:::ft_formatter_fixed_digits, digits = prec)
      return(ft)
    }

    # calculation of means and sds as well as Mandel tests
    V_calc_stats <- function(inp) {
      mns <- eCerto:::ldply_base(unique(sort(inp[,"Lab"])), function(p) {
        eCerto:::ldply_base(unique(sort(inp[,"Level"])), function(q) {
          x <- inp[inp[,"Lab"]==p & inp[,"Level"]==q,"Value"]
          data.frame("Lab"=p, "Level"=q, "mean"=mean(x, na.rm=TRUE), "sd"=sd(x, na.rm=TRUE), "n"=sum(is.finite(x)))
        })
      })
      mns[,"Mandel_h"] <- NA
      mns[,"Mandel_k"] <- NA
      for (q in unique(sort(mns[,"Level"]))) {
        flt <- mns[,"Level"]==q
        mns[flt,"Mandel_h"] <- mandel_h(xbar = mns[flt,"mean"], w = mns[flt,"n"])
        mns[flt,"Mandel_k"] <- mandel_k(s = mns[flt,"sd"])
      }
      return(mns)
    }

    # plot of raw data per level and lab
    plotV1 <- function(inp) {
      opar <- par(no.readonly = TRUE)
      n_q <- length(unique(inp[,"Level"]))
      n_p <- length(unique(inp[,"Lab"]))
      par(mfrow=c(1, n_q))
      par(mar=c(3,4,3,0)+0.5)
      for (q in 1:n_q) {
        flt <- inp[,"Level"]==unique(inp[,"Level"])[q]
        y_num <- as.numeric(factor(inp[flt,"Lab"], levels=unique(inp[,"Lab"])))
        plot(x = inp[flt,"Value"], y = y_num, type="n", ylim = rev(range(y_num)), las=1, main=unique(inp[,"Level"])[q], ylab=ifelse(q==1, "Lab", ""), xlab="")
        abline(v = mean(inp[flt,"Value"], na.rm=TRUE), lwd=2)
        for (p in 1:n_p) {
          flt2 <- flt & inp[,"Lab"]==unique(inp[,"Lab"])[p]
          x <- inp[flt2, "Value"]
          y <- rep(p, length(x))
          y[duplicated(x)] <- y[duplicated(x)] + 0.2*c(-1,1,-2,2)[sum(duplicated(x))]
          points(y = y, x = x, pch = c(21:25)[inp[flt2,"Replicate"]], bg = c(2:6)[inp[flt2,"Replicate"]], cex = 2)
        }
      }
      par(opar)
    }

    # Mandel h plot
    plotV2 <- function(mns, type = c("h", "k")) {
      type <- match.arg(type)
      n_p <- length(unique(mns[,"Lab"]))
      n_q <- length(unique(mns[,"Level"]))
      n_k <- floor(median(mns[,"n"]))
      if (type == "h") {
        m_crit <- qmandel_h(p = n_p, alpha = c(0.01, 0.05))
        idx <- "Mandel_h"
        ylab <- expression("Mandel's statistic, " * italic(h))
        fac <- c(-1,-1,1,1)
      } else {
        m_crit <- qmandel_k(k = n_k, p = n_p, alpha = c(0.01, 0.05))
        idx <- "Mandel_k"
        ylab <- expression("Mandel's statistic, " * italic(k))
        fac <- 1
      }
      plot(x = c(0, n_p*n_q*1.2+0.2), y = range(c(0, mns[,idx], fac*m_crit)), type="n", axes=F, ylab=ylab, xlab="", xaxs="i")
      abline(h = fac*m_crit, lty = 2, col = grey(0.8))
      barplot(mns[,idx] ~ interaction(mns[,"Level"],mns[,"Lab"]), las=2, col=c(grey(0.4), grey(0.8))[rep(rep(1:2, each = n_q), length.out=n_q*n_p)], add=TRUE, axisnames=FALSE)
      box()
    }

    # plot repeatability values and fit data
    plotV3 <- function(df) {
      x <- df[,1]
      y <- df[,2]
      xlab <- endsub(colnames(df)[1])
      ylab <- endsub(colnames(df)[2])
      par(mar=c(5,3,0,0)+0.5)
      plot(x=x, y=y, xlim=range(c(0,max(x)+0.05*max(x))), ylim=c(0, max(y)), xaxs="i", type="n", xlab=xlab, ylab=ylab)
      # fit linear model with intercept
      abline(lm(y~x), col = "blue", lwd=2)
      # fit linear model without intercept
      abline(lm(y~x+0), col = "lightblue", lwd=2)
      # fit log model with intercept
      fit_lm <- lm(log(y) ~ x)
      a <- exp(coef(fit_lm)[1])
      b <- coef(fit_lm)[2]
      xx <- seq(min(x), max(x), length.out = 200)
      yy <- a * exp(b * xx)
      lines(xx, yy, col = "orange", lwd = 2)
      points(x=x, y=y, pch=21, bg=grey(0.8), cex=1.5)
    }



    # User pars for V2 module ====
    V2_pars <- shiny::reactiveValues(
      "opt_tab_precision" = 3,
      "inp_file_path" = "",
      "ori_inp_file_name" = "",
      "par_update" = 0
    )

    shiny::observeEvent(input$opt_tab_precision, {
      V2_pars$opt_tab_precision <- as.numeric(input$opt_tab_precision)
    })

    # generic input table example
    output$example_table_generic <- renderUI({
      x <- read_Vdata(file = system.file(package = "eCerto", "extdata", "eCerto_Testdata_VModule.xlsx"))[1:23,2:8]
      ft <- show_upload_example_table(x=x, max_char = 15, optional = c(1,2))
      flextable::htmltools_value(ft, ft.align = "left")
    })

    # Upload & Data preparation ====
    # upload info used in UI part
    output$V_fileUploaded <- shiny::reactive({
      return(!is.null(input$inp_file$datapath) | !is.null(test_data))
    })
    shiny::outputOptions(output, "V_fileUploaded", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$inp_file$datapath, {
      shinyjs::html(id = "ori_inp_file_name", html = shiny::HTML(input$inp_file$name))
      # keep name of XLSX file
      if (tolower(tools::file_ext(input$inp_file$name)) == "xlsx") V2_pars$ori_inp_file_name <- input$inp_file$name
      V2_pars$inp_file_path <- normalizePath(input$inp_file$datapath)
    })

    # Reactives ====
    inp <- shiny::reactive({
      if (!is.null(test_data)) {
        return(test_data)
      } else {
        req(V2_pars$inp_file_path)
        x <- V2_pars$inp_file_path
        if (tolower(tools::file_ext(x)) %in% c("rdata", "rda")) {
          v_env <- new.env()
          load(file = x, envir = v_env)
          shiny::isolate(V2_pars$par_update <- V2_pars$par_update + 1)
          get("eCerto_V_backup", envir = v_env)[["tab"]]
        } else {
          # fmt <- check_fmt_Vdata(file = x)
          # read_Vdata(file = x, fmt = fmt)
          openxlsx::read.xlsx(xlsxFile = x, sheet = 1)
        }
      }
    })

    mns <- shiny::reactive({
      req(inp())
      V_calc_stats(inp = inp())
    })

    res <- shiny::reactive({
      req(mns())
      prepTabV3(mns = mns())
    })

    # Tables ====
    output$TabV0 <- shiny::renderUI({
      req(inp())
      ft <- prepTabV0(inp = inp(), id = "Tab.V0", caption = "Input data, grouped per cell")
      flextable::htmltools_value(ft, ft.align = "left")
    })

    output$TabV1 <- shiny::renderUI({
      req(mns(), V2_pars$opt_tab_precision)
      ft <- prepTabV1(mns = mns(), prec = V2_pars$opt_tab_precision, id = "Tab.V1", caption = "Cell means, standard deviations and number of finite measurement replicates per cell")
      flextable::htmltools_value(ft, ft.align = "left")
    })

    output$TabV2 <- shiny::renderUI({
      req(mns(), V2_pars$opt_tab_precision)
      n_q <- length(unique(mns()[,"Level"]))
      fts <- lapply(1:n_q, function(q) {
        ft <- prepTabV2(inp = inp(), q = q, prec = V2_pars$opt_tab_precision, id = paste0("Tab.V2", letters[q]), caption = paste("Statistic values for Level", q))
        flextable::htmltools_value(ft, ft.align = "left")
      })
      bslib::layout_columns(!!!fts)
    })

    output$TabV3 <- shiny::renderUI({
      req(res(), V2_pars$opt_tab_precision)
      ft <- styleTabV3(x = res(), prec = V2_pars$opt_tab_precision, id = "Tab.V3", caption = "Calculated repeatability values")
      flextable::htmltools_value(ft, ft.align = "left")
    })

    # Figures ====
    output$FigV1 <- renderPlotHD({
      req(inp())
      plotV1(inp = inp())
    })

    output$FigV2a <- renderPlotHD({
      req(mns())
      plotV2(mns(), type = "h")
    })

    output$FigV2b <- renderPlotHD({
      req(mns())
      plotV2(mns(), type = "k")
    })

    output$FigV3a <- renderPlotHD({
      req(res())
      plotV3(res()[,c(2,3)])
    })

    output$FigV3b <- renderPlotHD({
      req(res())
      plotV3(res()[,c(2,4)])
    })

  })
}
