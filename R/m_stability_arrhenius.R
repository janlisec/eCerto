#' @title m_stability_arrhenius.
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv The whole R6 object.
#'
#' @return a reactive indicating that the user wants to switch back from arrhenius to simple view of S modul.
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    m_arrheniusUI(id = "arrhenius")
#'  ),
#'  server = function(input, output, session) {
#'  rv <- eCerto:::test_rv()
#'  out <- m_arrheniusServer(id = "arrhenius", rv = rv)
#'  shiny::observeEvent(out$switch, { print(out$switch) })
#'  }
#' )
#' }
#'
#' @rdname m_arrhenius
#' @export
#'
m_arrheniusUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 10,
        shiny::div(
          style="width=100%; margin-bottom: 5px;",
          shiny::strong(
            shiny::actionLink(
              inputId = ns("ArrheniusPlot1_link"),
              label = "Fig.1 Determining temperature-dependent reaction rates"
            )
          )
        ),
        shiny::plotOutput(outputId = ns("Fig1"))
      ),
      shiny::column(2, shiny::wellPanel(
        shiny::selectInput(inputId = ns("analyte"), label = "analyte", choices = ""),
        sub_header("Change View"),
        shiny::actionButton(inputId = ns("s_switch_simple"), label = "Switch to linear model"),
        shiny::p(),
        shiny::checkboxGroupInput(
          inputId = ns("s_opt_Fig1"),
          label = "Options Fig.1",
          choices = list(
            "Show Ref Data" = "show_reference_point",
            "Use ordinal time" = "plot_nominal_scale",
            "Time in month" = "plot_in_month",
            "log-tansform values" = "plot_ln_relative"
          ),
          selected = c("show_reference_point", "plot_nominal_scale", "plot_in_month", "plot_ln_relative")
        )
      ))
    ),
    shiny::fluidRow(
      shiny::column(
        width = 10,
        shiny::div(
          style="width=100%; margin-bottom: 5px;",
          shiny::strong(
            shiny::actionLink(
              inputId = ns("ArrheniusTab_link"),
              label = "Tab.1 Calculation of possible storage time"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(width = 6, DT::DTOutput(outputId = ns("Tab1"))),
          shiny::column(width = 4, DT::DTOutput(outputId = ns("Tab1exp"))),
          shiny::column(width = 2, DT::DTOutput(outputId = ns("outTab")))
        ),
        DT::DTOutput(outputId = ns("Tab2"))
      ),
      shiny::column(
        width = 2,
        shiny::div(
          style="width=100%; margin-bottom: 5px;",
          shiny::strong(
            shiny::actionLink(
              inputId = ns("ArrheniusPlot2_link"),
              label = "Fig.2 Arrhenius Model"
            )
          )
        ),
        shiny::plotOutput(outputId = ns("Fig2")))
    )
  )
}

#' @rdname m_arrhenius
#' @export
m_arrheniusServer <- function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    silent <- get_golem_config("silent")

    prec <- 6

    #ns <- shiny::NS(id)

    out <- shiny::reactiveValues("switch"=0)

    # # use err_txt to provide error messages to the user
    err_txt <- shiny::reactiveVal(NULL)
    shiny::observeEvent(err_txt(), {
      shinyalert::shinyalert(text = err_txt(), type = "info")
      err_txt(NULL)
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$s_switch_simple, {
      out$switch <- out$switch+1
    }, ignoreInit = TRUE)

    shiny::observeEvent(getValue(rv, c("Stability", "data")), {
      shiny::updateSelectInput(session = session, inputId = "analyte", choices = unique(as.character(getValue(rv, c("Stability", "data"))[,"analyte"])))
    })

    df <- shiny::reactive({
      shiny::req(input$analyte)
      dat <- getValue(rv, c("Stability", "data"))
      req_col <- c("analyte","time","Value","Temp")
      shiny::validate(shiny::need(req_col %in% colnames(dat), message = paste("These columns required for Arrhenius calculations are not available:", paste(req_col[!(req_col %in% colnames(dat))], collapse=", "))))
      shiny::validate(shiny::need(input$analyte %in% as.character(dat[,"analyte"]), message="How did you manage to specify a non existent analyte name?"))
      tmp <- dat[as.character(dat[,"analyte"])==input$analyte,]
      # normalize data to mean of t=0
      flt <- is.finite(tmp[,"Value"]) & tmp[,"Value"]>0
      if (!all(flt)) {
        err_txt(paste("Did filter the follwing values:", paste(tmp[!flt,"Value"], collapse=", ")))
        tmp <- tmp[flt,]
      }
      tmp[,"Value"] <- tmp[,"Value"]/mean(tmp[tmp[,"time"]==0, "Value"], na.rm=TRUE)
      return(tmp)
    })

    # plot relative data over time and temp
    getFig1 <- function(tmp, show_reference_point = TRUE, plot_nominal_scale = TRUE, plot_in_month = TRUE, plot_ln_relative = TRUE) {
      time <- tmp[,"time"]
      val <- tmp[,"Value"]
      if (plot_in_month) time <- round(time*12/365,2)
      if (plot_nominal_scale) time <- factor(time)
      if (plot_ln_relative) val <- log(val)
      tf <- factor(tmp[,"Temp"])
      pchs <- c(21:25,21:23)[as.numeric(tf)]
      cols <- c(1:8)[as.numeric(tf)]
      mns <- tapply(val, list(tmp[,"Temp"], time), mean, na.rm=TRUE)
      sds <- tapply(val, list(tmp[,"Temp"], time), stats::sd, na.rm=TRUE)
      xlim <- range(as.numeric(time), na.rm=TRUE)
      ylim <- range(c(mns-sds, mns+sds, val), na.rm=TRUE)
      ylim <- ifelse(plot_ln_relative, 0, 1)+c(-1,1)*max(abs(ylim-ifelse(plot_ln_relative, 0, 1)))
      cex_plot <- 1.5
      graphics::par(mar=c(5.5,4.5,1,1))
      graphics::par(mfrow=c(1,length(levels(tf))-1))
      graphics::par(cex.lab=cex_plot*1.1, cex.axis=cex_plot*1.1)
      for (k in levels(tf)[-1]) {
        plot(xlim, ylim, xlab=ifelse(plot_in_month, "Month", "Days"), ylab=ifelse(plot_ln_relative, "log(Relative value)", "Relative value"), type="n", main="", axes=FALSE)
        graphics::mtext(text = paste0(k, "\u00B0C"), side = 1, line = -1.8, adj = 0.98, cex = cex_plot)
        graphics::axis(2)
        graphics::abline(h=ifelse(plot_ln_relative, 0, 1), col=grDevices::grey(0.9), lwd=3)
        flt <- time==0
        if (show_reference_point) {
          graphics::points(y=val[flt], x=time[flt], pch=21, bg=grDevices::grey(0.9), cex=2)
          graphics::abline(h=mean(val[flt], na.rm=TRUE)+c(-1,1)*stats::sd(val[flt], na.rm=TRUE), col=grDevices::grey(0.9), lwd=1, lty=2)
        }
        if (plot_nominal_scale) {
          tmp.x <- 1:length(levels(factor(time)))
          graphics::axis(1, at=tmp.x, labels = levels(factor(time)))
        } else {
          tmp.x <- as.numeric(levels(factor(time)))
          graphics::axis(1)
        }
        graphics::box()
        graphics::lines(x=tmp.x, y=mns[k,]-sds[k,], col=unique(cols[tf==k]), lwd=1, lty=2)
        graphics::lines(x=tmp.x, y=mns[k,], col=unique(cols[tf==k]), lwd=3)
        graphics::lines(x=tmp.x, y=mns[k,]+sds[k,], col=unique(cols[tf==k]), lwd=1, lty=2)
        flt <- tmp[,"Temp"]==k
        graphics::points(y=val[flt], x=time[flt], pch=pchs[flt], bg=cols[flt], cex=2)
        if (!plot_ln_relative) {
          graphics::mtext(text = paste0("recovery = ", round(100*mean(val[flt], na.rm=T),1), "%"), side = 3, line = -1.8, adj = 0.02, cex = cex_plot)
          graphics::mtext(text = paste0("(RSD = ", round(100*stats::sd(val[flt], na.rm=T)/mean(val[flt], na.rm=T),1), "%)"), side = 3, line = -3.6, adj = 0.02, cex = cex_plot)
        }
        if (plot_ln_relative & plot_in_month) {
          lm_res <- stats::coef(stats::lm(val[flt] ~ as.numeric(as.character(time[flt]))))
          graphics::mtext(text = paste("slope =", round(lm_res[2],4)), side = 3, line = -1.8, adj = 0.98, col=ifelse(lm_res[2]<0,3,2), cex = cex_plot)
        }
      }
      graphics::par(mfrow=c(1,1))
    }

    output$Fig1 <- shiny::renderPlot({
      #message("[m_arrheniusServer] Fig1")
      getFig1(
        tmp=df(),
        show_reference_point = "show_reference_point" %in% input$s_opt_Fig1,
        plot_nominal_scale = "plot_nominal_scale" %in% input$s_opt_Fig1,
        plot_in_month = "plot_in_month" %in% input$s_opt_Fig1,
        plot_ln_relative = "plot_ln_relative" %in% input$s_opt_Fig1
      )
    })


    # generate Tab1
    getTab1 <- function(tmp) {
      tf <- factor(tmp[,"Temp"])
      time <- round(tmp[,"time"]*12/365,2)
      val <- log(tmp[,"Value"])
      out <- plyr::ldply(levels(tf)[-1], function(k) {
        flt <- tmp[,"Temp"]==k
        a <- stats::coef(stats::lm(val[flt] ~ time[flt]))[2]
        return(data.frame(
          "T [\u00B0C]"=k,
          "Rec"=paste0(round(100*mean(tmp[flt,"Value"], na.rm=T),1), "%"),
          "RSD"=paste0(round(100*stats::sd(tmp[flt,"Value"], na.rm=T)/mean(tmp[flt,"Value"], na.rm=T),1), "%"),
          "1/K"=round(1/(273.15+as.numeric(k)),5),
          "k_eff"=a,
          "log(-k_eff)"=ifelse(a<0, log(-a), NA),
          check.names=FALSE))
      })
      return(out)
    }
    tab1 <- shiny::reactive({ getTab1(tmp=df()) })
    output$Tab1 <- DT::renderDT({
      out <- tab1()
      for (i in which(colnames(out) %in% c("k_eff", "log(-k_eff)"))) out[,i] <- round(out[,i], prec)
      return(out)
    }, options = list(dom="t"), rownames = FALSE)

    getTab2 <- function(tab1) {
      shiny::validate(shiny::need(sum(tab1()[,"k_eff"]<0)>=3, message="Need at least 3 negative reaction constants 'k_eff' to establish linear model."))
      s <- sum(tab1[,"1/K"])
      s2 <- sum(tab1[,"1/K"]^2)
      n <- nrow(tab1)
      se <- steyx(x = tab1[,"1/K"], y = tab1[,"log(-k_eff)"])
      out <- data.frame(
        "sum_x"=s,
        "sum_x2"=s2,
        "n"=n,
        "steyx"=se,
        "u(i)"=sqrt(se^2*s2/(s2*n-s^2)),
        "u(s)"=sqrt(se^2*n/(s2*n-s^2)),
        "cov"=-1*(se^2*s/(s2*n-s^2)),
        check.names=FALSE
      )
      return(out)
    }
    tab2 <- shiny::reactive({
      shiny::req(tab1())
      getTab2(tab1=tab1())
    })
    output$Tab2 <- DT::renderDT({
      out <- tab2()
      for (i in which(colnames(out) %in% c("steyx", "u(i)", "u(s)", "cov"))) out[,i] <- round(out[,i], prec)
      return(out)
    }, options = list(dom="t"), rownames = FALSE)

    expTab1 <- function(tab1, tab2) {
      ce <- stats::coef(stats::lm(tab1[,"log(-k_eff)"] ~ tab1[,"1/K"]))
      a <- ce[2]
      b <- ce[1]
      out <- tab1
      out[,"log(k)_calc"] <- a*tab1[,"1/K"] + b
      out[,"CI_upper"] <- sqrt(tab2[,"u(i)"]^2 + tab2[,"u(s)"]^2*tab1[,"1/K"]^2 + 2*tab2[,"cov"]*tab1[,"1/K"]) + out[,"log(k)_calc"]
      out[,"CI_lower"] <- 2 * out[,"log(k)_calc"] - out[,"CI_upper"]
      return(out)
    }
    tab1exp <- shiny::reactive({
      shiny::req(tab1(), tab2())
      expTab1(tab1=tab1(), tab2=tab2())
    })
    output$Tab1exp <- DT::renderDT({
      out <- tab1exp()[,-c(1:6)]
      for (i in 1:ncol(out)) out[,i] <- round(out[,i], prec)
      return(out)
    }, options = list(dom="t"), rownames = FALSE)

    output$outTab <- DT::renderDT({
      req(tab1exp())
      mt <- getValue(rv, c("General", "materialtabelle"))
      validate(need(expr = mt, message = "An existing material table is needed to extract the certified value used in subsequent calculations."))
      l <- which(mt[,"analyte"]==input$analyte)
      validate(need(expr = length(l)==1, message = paste("Could not find analyte", input$analyte, "within column 'analyte' of current material table.")))
      cert_val <- mt[l,"cert_val"]
      U_abs <- mt[l,"U_abs"]
      coef <- log((cert_val-U_abs)/cert_val)
      validate(need(expr = is.finite(coef), message = "cert_val and sd do not yield a finite value"))
      out <- tab1exp()
      out[,"month"] <- round(coef/(-1*exp(out[,"CI_upper"])))
      return(out[,c(1,10)])
    }, options = list(dom="t"), rownames = FALSE)


    getFig2 <- function(tab) {
      xlim <- range(tab[,"1/K"], na.rm=TRUE)
      ylim <- range(c(tab[,"log(-k_eff)"], tab[,"CI_upper"], tab[,"CI_lower"]), na.rm=TRUE)
      graphics::par(mar=c(5,4,2.5,1))
      plot(xlim, ylim, xlab="1/K", ylab="log(-k_eff)", type="n", main="")
      graphics::axis(side = 3, at = 1/(273.15+as.numeric(tab[,1])), labels = tab[,1])
      graphics::lines(x=tab[,"1/K"], y=tab[,"CI_upper"], col=2, lwd=1, lty=2)
      graphics::lines(x=tab[,"1/K"], y=tab[,"log(k)_calc"], col=2, lwd=3)
      graphics::lines(x=tab[,"1/K"], y=tab[,"CI_lower"], col=2, lwd=1, lty=2)
      graphics::points(y=tab[,"log(-k_eff)"], x=tab[,"1/K"], pch=21, bg=4, cex=2)
    }
    output$Fig2 <- shiny::renderPlot({
      shiny::req(tab1exp())
      getFig2(tab=tab1exp())
    })

    shiny::observeEvent(input$ArrheniusPlot1_link, {
      help_the_user_modal("stability_arrhenius_fig1")
    })

    shiny::observeEvent(input$ArrheniusTab_link, {
      help_the_user_modal("stability_arrhenius_tab1")
    })

    shiny::observeEvent(input$ArrheniusPlot2_link, {
      help_the_user_modal("stability_arrhenius_fig2")
    })

    return(out)

  })

}
