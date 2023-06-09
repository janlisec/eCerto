#' @title m_stability_arrhenius.
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv The whole R6 object.
#'
#' @return A reactive indicating that the user wants to switch back
#'    from arrhenius to simple view of S modul.
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(
#'    eCerto:::m_arrheniusUI(id = "arrhenius")
#'  ),
#'  server = function(input, output, session) {
#'    rv <- eCerto:::test_rv(type = "SR3")
#'    shiny::isolate(eCerto::setValue(rv, c("Stability", "data"), eCerto:::test_Stability_Arrhenius()))
#'  #rv <- eCerto$new(init_rv())
#'  #x <- eCerto:::test_Stability_Arrhenius()
#'  #isolate(setValue(rv, c("Stability", "data"), x))
#'  out <- eCerto:::m_arrheniusServer(id = "arrhenius", rv = rv)
#'  shiny::observeEvent(out$switch, { print(out$switch) })
#'  }
#' )
#' }
#'
#' @noRd
#' @keywords internal
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
              label = "Fig.S2 - determining temperature-dependent reaction rates"
            )
          )
        ),
        shiny::plotOutput(outputId = ns("FigS2"))
      ),
      shiny::column(
        width = 2,
        shiny::wellPanel(
          shiny::selectInput(inputId = ns("analyte"), label = "analyte", choices = ""),
          shiny::selectInput(inputId = ns("flt_ids"), label = "exclude ids", choices = "", multiple = TRUE),
          sub_header("Options Fig.S2", b=12),
          shiny::checkboxGroupInput(
            inputId = ns("s_opt_FigS2"),
            label = NULL,
            choices = list(
              "Show Ref Data" = "show_reference_point",
              "Use ordinal time" = "plot_nominal_scale",
              "Time in month" = "plot_in_month",
              "log-tansform values" = "plot_ln_relative",
              "Round Month Time" = "round_time",
              "Show sample IDs" = "show_ids"
            ),
            selected = c("show_reference_point", "plot_nominal_scale", "plot_in_month", "plot_ln_relative")
          ),
          shiny::actionButton(inputId = ns("s_switch_simple"), label = "Switch to linear model")
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 8,
        shiny::div(
          style="width=100%; margin-bottom: 5px;",
          sub_header(shiny::actionLink(inputId = ns("ArrheniusTab_link"), label = "Tab.S2 - calculation of possible storage time"))
        ),
        shiny::fluidRow(
          shiny::column(width = 6, DT::DTOutput(outputId = ns("Tab1"))),
          shiny::column(width = 4, DT::DTOutput(outputId = ns("Tab1exp"))),
          shiny::column(width = 2, DT::DTOutput(outputId = ns("outTab")))
        ),
        shiny::fluidRow(
          shiny::column(width = 10, DT::DTOutput(outputId = ns("Tab2"))),
          shiny::column(width = 2, shiny::uiOutput(outputId = ns("user_month")))
        )
      ),
      shiny::column(
        width = 2,
        shiny::div(
          style="width=100%; margin-bottom: 5px;",
          sub_header(shiny::actionLink(inputId = ns("ArrheniusPlot2_link"), label = "Fig.S3 - Arrhenius model"))
        ),
        shiny::plotOutput(outputId = ns("Fig2"))
      ),
      shiny::column(
        width = 2,
        shiny::wellPanel(
          shiny::fluidRow(
            shiny::column(
              width = 6,
              sub_header(shiny::actionLink(inputId = ns("ArrheniusStorrageTemp_link"), label = "Potential Storage Temp")),
              shiny::numericInput(inputId = ns("user_temp"), label = NULL, value = -20, min = -80, max = 23, step = 1)
            ),
            shiny::column(
              width = 6,
              shiny::radioButtons(inputId = ns("rbtn_storage"), label = "Use values from...", choices = list("Tab.C3" = "mt", "Reference Temp" = "rt", "input-box below" = "inp"), selected = "rt"),
              shiny::numericInput(inputId = ns("num_coef"), label = NULL, value = NULL)
            )
          )
        )
      )
    )
  )
}

#' @noRd
#' @keywords internal
m_arrheniusServer <- function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    silent <- get_golem_config("silent")

    prec <- 6

    out <- shiny::reactiveValues("switch"=0)

    # use err_txt to provide error messages to the user
    err_txt <- shiny::reactiveVal(NULL)
    shiny::observeEvent(err_txt(), {
      shinyalert::shinyalert(text = err_txt(), type = "info")
      err_txt(NULL)
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$s_switch_simple, {
      out$switch <- out$switch+1
    }, ignoreInit = TRUE)

    an <- reactiveVal()
    shiny::observeEvent(getValue(rv, c("Stability", "data")), {
      x <- getValue(rv, c("Stability", "data"))
      if (!is.factor(x[,"analyte"])) x[,"analyte"] <- factor(x[,"analyte"])
      an(levels(x[,"analyte"]))
      shiny::updateSelectInput(session = session, inputId = "analyte", choices = an())
      shiny::updateSelectInput(session = session, inputId = "flt_ids", choices = 1:nrow(x))
    })

    shiny::observeEvent(rv$cur_an, {
      req(input$analyte, an())
      if (!identical(input$analyte, rv$cur_an) && rv$cur_an %in% an()) shiny::updateSelectInput(session = session, inputId = "analyte", choices = an(), selected = rv$cur_an)
      if (!is.null(input$flt_ids)) shiny::updateSelectInput(session = session, inputId = "flt_ids", selected = NULL)
    }, ignoreNULL = TRUE)

    shiny::observeEvent(input$analyte, {
      if (is.null(rv$cur_an) | !identical(rv$cur_an, input$analyte)) rv$cur_an <- input$analyte
    }, ignoreNULL = TRUE, ignoreInit = TRUE)


    df <- shiny::reactive({
      shiny::req(input$analyte)
      dat <- getValue(rv, c("Stability", "data"))
      # [ToDo JL] the filtering step should be initiated during upload and user selections should be saved
      rownames(dat) <- 1:nrow(dat)
      if (length(input$flt_ids)>=1) {
        dat <- dat[-as.numeric(input$flt_ids),]
      }
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

    output$FigS2 <- shiny::renderPlot({
      req(df())
      prepFigS2(
        tmp=df(),
        show_reference_point = "show_reference_point" %in% input$s_opt_FigS2,
        plot_nominal_scale = "plot_nominal_scale" %in% input$s_opt_FigS2,
        plot_in_month = "plot_in_month" %in% input$s_opt_FigS2,
        plot_ln_relative = "plot_ln_relative" %in% input$s_opt_FigS2,
        round_time = "round_time" %in% input$s_opt_FigS2,
        show_ids = "show_ids" %in% input$s_opt_FigS2
      )
    })

    # generate Tab1
    getTab1 <- function(tmp) {
      tf <- factor(tmp[,"Temp"])
      if ("round_time" %in% input$s_opt_FigS2) {
        # the version for compatibility with Bremser (round to 1/4 month precision)
        time <- tmp[,"time"]*12/365
        time <- round(round(4*time)/4,2)
      } else {
        # the day wise precise version
        time <- round(tmp[,"time"]*12/365,2)
      }
      val <- log(tmp[,"Value"])
      out <- plyr::ldply(levels(tf)[-1], function(k) {
        # the linear model shall include the reference data
        flt <- tmp[,"Temp"]==k | tmp[,"Temp"]==levels(tf)[1]
        a <- stats::coef(stats::lm(val[flt] ~ time[flt]))[2]
        # Rec and RSD are calculated without reference data
        flt <- tmp[,"Temp"]==k
        return(data.frame(
          "dummy"=k,
          "Rec"=paste0(round(100*mean(tmp[flt,"Value"], na.rm=T),1), "%"),
          "RSD"=paste0(round(100*stats::sd(tmp[flt,"Value"], na.rm=T)/mean(tmp[flt,"Value"], na.rm=T),1), "%"),
          "1/K"=1/(273.15+as.numeric(k)),
          "k_eff"=a,
          "log(-k_eff)"=ifelse(a<0, log(-a), NA),
          check.names=FALSE))
      })
      colnames(out)[1] <- "T [\u00B0C]"
      return(out)
    }
    tab1 <- shiny::reactive({
      req(df())
      getTab1(tmp=df())
    })
    output$Tab1 <- DT::renderDT({
      out <- tab1()
      for (i in which(colnames(out) %in% c("1/K", "k_eff", "log(-k_eff)"))) out[,i] <- round(out[,i], prec)
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

    observe({
      req(input$rbtn_storage, input$analyte)
      if (input$rbtn_storage == "rt") {
        x <- getValue(rv, c("Stability", "data"))
        x <- x[x[,"analyte"] == input$analyte,]
        if ("Temp" %in% colnames(x)) { x <- x[x[,"Temp"] == min(x[,"Temp"], na.rm=TRUE),] }
        coef <- log((mean(x[,"Value"])-2*sd(x[,"Value"]))/mean(x[,"Value"]))
        shiny::updateNumericInput(inputId = "num_coef", value = coef)
        shinyjs::disable(id = "num_coef")
      }
      if (input$rbtn_storage == "mt") {
        mt <- getValue(rv, c("General", "materialtabelle"))
        l <- which(mt[,"analyte"]==input$analyte)
        cert_val <- mt[l,"cert_val"]
        U_abs <- mt[l,"U_abs"]
        coef <- log((cert_val-U_abs)/cert_val)
        shiny::updateNumericInput(inputId = "num_coef", value = coef)
        shinyjs::disable(id = "num_coef")
      }
      if (input$rbtn_storage == "inp") {
        shinyjs::enable(id = "num_coef")
      }
    })

    output$outTab <- DT::renderDT({
      req(tab1exp(), input$num_coef)
      out <- tab1exp()
      out[,"month"] <- round(input$num_coef/(-1*exp(out[,"CI_upper"])))
      return(out[,c(1,10)])
    }, options = list(dom="t"), rownames = FALSE)

    output$user_month <- shiny::renderUI({
      shiny::req(input$user_temp, tab1(), input$num_coef, tab2())
      ce <- stats::coef(stats::lm(tab1()[,"log(-k_eff)"] ~ tab1()[,"1/K"]))
      ut_K <- 1/(273.15+input$user_temp)
      m <- as.numeric(round(input$num_coef/(-1*exp(ce[2]*ut_K + ce[1]))))
      m_CIup <- sqrt(tab2()[,"u(i)"]^2 + tab2()[,"u(s)"]^2*ut_K^2 + 2*tab2()[,"cov"]*ut_K) + (ce[2]*ut_K + ce[1])
      m_CIup <- as.numeric(round(input$num_coef/(-1*exp(m_CIup))))
      shiny::HTML("At the specified temperature of", input$user_temp, " the analyte ", input$analyte, " is expected to be stable for", m, "month (mean) or", m_CIup, "month (CI_upper) respectively.")
    })

    output$Fig2 <- shiny::renderPlot({
      shiny::req(tab1exp())
      prepFigS3(tab=tab1exp())
    })

    shiny::observeEvent(input$ArrheniusPlot1_link, {
      show_help("stability_arrhenius_figS2")
    })

    shiny::observeEvent(input$ArrheniusTab_link, {
      show_help("stability_arrhenius_tab1")
    })

    shiny::observeEvent(input$ArrheniusPlot2_link, {
      show_help("stability_arrhenius_figS3")
    })

    shiny::observeEvent(input$ArrheniusStorrageTemp_link, {
      show_help("stability_arrhenius_storage")
    })

    return(out)

  })

}
