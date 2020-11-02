#
# HOMOGENEITY
# !! be aware that while we compute output based on permanently stored data, we need to check for responsive variable to ensure updating
# !! use req(h_Data()) instead of req(getData("h_dat", env_perm)) in general
#
h_Data <- reactive({
  if (is.null(input$h_input_files)) {
    if (is.null(input$in_file_ecerto_backup)) {
      # delete persistent values if both input dialogs are empty (e.g. after restart of the tool)
      if (exists("h_dat", envir = env_perm)) rm("h_dat", envir = env_perm)
      if (exists("h_vals", envir = env_perm)) rm("h_vals", envir = env_perm)
      return(NULL)
    } else {
      file.type <- tools::file_ext(input$in_file_ecerto_backup$datapath)
      validate(need(length(file.type)==1 & tolower(file.type)=="rdata", "Please select only one RData file."))
      tryCatch({ load(input$in_file_ecerto_backup$datapath[1]) }, error = function(e) { stop(safeError(e)) })
      if (is.null(res$Homogeneity)) {
        # delete persistent values if backup without homogeneity data is loaded
        if (exists("h_dat", envir = env_perm)) rm("h_dat", envir = env_perm)
        if (exists("h_vals", envir = env_perm)) rm("h_vals", envir = env_perm)
        return(NULL)
      } else {
        assign("h_dat", value = res$Homogeneity$h_dat, envir = env_perm)
        assign("h_vals", value = res$Homogeneity$h_vals, envir = env_perm)
        updateSelectizeInput(session, inputId = "h_sel_analyt", selected = res$Homogeneity$h_sel_analyt)
        updateNumericInput(session, inputId = "h_precision", value = res$Homogeneity$h_precision)
        updateNumericInput(session, inputId = "h_Fig_width", value = res$Homogeneity$h_Fig_width)
        return(res$Homogeneity$h_dat)
      }
    }
  } else {
    h_dat <- openxlsx::read.xlsx(xlsxFile = input$h_input_files$datapath)
    validate(
      need("analyte" %in% colnames(h_dat), "No column 'analyte' found in input file."),
      need("value" %in% colnames(h_dat), "No column 'value' found in input file.")
    )
    validate(need(is.numeric(h_dat[,"value"]), "Column 'value' in input file contains non-numeric values."))
    h_dat[,"analyte"] <- factor(h_dat[,"analyte"])
    validate(need("Flasche" %in% colnames(h_dat), "No column 'Flasche' found in input file."))
    h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
    validate(need("H_type" %in% colnames(h_dat), "No column 'H_type' found in input file."))
    h_dat[,"H_type"] <- factor(h_dat[,"H_type"])
    h_vals <- plyr::ldply(split(h_dat, h_dat[,"analyte"]), function(y) {
      plyr::ldply(split(y, y[,"H_type"]), function(x) {
        anm <- anova(lm(value ~ Flasche, data=x))
        MSamong <- anm[1,"Mean Sq"]
        MSwithin <- anm[2,"Mean Sq"]
        mn <- mean(sapply(split(x[,"value"],x[,"Flasche"]),mean,na.rm=T),na.rm=T)
        n <- round(mean(table(as.character(x[,"Flasche"]))))
        N <- length(unique(x[,"Flasche"]))
        s_bb <- ifelse(MSamong>MSwithin, sqrt((MSamong-MSwithin)/n), 0)/mn
        s_bb_min <- (sqrt(MSwithin/n)*(2/(N*(n-1)))^(1/4))/mn
        data.frame("mean"=mn, "n"=n, "N"=N, "MSamong"=MSamong, "MSwithin"=MSwithin, "P"=anm$Pr[1], "s_bb"=s_bb, "s_bb_min"=s_bb_min)
      }, .id="H_type")
    }, .id="analyte")
    assign("h_dat", value = h_dat, envir = env_perm)
    assign("h_vals", value = h_vals, envir = env_perm)
    return(h_dat)
  }
})

output$h_fileUploaded <- reactive({
  return(!is.null(h_Data()))
})
outputOptions(output, 'h_fileUploaded', suspendWhenHidden=FALSE)

output$h_sel_analyt <- renderUI({
  req(h_Data())
  lev <- levels(interaction(getData("h_dat", env_perm)[,"analyte"],getData("h_dat", env_perm)[,"H_type"]))
  selectInput(inputId="h_sel_analyt", label="analyte", choices=lev)
})

h_means <- reactive({
  req(getData("h_dat", env_perm), input$h_sel_analyt)
  h_dat <- getData("h_dat", env_perm)
  h_dat <- h_dat[interaction(h_dat[,"analyte"],h_dat[,"H_type"])==input$h_sel_analyt,]
  h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
  out <- plyr::ldply(split(h_dat[,"value"], h_dat[,"Flasche"]), function(x) {
    data.frame("mean"=mean(x,na.rm=T), "sd"=sd(x,na.rm=T), "n"=sum(is.finite(x))) 
  }, .id="Flasche")
  rownames(out) <- out[,"Flasche"]
  return(out)
})

# Error checks
h_errors <- reactive({
  req(input$h_precision)
  validate(need(is.numeric(input$h_precision) && input$h_precision>=0 && input$h_precision<=12, message="please check precision value"))
  return("")
})
output$h_error_message <- renderText(h_errors())

# Tables
output$h_overview_stats <- DT::renderDataTable({
  req(h_means(), input$h_precision)
  tab <- h_means()
  for (i in c("mean","sd")) { tab[,i] <- pn(tab[,i], input$h_precision) }
  return(tab)
}, options = list(paging = FALSE, searching = FALSE), rownames=NULL)

output$h_vals <- DT::renderDataTable({
  req(h_Data())
  h_vals <- getData("h_vals")
  for (i in c("mean","MSamong","MSwithin","P","s_bb","s_bb_min")) {
    h_vals[,i] <- pn(h_vals[,i], input$h_precision)
  }
  if (!is.null(c_Data())) {
    cert_vals <- getData("cert_vals")
    h_vals[,"In_Cert_Module"] <- sapply(h_vals[,"analyte"], function(x) {
      ifelse(is.null(cert_vals),"cert table not found", ifelse(x %in% cert_vals[,"analyte"], "Yes", "No"))
    })
  }
  return(h_vals)
}, options = list(paging = FALSE, searching = FALSE), rownames=NULL)


# Plots & Print
output$h_boxplot <- shiny::renderPlot({
  req(h_Data(), input$h_sel_analyt, input$h_precision, input$h_Fig_width)
  h_dat <- getData("h_dat", env_perm)
  h_dat <- h_dat[interaction(h_dat[,"analyte"],h_dat[,"H_type"])==input$h_sel_analyt,]
  h_dat[,"Flasche"] <- factor(h_dat[,"Flasche"])
  omn <- round(mean(h_dat[,"value"],na.rm=T),input$h_precision)
  osd <- round(sd(h_dat[,"value"],na.rm=T),input$h_precision)
  anp <- formatC(anova(lm(h_dat[,"value"] ~ h_dat[,"Flasche"]))$Pr[1],digits = 2, format = "e")
  par(mar=c(5,4,6,0)+0.1)
  plot(x=c(1,length(levels(h_dat[,"Flasche"]))), y=range(h_dat[,"value"],na.rm=T), type="n", xlab="Flasche", ylab=paste0(input$h_sel_analyt, " [", unique(h_dat["unit"]),"]"), axes=F)
  abline(h=omn, lty=2)
  abline(h=omn+c(-1,1)*osd, lty=2, col=grey(0.8))
  boxplot(h_dat[,"value"] ~ h_dat[,"Flasche"], add=TRUE)
  mtext(text = paste("Overall mean =", omn), side = 3, line = 2.45, adj = 1)
  mtext(text = paste("Overall sd =", osd), side = 3, line = 1.3, adj = 1)
  mtext(text = paste("ANOVA P =", anp), side = 3, line = 2.45, adj = 0)
}, height=500, width=reactive({input$h_Fig_width}))

output$h_statement <- shiny::renderText({
  req(h_Data(), input$h_sel_analyt)
  h_vals <- getData("h_vals", env_perm)
  ansd <- max(h_vals[interaction(h_vals[,"analyte"],h_vals[,"H_type"])==input$h_sel_analyt,c("s_bb","s_bb_min")])
  anp <- h_vals[interaction(h_vals[,"analyte"],h_vals[,"H_type"])==input$h_sel_analyt,"P"]
  if (anp<0.05) {
    return(paste0("The tested items (Flasche) are significantly different (ANOVA P-value = ", pn(anp,2),"). Please check your method and model."))
  } else {
    return(paste0("The tested items (Flasche) are not significantly different (ANOVA P-value = ", pn(anp,2), "). The uncertainty value for analyte ", input$h_sel_analyt, " was determined as ", pn(ansd), "."))
  }
})

output$h_anova <- shiny::renderPrint({
  req(h_Data(), input$h_sel_analyt)
  h_dat <- getData("h_dat", env_perm)
  h_dat <- h_dat[interaction(h_dat[,"analyte"],h_dat[,"H_type"])==input$h_sel_analyt,]
  anova(lm(h_dat[,"value"] ~ h_dat[,"Flasche"]))
})


# Special UI
output$h_transfer_ubb <- renderUI({
  validate(need(input$sel_analyt, message = "please upload certification data first"))
  req(getData("cert_vals"))
  selectInput(inputId="h_transfer_ubb", label="", selectize=TRUE, choices=attr(getData("cert_vals"), "col_code")[substr(attr(getData("cert_vals"), "col_code")[,"ID"],1,1)=="U","Name"])
})

output$h_transfer_H_type <- renderUI({
  req(h_Data())
  h_vals <- getData("h_vals")
  selectInput(inputId="h_transfer_H_type", label="", selectize=TRUE, choices=levels(h_vals[,"H_type"]))
})

observeEvent(input$h_transfer_ubb_button, {
  req(h_Data(), getData("h_vals"), getData("cert_vals"), input$h_transfer_ubb, input$h_transfer_H_type)
  h_vals <- getData("h_vals")
  cert_vals <- getData("cert_vals")
  for (i in 1:nrow(cert_vals)) {
    j <- which(as.character(h_vals[,"analyte"])==as.character(cert_vals[i,"analyte"]) & as.character(h_vals[,"H_type"])==isolate(input$h_transfer_H_type))
    if (length(j)==1) { cert_vals[i,attr(cert_vals, "col_code")[which(attr(cert_vals, "col_code")[,"Name"]==isolate(input$h_transfer_ubb)),"ID"]] <- max(h_vals[j,c("s_bb","s_bb_min")]) }
  }
  assign("cert_vals", value=cert_vals, envir = env_perm)
  shinyjs::click(id="show_table")
})


# Backup and reporting
# combine data for report and backup in a list
h_res <- reactive({
  if (is.null(h_Data())) {
    return(list("Homogeneity"=NULL))
  } else {
    return(list("Homogeneity"=list(
      "h_file"=input$h_input_files[[1]],
      "h_dat"=getData("h_dat"),
      "h_vals"=getData("h_vals"),
      "h_sel_analyt"=input$h_sel_analyt,
      "h_precision"=input$h_precision,
      "h_Fig_width"=input$h_Fig_width
    )))
  }
}, label="debug_h_res")

output$h_Report <- downloadHandler(
  filename = function() {paste0(input$study_id, "_", input$h_sel_analyt, '_homogeneity.html')}, 
  content = function(file) {
    owd <- setwd(tempdir(check = TRUE)); on.exit(setwd(owd))
    writeLines(text = Report_Vorlage_Homogeneity(), con = 'Report_Vorlage_Homogeneity.Rmd')
    out <- rmarkdown::render(
      input='Report_Vorlage_Homogeneity.Rmd',
      output_format=rmarkdown::html_document(),
      params=list("res"=h_res()),
      envir = new.env(parent = globalenv())
    )
    file.rename(out, file)
  }
)