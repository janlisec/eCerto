# reactive values
s_Data <- reactive({
  if (is.null(input$s_input_file)) {
    if (is.null(input$in_file_ecerto_backup)) {
      if (exists("s_dat", envir = env_perm)) rm("s_dat", envir = env_perm)
      if (exists("s_vals", envir = env_perm)) rm("s_vals", envir = env_perm)
      return(NULL)
    } else {
      file.type <- tools::file_ext(input$in_file_ecerto_backup$datapath)
      validate(need(length(file.type)==1 & tolower(file.type)=="rdata", "Please select only one RData file."))
      tryCatch({ load(input$in_file_ecerto_backup$datapath[1]) }, error = function(e) { stop(safeError(e)) })
      if (is.null(res$Stability)) {
        if (exists("s_dat", envir = env_perm)) rm("s_dat", envir = env_perm)
        if (exists("s_vals", envir = env_perm)) rm("s_vals", envir = env_perm)
        return(NULL)
      } else {
        assign("s_dat", value = res$Stability$s_dat, envir = env_perm)
        assign("s_vals", value = res$Stability$s_vals, envir = env_perm)
        return(res$Stability$s_dat)
      }
    }
  } else {
    # allow the user to import either a simple 2-column table with analyte names extracted from table names or use the complicated format from the LTS modul
    test_format <- openxlsx::read.xlsx(xlsxFile = input$s_input_file$datapath[1], sheet = 1)
    if (ncol(test_format)>=3 && "KW" %in% colnames(test_format)) {
      s_dat <- read_lts_input(file = input$s_input_file$datapath[1], simplify=TRUE)
      colnames(s_dat)[colnames(s_dat)=="KW"] <- "analyte"
    } else {
      s_dat <- plyr::ldply(openxlsx::getSheetNames(file = input$s_input_file$datapath[1]), function(x) { 
        cbind("analyte"=x, openxlsx::read.xlsx(xlsxFile = input$s_input_file$datapath[1], sheet = x, detectDates=TRUE))
      }, .id = NULL)
    }
    
    validate(need(c("analyte","Value","Date") %in% colnames(s_dat), "No all required input columns found in input file."))
    validate(need(is.numeric(s_dat[,"Value"]), "Column 'Value' in input file contains non-numeric values."))
    if (class(s_dat[,"Date"])!="Date") { s_dat[,"Date"] <- as.Date.character(s_dat[,"Date"],tryFormats = c("%Y-%m-%d","%d.%m.%Y","%Y/%m/%d")) }
    validate(need(class(s_dat[,"Date"])=="Date", "Sorry, could not convert column 'Date' into correct format."))
    s_dat[,"analyte"] <- factor(s_dat[,"analyte"])
    s_vals <- plyr::ldply(split(s_dat, s_dat[,"analyte"]), function(x) {
      x_lm <- lm(Value ~ Date, data=x)
      mon_diff <- mondf(min(x[,"Date"]),max(x[,"Date"]))
      x_slope <- summary(x_lm)$coefficients[2,1:2]
      data.frame("mon_diff"=mon_diff, "slope"=x_slope[1], "SE_slope"=x_slope[2], "U_Stab"=x_slope[1]*x_slope[2])
    }, .id="analyte")
    assign("s_dat", value = s_dat, envir = env_perm)
    assign("s_vals", value = s_vals, envir = env_perm)
    return(s_dat)
  }
})

output$s_fileUploaded <- reactive({
  return(!is.null(s_Data()))
})
outputOptions(output, 's_fileUploaded', suspendWhenHidden=FALSE)

s_res <- reactive({
  # combine data for backup in a list
  if (is.null(s_Data())) {
    return(list("Stability"=NULL))
  } else {
    return(list("Stability"=list(
      "s_file"=input$s_input_file[[1]],
      "s_sel_analyte"=input$s_sel_analyte,
      "s_dat"=getData("s_dat"),
      "s_vals"=getData("s_vals")
    )))
  }
}, label="debug_s_res")


# Tables
output$s_overview <- DT::renderDataTable({
  req(s_Data(), input$s_sel_analyte)
  s <- getData("s_dat")
  s[s[,"analyte"]==input$s_sel_analyte,c("Date","Value")]
}, options = list(paging = TRUE, searching = FALSE), rownames=NULL)

output$s_vals <- DT::renderDataTable({
  req(s_Data())
  s_vals <- getData("s_vals")
  for (i in c("slope","SE_slope","U_Stab")) {
    s_vals[,i] <- pn(s_vals[,i], 4)
  }
  if (!is.null(c_Data())) {
    cert_vals <- getData("cert_vals")
    s_vals[,"In_Cert_Module"] <- sapply(s_vals[,"analyte"], function(x) {
      ifelse(is.null(cert_vals),"cert table not found", ifelse(x %in% cert_vals[,"analyte"], "Yes", "No"))
    })
  }
  
  return(s_vals)
}, options = list(paging = FALSE, searching = FALSE), rownames=NULL)

# Figures
output$s_plot <- renderPlot({
  req(s_Data(), input$s_sel_analyte)
  s <- s_Data()
  l <- s[,"analyte"]==input$s_sel_analyte
  
  # make a simple plot...
  # plot(Value~Date, data=s[l,], main=input$s_sel_analyte)
  # abline(lm(Value~Date, data=s[l,]))

  # ...or convert to format used in LTS modul
  # load SD and U from certification if available
  CertVal <- mean(s[l,"Value"], na.rm=T)
  U <- 2*sd(s[l,"Value"], na.rm=T)
  if (!is.null(input$sel_analyt) & !is.null(input$s_sel_dev)) {
    cert_vals <- getData("cert_vals")
    if (any(cert_vals[,"analyte"] %in% input$s_sel_analyte)) {
      CertVal <- cert_vals[cert_vals[,"analyte"] %in% input$s_sel_analyte,"cert_val"]
      U <- ifelse(input$s_sel_dev=="U", 1, 2)*cert_vals[cert_vals[,"analyte"] %in% input$s_sel_analyte, ifelse(input$s_sel_dev=="U", "U", "sd")]
    }
  }
  x <- list("val"=s[l,], 
            "def"=data.frame(
              "CertVal" = CertVal,
              "U"= U,
              "U_Def" = "2s",
              "KW" = input$s_sel_analyte,
              "KW_Def" = ifelse("KW_Def" %in% colnames(s), unique(s[l,"KW_Def"])[1],"KW_Def"),
              "KW_Unit" = ifelse("KW_Unit" %in% colnames(s), unique(s[l,"KW_Unit"])[1],"KW_Unit"),
              stringsAsFactors = FALSE
              )
            )
  plot_lts_data(x=x)
})

# Specific UI and events
output$s_sel_analyte <- renderUI({
  req(s_Data())
  selectInput(inputId="s_sel_analyte", label="analyte", choices=levels(s_Data()[,"analyte"]), selected=levels(s_Data()[,"analyte"])[1])
})

output$s_sel_dev <- renderUI({
  req(s_Data(), input$sel_analyt)
  selectInput(inputId="s_sel_dev", label="deviation to show", choices=c("2s","U"), selected="2s")
})

output$s_info <- renderUI({
  req(s_Data(), input$s_sel_analyte)
  txt <- "mean and 2s of uploaded stability data for "
  if (!is.null(input$sel_analyt) & !is.null(input$s_sel_dev)) {
    cert_vals <- getData("cert_vals")
    if (any(cert_vals[,"analyte"] %in% input$s_sel_analyte)) {
      txt <- paste0("mean and ", input$s_sel_dev, " of uploaded certification data for ")
    }
  }
  helpText(paste0("Figure shows ", txt, input$s_sel_analyte,"."))
})

output$s_transfer_ubb <- renderUI({
  validate(need(input$sel_analyt, message = "please upload certification data first"))
  cert_vals <- getData("cert_vals")
  selectInput(inputId="s_transfer_ubb", label="", selectize=TRUE, choices=attr(cert_vals, "col_code")[substr(attr(cert_vals, "col_code")[,"ID"],1,1)=="U","Name"])
})

observeEvent(input$s_transfer_ubb_button, {
  req(s_Data(), getData("s_vals"), getData("cert_vals"), input$s_transfer_ubb)
  s_vals <- getData("s_vals")
  cert_vals <- getData("cert_vals")
  for (i in 1:nrow(cert_vals)) {
    j <- which(as.character(s_vals[,"analyte"])==as.character(cert_vals[i,"analyte"]))
    if (length(j)==1) { cert_vals[i,attr(cert_vals, "col_code")[which(attr(cert_vals, "col_code")[,"Name"]==isolate(input$s_transfer_ubb)),"ID"]] <- max(s_vals[j,"U_Stab"]) }
  }
  assign("cert_vals", value=cert_vals, envir = env_perm)
  shinyjs::click(id="show_table")
})
