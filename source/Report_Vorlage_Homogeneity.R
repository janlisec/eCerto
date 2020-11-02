#'Report_Vorlage_Homogeneity.Rmd
#'genarated using: dput(readLines("C:/Users/jlisec/Documents/Projects/BAMTool_Backup/Reportvorlagen/Report_Vorlage_Homogeneity.Rmd"))
Report_Vorlage_Homogeneity <- function() {
  c("---", "params:", "  res: NA", "---", "", "## BAM Tool Report Homogeneity", 
    "", "**Date: `r format(Sys.time(), '%d %B, %Y')`**", "", "*Test-Output // Designphase*", 
    "", "### Modul: Homogeneity", "", "```{r data, echo=FALSE}", 
    "# get a local representation of all data provided by Shiny", 
    "Homogeneity <- params$res$Homogeneity", "h_dat <- Homogeneity[[\"h_dat\"]]", 
    "```", "", "`r ifelse(length(unique(h_dat[,\"analyte\"])>=2), \"These are the Homogeneity boxplots:\", \"This is the Homogeneity boxplot:\")`", 
    "", "```{r, echo=FALSE, comment=NA, fig.width=Homogeneity$h_Fig_width/72}", 
    "h_precision <- Homogeneity[[\"h_precision\"]]", "#for (a in levels(h_dat[,\"analyte\"])) {", 
    "for (a in levels(interaction(h_dat[,\"analyte\"],h_dat[,\"H_type\"]))) {", 
    "  tmp <- h_dat[interaction(h_dat[,\"analyte\"],h_dat[,\"H_type\"])==a,]", 
    "  omn <- round(mean(tmp[,\"value\"],na.rm=T), h_precision)", 
    "  osd <- round(sd(tmp[,\"value\"],na.rm=T), h_precision)", "  anp <- formatC(anova(lm(tmp[,\"value\"] ~ tmp[,\"Flasche\"]))$Pr[1],digits = 2, format = \"e\")", 
    "  par(mar=c(5,4,6,0)+0.1)", "  plot(x=c(1,length(levels(tmp[,\"Flasche\"]))), y=range(tmp[,\"value\"],na.rm=T), type=\"n\", xlab=\"Flasche\", ylab=paste0(a, \" [\", unique(tmp[,\"unit\"]),\"]\"), axes=F)", 
    "  abline(h=omn, lty=2)", "  abline(h=omn+c(-1,1)*osd, lty=2, col=grey(0.8))", 
    "  boxplot(tmp[,\"value\"] ~ tmp[,\"Flasche\"], add=TRUE)", "  mtext(text = paste(\"Overall mean =\", omn), side = 3, line = 2.45, adj = 1)", 
    "  mtext(text = paste(\"Overall sd =\", osd), side = 3, line = 1.3, adj = 1)", 
    "  mtext(text = paste(\"ANOVA P =\", anp), side = 3, line = 2.45, adj = 0)", 
    "}", "```", "", "This is the table of certified values:", "", 
    "```{r, echo=FALSE, comment=NA}", "#Homogeneity[[\"h_vals\"]][,\"ubb_r\"] <- round(Homogeneity[[\"h_vals\"]][,\"ubb_r\"], Homogeneity[[\"h_precision\"]])", 
    "knitr::kable(Homogeneity[[\"h_vals\"]], row.names=FALSE, format.args = list(decimal.mark = \",\"))", 
    "```", "", "This is the table of input data:", "", "```{r, echo=FALSE, comment=NA}", 
    "knitr::kable(Homogeneity[[\"h_dat\"]], row.names=FALSE, format.args = list(decimal.mark = \",\"))", 
    "```", "", "End of Report.")
}