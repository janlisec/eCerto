#'Report_Vorlage_Material.Rmd
#'genarated using: dput(readLines("C:/Users/jlisec/Documents/Projects/BAMTool_Backup/Report_Vorlage_Material.Rmd"))
Report_Vorlage_Material <- function() {
  c("---", "params:", "  res: NA", "---", "", "## BAM Tool Report Material", 
    "", "**Study ID: `r params$res$Certification$study_id`**", "", 
    "**User: `r params$res$Certification$user`**", "", "**Date: `r format(Sys.time(), '%d %B, %Y')`**", 
    "", "*Testoutput // Designphase*", "", "### Modul: Certification", 
    "", "```{r data, echo=FALSE}", "# get a local representation of all data provided by Shiny", 
    "Zertifizierung <- params$res$Certification", "", "```", "", 
    "These files have been provided to the tool by `r params$res$Certification$user`:", 
    "", "```{r data_files, echo=FALSE, comment=NA}", "cat(paste(Zertifizierung$input_files, collapse=\", \"))", 
    "```", "", "This is the table of certified values:", "", "```{r, echo=FALSE, comment=NA}", 
    "knitr::kable(Zertifizierung$cert_vals, row.names=FALSE, format.args = list(decimal.mark = \",\"))", 
    "```", "", "End of Report.")
}