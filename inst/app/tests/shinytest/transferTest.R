app <- ShinyDriver$new("../../")
app$snapshotInit("transferTest", screenshot = FALSE)
# this test observe if values from Homogeneity get transfer correctly into
# the materialtabelle
app$setInputs(link_to_start = "click")
# app$setInputs(`excelfile-moduleSelect` = "Homogeneity")
app$uploadFile(
  `excelfile-uploadTabset-upld-xlsxfile-file` = system.file(package = "ecerto","extdata","Homog_Test.xlsx")
)
app$snapshot(items = list(output = "excelfile-uploadTabset-preview_out"))
app$setInputs(`excelfile-go` = "click")
app$snapshot(items = list(output = TRUE))
app$setInputs(navbarpage = "Start")
app$setInputs(`excelfile-moduleSelect` = "Certifications")
app$uploadFile(`excelfile-uploadTabset-upld-xlsxfile-file` = c(
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
))
Sys.sleep(2)
app$setInputs(`excelfile-uploadTabset-pam-rowslider` = c(7, 14))
app$setInputs(`excelfile-uploadTabset-pam-colslider` = c(1, 8))
Sys.sleep(2)
app$setInputs(`excelfile-go` = "click")

app$setInputs(navbarpage = "tP_homogeneity")
app$setInputs(`trH-h_transfer_ubb_button` = "click")

# get only Homogeneity outputs (next two lines here)
vals <- app$getAllValues()
hcols_output = grep("Homogeneity+|trH+", names(vals$output), perl=TRUE, value=FALSE)
hcols_input = grep("Homogeneity+|trH+", names(vals$input), perl=TRUE, value=FALSE)
app$snapshot(items = list(
  input = names(vals$input[hcols_input]),
  output = names(vals$output[hcols_output]),
  export = "certification-mat_cert-materialtabelle"))
