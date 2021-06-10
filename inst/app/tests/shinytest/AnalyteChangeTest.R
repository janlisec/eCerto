app <- ShinyDriver$new("../../")
app$snapshotInit("AnalyteChangeTest")
# this test is for testing if the materialtabelle is updated correctly
# after different analyte tab was selected
export_params = c(
  "certification-mat_cert-precision2",
  "certification-mat_cert-materialtabelle",
  "certification-CertLoadedServer.output"
  )

app$setInputs(link_to_start = "click")
app$setInputs(`moduleSelect` = "Certifications") 
# app$uploadFile(`excelfile-uploadTabset-upld-xlsxfile-file` = c(
app$uploadFile(`excelfile-excel_file` = c(
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
))
Sys.sleep(2)
app$setInputs(`excelfile-uploadTabset-pam-rowslider` = c(7, 14))
app$setInputs(`excelfile-uploadTabset-pam-colslider` = c(1, 8))
Sys.sleep(2)
app$setInputs(`excelfile-go` = "click")
# app$setInputs(`certification-analyteModule-tabs` = "Si", wait_=FALSE, values_=FALSE)
app$snapshot(
  items = list(export = export_params),
  screenshot = FALSE)

app$setInputs(`certification-analyteModule-tabs` = "Zn")
app$snapshot(
  items = list(export = export_params),
  #items = list(export = "mat_cert-materialtabelle"),
  screenshot = FALSE)

app$setInputs(`certification-analyteModule-flt_samplesZn` = c("8", "24", "32"), wait_=FALSE, values_=FALSE)
# vals <- app$getAllValues()
# are the inputs set correctly?
# expect_identical(vals[["input"]][["certification-analyteModule-flt_samplesZn"]], c("8","24","32"))
app$snapshot(
  items = list(export = export_params),
  screenshot = FALSE)

app$setInputs(`certification-analyteModule-tabs` = "Fe")
app$snapshot(
  items = list(export = export_params),
  screenshot = FALSE)
app$setInputs(`certification-analyteModule-flt_samplesFe` = "10")
app$snapshot(
  items = list(export = export_params),
  screenshot = FALSE)
