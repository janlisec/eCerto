app <- ShinyDriver$new("../../")
app$snapshotInit("AnalyteChangeTest")
# this test is for testing if the materialtabelle is updated correctly
# after different analyte tab was selected
app$setInputs(link_to_start = "click")
app$setInputs(`excelfile-moduleSelect` = "Certifications")
app$uploadFile(`excelfile-test-upld-xlsxfile-file` = c(
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
))
app$setInputs(`excelfile-test-pam-rowslider` = c(7, 14))
app$setInputs(`excelfile-test-pam-colslider` = c(1, 8))
app$setInputs(`excelfile-go` = "click")
# app$setInputs(`certification-analyteModule-tabs` = "Si", wait_=FALSE, values_=FALSE)
app$snapshot(
  items = list(export = c("mat_cert-precision2","mat_cert-materialtabelle")),
  screenshot = FALSE)

app$setInputs(`certification-analyteModule-tabs` = "Zn")
app$snapshot(
  items = list(export = c("mat_cert-precision2","mat_cert-materialtabelle")),
  #items = list(export = "mat_cert-materialtabelle"),
  screenshot = FALSE)

app$setInputs(`certification-analyteModule-flt_samplesZn` = c("8", "24", "32"), wait_=FALSE, values_=FALSE)
# vals <- app$getAllValues()
# are the inputs set correctly?
# expect_identical(vals[["input"]][["certification-analyteModule-flt_samplesZn"]], c("8","24","32"))
app$snapshot(
  items = list(export = c("mat_cert-precision2","mat_cert-materialtabelle")),
  screenshot = FALSE)

app$setInputs(`certification-analyteModule-tabs` = "Fe")
app$snapshot(
  items = list(export = c("mat_cert-precision2","mat_cert-materialtabelle")),
  screenshot = FALSE)
app$setInputs(`certification-analyteModule-flt_samplesFe` = "10")
app$snapshot(
  items = list(export = c("mat_cert-precision2","mat_cert-materialtabelle")),
  screenshot = FALSE)
