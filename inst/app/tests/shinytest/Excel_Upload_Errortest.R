app <- ShinyDriver$new("../../")
app$snapshotInit("Excel_Upload_Errortest", screenshot = FALSE)
# Tests if all error messages occur properly
app$setInputs(navbarpage = "Start")
app$setInputs(`excelfile-moduleSelect` = "Certifications")
app$uploadFile(`excelfile-test-upld-xlsxfile-file` = c(
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx")
))
app$snapshot(
  items = list(output = "excelfile-test-preview_out"),
  screenshot = FALSE)

app$setInputs(`excelfile-moduleSelect` = "Homogeneity")
app$uploadFile(`excelfile-test-upld-xlsxfile-file` = c(
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx")
))
app$snapshot(
  items = list(output = "excelfile-test-preview_out"),
  screenshot = FALSE)

app$uploadFile(`excelfile-test-upld-xlsxfile-file` = c(
  system.file(package = "ecerto","extdata","Homog_Test.xlsx")
)) 
app$snapshot(
  items = list(output = "excelfile-test-preview_out"),
  screenshot = FALSE)