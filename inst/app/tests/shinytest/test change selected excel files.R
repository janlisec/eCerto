app <- ShinyDriver$new("../../")
app$snapshotInit("test change selected excel files")

app$setInputs(navbarpage = "Start")
app$uploadFile(`excelfile-test-upld-xlsxfile-file` = c(
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris Koblenz_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
)) # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$snapshot()
app$uploadFile(`excelfile-test-upld-xlsxfile-file` = c(
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_BAM-3_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_BAM-ICPs_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_BAM-photom_m.xlsx")
)) # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$snapshot()
app$setInputs(`excelfile-go` = "click")
