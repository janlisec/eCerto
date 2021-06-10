app <- ShinyDriver$new("../../")
app$snapshotInit("UploadTest", screenshot = FALSE)
app$setInputs(navbarpage = "Start")
# upload parameter to test
prm_list = list(
  input = c(
    "excelfile-uploadTabset-pam-colslider",
    "excelfile-uploadTabset-pam-rowslider",
    "excelfile-go", # is go activated
    "excelfile-moduleSelect", # which module is selected
    "excelfile-uploadTabset-upld-sheet-sheet_sel", # which sheetname is selected
    "excelfile-uploadTabset-upld-xlsxfile-file" # uploaded Excel file
  ),
  output = "excelfile-uploadTabset-preview_out")

app$setInputs(`excelfile-moduleSelect` = "Certifications")
app$uploadFile(`excelfile-uploadTabset-upld-xlsxfile-file` = c(
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"),
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"),
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
))
Sys.sleep(2)
app$setInputs(`excelfile-uploadTabset-pam-rowslider` = c(7, 14))
app$setInputs(`excelfile-uploadTabset-pam-colslider` = c(1, 8))
app$snapshot(
  items = prm_list,
  screenshot = FALSE)

# Das Modul zweimal gewechselt wird
app$setInputs(`excelfile-moduleSelect` = "Homogeneity")
app$snapshot(
  items = prm_list,
  screenshot = FALSE)
app$setInputs(`excelfile-moduleSelect` = "Certifications")
app$snapshot(
  items = prm_list,
  screenshot = FALSE)

# Der Sheet gewechselt wird:
# -- Update Sliders
# -- Update Preview
app$setInputs(`excelfile-uploadTabset-upld-sheet-sheet_sel` = "nassch. Analysenverfahren")
Sys.sleep(2)
app$snapshot(
  items = prm_list,
  screenshot = FALSE)

# Eine andere Excelfile mit anderen Sheetnamen hochgeladen wird
app$uploadFile(`excelfile-uploadTabset-upld-xlsxfile-file` = c(
  system.file(package = "ecerto","extdata","Homog_Test.xlsx")
))
Sys.sleep(2)
app$snapshot(
  items = prm_list, screenshot = FALSE)

