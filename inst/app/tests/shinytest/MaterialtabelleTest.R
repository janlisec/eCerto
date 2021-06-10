app <- ShinyDriver$new("../../")
app$snapshotInit("MaterialtabelleTest")

app$setInputs(link_to_start = "click")
app$setInputs(`excelfile-moduleSelect` = "Certifications")
app$uploadFile(`excelfile-uploadTabset-upld-xlsxfile-file` = c(
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Koblenz_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_Aleris_Duffel_m.xlsx"), 
  system.file(package = "ecerto","extdata","Ergebnisblatt_BAM-M321_AMAG_Nasschemie_m.xlsx")
))
app$setInputs(`excelfile-uploadTabset-pam-rowslider` = c(7, 13))
app$setInputs(`excelfile-uploadTabset-pam-colslider` = c(1, 8))
app$setInputs(`excelfile-go` = "click")

app$setInputs(`certification-mat_cert-c_fix_col_names` = "F1",allowInputNoBinding_ = TRUE)
app$setInputs(`certification-mat_cert-c_displayed_col_name` = "col1",allowInputNoBinding_ = TRUE)
app$snapshot(items = list(
  export = "certification-mat_cert-materialtabelle"))
app$setInputs(`certification-mat_cert-c_fix_col_names` = "U2",allowInputNoBinding_ = TRUE)
app$setInputs(`certification-mat_cert-c_displayed_col_name` = "col2",allowInputNoBinding_ = TRUE)
app$snapshot(items = list(
  export = "certification-mat_cert-materialtabelle"))
app$setInputs(`certification-mat_cert-c_fix_col_names` = "U3",allowInputNoBinding_ = TRUE)
app$setInputs(`certification-mat_cert-c_displayed_col_name` = "delete",allowInputNoBinding_ = TRUE)
app$snapshot(items = list(
  export = "certification-mat_cert-materialtabelle"))
app$setInputs(`certification-mat_cert-show_table` = "click")
app$snapshot(items = list(
  export = "certification-mat_cert-materialtabelle"))
app$setInputs(`certification-mat_cert-pooling` = TRUE)
app$snapshot(items = list(
  export = "certification-mat_cert-materialtabelle"))