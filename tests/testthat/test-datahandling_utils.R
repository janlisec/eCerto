test_that("loading functions decline other filetypes as excel", {
  # Create sample data
  df <- tibble::tibble(x = 1, y = 2)
  path_csv <- tempfile()
  write.csv(df, path_csv, row.names = FALSE)

  fpath = paste0(path_csv,"test.csv")
  expect_error(ecerto::load_sheetnames(fpath))
  expect_error(ecerto::load_excelfiles(fpath, 1))
})



# Test access_nested_list -------------------------------------------------

# @Frederick: habe ich auskommentiert, da ich die Funktion 'access_nested_list' direkt in das R6 integriert habe
# @Frederick: Ggf. rückgängig machen, falls etwas dagegen spricht und nachdem wir das diskutiert haben

# testthat::test_that("access_nested_list: Sucessfully access nested element",{
#   lz = list(a1=list(b1 = "Streusalz",b2 = "Andreas Scheuer"), a2 = "Wurst")
#   k = c("a1","b2") # keys
#   expect_equal(ecerto::access_nested_list(lz,k),"Andreas Scheuer")
# })
#
# testthat::test_that("access_nested_list: Error because nested field does not exist",{
#   lz = list(a1=list(b1 = "Streusalz",b2 = "Andreas Scheuer"), a2 = "Wurst")
#   k = c("a1","c1") # keys
#   expect_error(ecerto::access_nested_list(lz,k))
# })
#
# testthat::test_that("access_nested_list: Error because too much keys",{
#   lz = list(a1=list(b1 = "Streusalz",b2 = "Andreas Scheuer"), a2 = "Wurst")
#   k = c("a1","b1","c1") # keys
#   expect_error(ecerto::access_nested_list(lz,k))
# })
#
# testthat::test_that("access_nested_list: use reactiveValues",{
#   lz = list(a1=list(b1 = "Streusalz",b2 = "Andreas Scheuer"), a2 = "Wurst")
#   lz = do.call(shiny::reactiveValues,lz)
#   k = c("a1","b2") # keys
#   expect_equal(ecerto::access_nested_list(lz,k),"Andreas Scheuer")
# })
#
# testthat::test_that("set_nested_list: use reactiveValues",{
#   lz = list(a1=list(b1 = "Streusalz",b2 = "Andreas Scheuer"), a2 = "Wurst")
#   lz = do.call(shiny::reactiveValues,lz)
#   k = c("a1","b2") # keys
#
#   ecerto::set_nested_list(lz,k,"Klaus Wowereit")
#   expect_equal(ecerto::access_nested_list(lz,k),"Klaus Wowereit")
# })
#
# testthat::test_that("set_nested_list: Throw error because element does not exist",{
#   lz = list(a1=list(b1 = "Streusalz",b2 = "Andreas Scheuer"), a2 = "Wurst")
#   lz = do.call(shiny::reactiveValues,lz)
#   k = c("a1","b2","d2") # keys
#
#   expect_error(ecerto::set_nested_list(lz,k,"Klaus Wowereit"))
# })


# get/set -----------------------------------------------------------------

testthat::test_that("getValue: empty key should return reactive thing",{
  lz = list(a1=list(b1 = "Streusalz",b2 = "Andreas Scheuer"), a2 = "Wurst")
  lz = ecerto::reactiveClass$new(do.call(shiny::reactiveValues,lz))
  k = c("a1","b1") # keys
  expect_equal(class(ecerto::getValue(lz,NULL)),"reactivevalues")
})
