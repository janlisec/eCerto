testthat::test_that(
  desc = "df_to_nested_list and flatten_list_to_df are working",
  code = {
    df <- data.frame(
      path = c("a_b_c", "a_b_d", "a_e", "a_b_c", "a_b_d", "f"),
      idx = c("1_1_1", "1_1_2", "1_2", "1_3_1", "1_3_2", "2"),
      value = c("value1", "value2", "value3", "value5", "value6", "value4"),
      stringsAsFactors = FALSE
    )
    lst <- df_to_nested_list(df)
    df2 <- flatten_list_to_df(lst)
    testthat::expect_true(identical(df, df2))

    # use different separator
    df2 <- flatten_list_to_df(lst, sep="|")
    testthat::expect_equal(lst, df_to_nested_list(df2, sep="[|]"))

    # check filtering
    df3 <- filter_flattened_list(df = df2, flt = "^1[|]3")
    testthat::expect_true(is.data.frame(df3))
    testthat::expect_equal(nrow(df3), 2)

  }
)

testthat::test_that(
  desc = "remove_prefix working (currently unused)",
  code = {
    example_list <- list(
      "pre:N1" = structure(list(
        "pre:SubN1" = 1,
        "pre:SubN2" = 2
      ), attr1 = "value1"),
      "pre:N2" = structure(list(
        "pre:SubN3" = 3,
        "pre:SubN4" = 4
      ), attr2 = "value2")
    )
    out <- remove_prefix(example_list)
    testthat::expect_true(is.list(out))
    testthat::expect_equal(names(out), c("N1","N2"))
  }
)

testthat::test_that(
  desc = "set up new DRMD xml document",
  code = {
    # set up drmd
    dcc <- new_dcc_quantity_result()
    drmd_result_container <- new_drmd_measurementResult(quantities = dcc)
    drmd_result_container2 <- new_drmd_measurementResult(isCertified = "false", name_drmd = "Fun values only.", quantities = dcc)
    drmd_lst <- new_drmd_document(admin_data = new_drmd_admin_data(), result_data = list(drmd_result_container, drmd_result_container2))

    # convert to xml and validate against schema
    drmd_xml <- xml2::as_xml_document(x = drmd_lst)
    testthat::expect_message(drmd_xml_valid <- validate_drmd_xml(drmc = drmd_xml)) # validation of internal xml results in message
    testthat::expect_true(inherits(drmd_xml_valid, "xml_document"))

    # write to temp file
    fl <- tempfile(fileext = ".xml")
    xml2::write_xml(x = drmd_xml_valid, file = fl)
    testthat::expect_error(testthat::expect_message(drmd_xml_from_file <- validate_drmd_xml(drmc = xml2::read_xml(x = fl))))  # validation of external xml does not (!) result in message
    testthat::expect_true(inherits(drmd_xml_from_file, "xml_document"))

  }
)

