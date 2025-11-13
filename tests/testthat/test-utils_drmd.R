testthat::test_that(
  desc = "flatten_list_to_df is working",
  code = {
    lst <- list(
      a = list(
        b = list(
          c = "v1",
          d = "v2"
        ),
        e = "v3",
        b = list(
          c = "v5",
          d = "v6"
        )
      ),
      f = "v4"
    )
    out <- eCerto:::flatten_list_to_df(lst)
    testthat::expect_true(is.data.frame(out))
    testthat::expect_true(all(colnames(out) %in% c("path","idx","value")))
    testthat::expect_true(nrow(out)==6)

    out <- eCerto:::flatten_list_to_df(lst, sep="|")
    testthat::expect_equal(grep("[|]", out[,1]), 1:5)
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
    out <- eCerto:::remove_prefix(example_list)
    testthat::expect_true(is.list(out))
    testthat::expect_equal(names(out), c("N1","N2"))
  }
)

testthat::test_that(
  desc = "set up new DRMD xml document",
  code = {
    # set up drmd
    dcc <- eCerto:::new_dcc_quantity_result()
    drmd_result_container <- eCerto:::new_drmd_measurementResult(quantities = dcc)
    drmd_result_container2 <- eCerto:::new_drmd_measurementResult(isCertified = "false", name_drmd = "Fun values only.", quantities = dcc)
    drmd_lst <- eCerto:::new_drmd_document(
      admin_data = eCerto:::new_drmd_admin_data(),
      result_data = list(drmd_result_container, drmd_result_container2)
    )

    # convert to xml and validate against schema
    drmd_xml <- xml2::as_xml_document(x = drmd_lst)
    testthat::expect_message(drmd_xml_valid <- eCerto:::validate_drmd_xml(drmc = drmd_xml)) # validation of internal xml results in message
    testthat::expect_true(inherits(drmd_xml_valid, "xml_document"))

    # write to temp file
    fl <- tempfile(fileext = ".xml")
    xml2::write_xml(x = drmd_xml_valid, file = fl)
    testthat::expect_error(testthat::expect_message(drmd_xml_from_file <- eCerto:::validate_drmd_xml(drmc = xml2::read_xml(x = fl))))  # validation of external xml does not (!) result in message
    testthat::expect_true(inherits(drmd_xml_from_file, "xml_document"))

  }
)

