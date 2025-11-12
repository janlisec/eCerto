#' @title Funktion zur Umwandlung eines DataFrames in eine verschachtelte Liste
#' @param df A data.frame with columns `ìdx`, `path` and `value`.
#' @param sep The character separator that was used in columns `ìdx` and `path`.
#' @return A nested list.
#' @noRd
#' @keywords internal
#' @examples
#' df <- data.frame(
#'   path = c("a_b_c", "a_b_d", "a_e", "a_b_c", "a_b_d", "f"),
#'   idx = c("1_1_1", "1_1_2", "1_2", "1_3_1", "1_3_2", "2"),
#'   value = c("value1", "value2", "value3", "value5", "value6", "value4"),
#'   stringsAsFactors = FALSE
#' )
#' lst <- df_to_nested_list(df)
#' df2 <- flatten_list_to_df(lst)
#' identical(df, df2)
#' df2 <- flatten_list_to_df(lst, sep="|")
#' df_to_nested_list(df2, sep="[|]")
df_to_nested_list <- function(df, sep = "_") {
  # set up an empty list
  nested_list <- list()
  # extract position and list-item names
  idx <- lapply(strsplit(df$idx, sep), as.numeric)
  nms <- strsplit(df$path, sep)
  # iterate on dataframe rows
  for (i in seq_len(nrow(df))) {
    ix <- idx[[i]]
    val <- df$value[i]
    # use purrr::modify_in, to populate list
    nested_list <- purrr::modify_in(nested_list, ix, ~ val)
  }
  # in a second round we assign the names to the sub lists
  for (i in seq_len(nrow(df))) {
    ix <- idx[[i]]
    for (j in seq_len(length(ix))) {
      if (j==1) {
        if (is.null(names(nested_list))) nested_list <- purrr::set_names(nested_list, "dummy")
        names(nested_list)[ix[j]] <- nms[[i]][j]
      } else {
        nested_list <- purrr::modify_in(nested_list, ix[1:(j-1)], \(x) {
          if (is.null(names(x))) x <- purrr::set_names(x, "dummy")
          names(x)[ix[j]] <- nms[[i]][j]
          return(x)
        })
      }
    }
  }
  return(nested_list)
}

#' @title Funktion zur Umwandlung der verschachtelten Liste in einen DataFrame mit Index-Pfaden
#'
#' @param nested_list A nested list.
#' @param sep The character separator to be used.
#'
#' @return A data.frame with columns `ìdx`, `path` and `value`
#' @noRd
#' @keywords internal
#'
#' @examples
#' lst <- list(
#'   a = list(
#'     b = list(
#'       c = "v1",
#'       d = "v2"
#'     ),
#'     e = "v3",
#'     b = list(
#'       c = "v5",
#'       d = "v6"
#'     )
#'   ),
#'   f = "v4"
#' )
#' flatten_list_to_df(lst)
#' flatten_list_to_df(lst, sep="|")
#' #drmc <- xml2::read_xml(x = "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/DRMD/drmc-007.xml")
#' #drmc <- xml2::as_list(drmc, ns = xml2::xml_ns(drmc))
#' #head(flatten_list_to_df(drmc))
#' #df <- flatten_list_to_df(drmc)
#' #filter_flattened_list(df = df, flt = "^1_2_2_2_1_2")

flatten_list_to_df <- function(nested_list, sep = "_") {
  # internal recursive version
  flatten_list_to_df_recursive <- function(nested_list, parent_name = "", idx_path = list(), sep = "_") {
    result <- data.frame()
    for (i in seq_along(nested_list)) {
      name <- names(nested_list)[i]
      value <- nested_list[[i]]
      current_name <- if (parent_name == "") name else paste(parent_name, name, sep = ifelse(is.null(name), "", sep))
      current_idx_path <- c(idx_path, i)

      if (is.list(value)) {
        # recursive call of function to process sub lists
        result <- rbind(result, flatten_list_to_df_recursive(value, current_name, current_idx_path, sep = sep))
      } else {
        # attach the final value
        result <- rbind(result, data.frame(path = current_name, idx = paste(current_idx_path, collapse = sep), value = value, stringsAsFactors = FALSE))
      }
    }
    return(result)
  }
  flatten_list_to_df_recursive(nested_list = nested_list, sep = sep)
}

#' Title
#'
#' @param df A df with at least col `idx`.
#' @param flt A flt string used in col `idx`.
#'
#' @return A filtered df.
#' @noRd
#' @keywords internal
filter_flattened_list <- function(df, flt = "^1_1") {
  stopifnot("idx" %in% colnames(df))
  idx <- grep(flt, df[,"idx"])
  df[idx,]
}

#' @title remove_prefix
#'
#' @description A nested list, i.e. an XML converted to such a structure, may
#'     contain names which shall be systematically modified. In case of the
#'     XML based structure, one could like to remove the namespace prefix tags
#'     which follow the structure 'prefix:name'.
#'     This function allows to recursively modify all names of a nested list
#'     removing a specified pattern.
#'
#' @param nested_list A nested list.
#' @param pattern A pattern to be removed from list names.
#'
#' @return A nested list of same length and structure as input but with
#'     potentially modified names.#'
#'
#' @noRd
#' @keywords internal
#'
#' @examples
#' example_list <- list(
#'   "pre:N1" = structure(list(
#'     "pre:SubN1" = 1,
#'     "pre:SubN2" = 2
#'   ), attr1 = "value1"),
#'   "pre:N2" = structure(list(
#'     "pre:SubN3" = 3,
#'     "pre:SubN4" = 4
#'   ), attr2 = "value2")
#' )
#' remove_prefix(example_list)

remove_prefix <- function(nested_list, pattern = "^[^:]*:") {
  # Funktion zum Entfernen des Präfixes
  remove_prefix_from_names <- function(x) {
    if (is.list(x)) {
      # Speichern der Attribute
      attrs <- attributes(x)

      # Entfernen des Präfixes von den Namen
      if (!is.null(attrs$names)) {
        attrs$names <- sub(pattern, "", attrs$names)
      }

      # Rekursive Anwendung auf Unterlisten
      x <- lapply(x, remove_prefix_from_names)

      # Wiederherstellen der Attribute
      attributes(x) <- attrs
    }
    return(x)
  }

  # Anwenden der Funktion auf die verschachtelte Liste
  nested_list <- remove_prefix_from_names(nested_list)
  return(nested_list)
}

#' Title
#'
#' @return A named list that can be converted into the `administrativeData` part
#'     of a DRMD XML file.
#' @noRd
#' @keywords internal
#'
#' @examples
#' new_drmd_admin_data()
new_drmd_admin_data <- function() {
  lst <- list(
    "drmd:administrativeData" = list(
      "drmd:coreData" = list(
        "drmd:titleOfTheDocument" = list("productInformationSheet"),
        "drmd:uniqueIdentifier" = list("{Hah-value}"),
        "drmd:validity" = list("drmd:untilRevoked" = list("true"))
      ),
      "drmd:referenceMaterialProducer" = list(
        "drmd:name" = list("dcc:content" = list("BAM")),
        "drmd:contact" = list(
          "dcc:name" = list("dcc:content" = list("BAM")),
          "dcc:eMail" = list("sales.crm@bam.de"),
          "dcc:phone" = list("+49 30 8104 2061"),
          "dcc:fax" = list("+49 30 8104 72061"),
          "dcc:link" = list("www.bam.de"),
          "dcc:location" = list(
            "dcc:street" = list("Richard-Willst\u00e4tter-Str."),
            "dcc:streetNo" = list("11"),
            "dcc:postCode" = list("D-12489"),
            "dcc:city" = list("Berlin"),
            "dcc:countryCode" = list("DE")
          )
        )
      ),
      "drmd:respPersons" = list(
        "dcc:respPerson" = list(
          "dcc:person" = list("dcc:name" = list("dcc:content" = list("Dr. John Doe"))),
          "dcc:role" = list("Project Coordinator, Division 1.7")
        )
      )
    )
  )
  return(lst)
}

#' Title
#'
#' @return A named list that can be converted into the `administrativeData` part
#'     of a DRMD XML file.
#' @noRd
#' @keywords internal
#'
#' @examples
#' new_drmd_statements()
new_drmd_statements <- function() {
  lst <- list(
    "drmd:statements" = list(
      "drmd:intendedUse" = list(
        "dcc:name" = list("dcc:content" = list("Intended Use")),
        "dcc:content" = list("The CRM is intended for checking the amount of boredom in students.")
      ),
      "drmd:commutability" = list(
        "dcc:name" = list("dcc:content" = list("Commutability"))
      ),
      "drmd:storageInformation" = list(
        "dcc:name" = list("dcc:content" = list("Storage Information"))
      ),
      "drmd:instructionsForHandlingAndUse" = list(
        "dcc:name" = list("dcc:content" = list("instructionsForHandlingAndUse"))
      )
    )
  )
  return(lst)
}

#' Title
#'
#' @return A named list that can be converted into the `materials` part
#'     of a DRMD XML file.
#' @noRd
#' @keywords internal
#'
#' @examples
#' new_drmd_materials()
new_drmd_materials <- function() {
  lst <- list(
    "drmd:materials" = list(
      "drmd:material" = list(
        "drmd:name" = list("dcc:content" = list("material name")),
        "drmd:description" = list("dcc:content" = list("The RM disappears once looked upon.")),
        #"drmd:materialClass" = list("tbd"),
        "drmd:minimumSampleSize" = list(
          "dcc:itemQuantity" = list(
            "si:realListXMLList" = list(
              "si:valueXMLList" = list("2"),
              "si:unitXMLList" = list("mL")
            )
          )
        )
      )
    )
  )
  return(lst)
}

#' Title
#'
#' @param name Item `name` of dcc:quantity.
#' @param label Item `label` of dcc:quantity.
#' @param value Item `value` of dcc:quantity.
#' @param unit Item `unit` of dcc:quantity.
#' @param uncertainty Item `uncertainty` of dcc:quantity.
#' @param coverageFactor Item `coverageFactor` of dcc:quantity.
#'
#' @return A named list that can be converted into the `quantity` entity of a DCC XML file.
#' @noRd
#' @keywords internal
#'
#' @examples
#' dcc <- new_dcc_quantity_result()
#' mt <- eCerto::CRM001$General$materialtabelle
#' mt_xml <- lapply(1:nrow(mt), function(i) {
#'   new_dcc_quantity_result(
#'     name = mt[i,"analyte"],
#'     label = mt[i,"analyte"],
#'     value = mt[i,"cert_val"],
#'     unit = mt[i,"unit"],
#'     uncertainty = mt[i,"U_abs"],
#'     coverageFactor = mt[i,"k"]
#'   )
#' })
new_dcc_quantity_result <- function(name = "Copper (Cu)", label = "Cu", value = 57.68, unit = "\u005Cpercent", uncertainty = 0.14, coverageFactor = 2) {
  lst <- list("drmd:quantity" = structure(list(), "refType" = "basic_measuredValue"))
  purrr::pluck(lst, "drmd:quantity", "dcc:name") <- list("dcc:content" = structure(list(name), "lang" = "en"))
  purrr::pluck(lst, "drmd:quantity", "si:real", "si:label") <- list(label)
  purrr::pluck(lst, "drmd:quantity", "si:real", "si:quantityTypeQUDT") <- list("MassFraction")
  purrr::pluck(lst, "drmd:quantity", "si:real", "si:value") <- list(value)
  purrr::pluck(lst, "drmd:quantity", "si:real", "si:unit") <- list(unit)
  purrr::pluck(lst, "drmd:quantity", "si:real", "si:measurementUncertaintyUnivariate") <- list(
    "si:expandedMU" = list(
      "si:valueExpandedMU" = list(uncertainty),
      "si:coverageFactor" = list(coverageFactor)
    )
  )
  return(lst)
}

#' Title
#'
#' @param isCertified Attribute `isCertified` of drmd:measurementResult.
#' @param name_drmd Item `name_drmd` of drmd:measurementResult.
#' @param name_dcc Item `name_dcc` of drmd:measurementResult.
#' @param description Item `description` of drmd:measurementResult.
#' @param quantities List of item `quantities` of drmd:measurementResult.
#'
#' @return A named list that can be converted into the `measurementResult` entity of a DRMD XML file.
#' @noRd
#' @keywords internal
#'
#' @examples
#' drmd_result_container <- new_drmd_measurementResult(quantities = dcc)
#' drmd_result_container2 <- new_drmd_measurementResult(isCertified = "false", name_drmd = "Fun values only.", quantities = dcc)
new_drmd_measurementResult <- function(isCertified = "true", name_drmd = "Certified mass fractions and their associated uncertainties.", name_dcc = "Certified Values", description = "Description", quantities = NULL) {
  lst <- list("drmd:materialProperties" = structure(list(), "isCertified" = isCertified))
  purrr::pluck(lst, "drmd:materialProperties", "drmd:name") <- list("dcc:content" = structure(list(name_drmd), "lang" = "en"))
  purrr::pluck(lst, "drmd:materialProperties", "drmd:results") <- list("drmd:result" = list("drmd:name" = list("dcc:content" = structure(list(name_dcc), "lang" = "en"))))
  purrr::pluck(lst, "drmd:materialProperties", "drmd:results", "drmd:result", "drmd:description") <- list("dcc:content" = structure(list(description), "lang" = "en"))
  purrr::pluck(lst, "drmd:materialProperties", "drmd:results", "drmd:result", "drmd:data", "drmd:list") <- quantities
  return(lst)
}

#' Title
#'
#' @param admin_data Item `admin_data` of drmd:measurementResult.
#' @param result_data Item `result_data` of drmd:measurementResult.
#' @param remove_ns Boolean indicating if namespace prefixes shall be removed.
#'
#' @return A named list that can be converted into a full `digitalReferenceMaterialDocument` XML file.
#' @noRd
#' @keywords internal
#'
#' @examples
#' \dontrun{
#'  dcc <- new_dcc_quantity_result()
#'  drmd_result_container <- new_drmd_measurementResult(quantities = dcc)
#'  drmd_result_container2 <- new_drmd_measurementResult(isCertified = "false", name_drmd = "Fun values only.", quantities = dcc)
#'  drmd_lst <- new_drmd_document(admin_data = new_drmd_admin_data(), result_data = list(drmd_result_container, drmd_result_container2))
#'  # flatten_list_to_df(drmd_lst)
#'  drmd_xml <- xml2::as_xml_document(x = drmd_lst)
#'  fl <- tempfile(fileext = ".xml")
#'  str(validate_drmd_xml(drmc = drmd_xml))
#'  xml2::write_xml(x = drmd_xml, file = fl)
#'  str(validate_drmd_xml(drmc = xml2::read_xml(x = fl)))
#'  tmp <- readLines(fl)
#' }
new_drmd_document <- function(admin_data = NULL, result_data = NULL, remove_ns = FALSE) {
  lst <- list("drmd:digitalReferenceMaterialDocument" = structure(
    list(
      admin_data[[1]],
      new_drmd_materials()[[1]],
      result_data,
      new_drmd_statements()[[1]]
    ), names = c("drmd:administrativeData", "drmd:materials", "drmd:materialPropertiesList", "drmd:statements"),
    "schemaVersion"="0.3.0", "xmlns:dcc"="https://ptb.de/dcc", "xmlns:drmd"="https://example.org/drmd", "xmlns:si"="https://ptb.de/si")
  )
  if (remove_ns) lst <- remove_prefix(lst)
  return(lst)
}
