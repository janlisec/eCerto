#' @title read_drmd_xml.
#'
#' @description \code{read_drmd_xml} will handle upload of a single XML file.
#'
#' @details Function will handle upload of a single XML file.
#'
#' @param xml_file A file path.
#' @param as_list Convert to list.
#' @param validate Validate against DRMD schema definition.
#' @return A list or a xml document.
#'
#' @noRd
#' @keywords internal
read_drmd_xml <- function(xml_file, as_list = TRUE, validate = FALSE) {
  drmc <- xml2::read_xml(x = xml_file)
  if (validate) {
    schema <- xml2::read_xml(x = "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/DRMD/drmd.xsd")
    xml2::xml_validate(drmc, schema)
  }
  if (as_list) drmc <- xml2::as_list(drmc, ns = xml2::xml_ns(drmc))
  return(drmc)
}
