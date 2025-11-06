#' @title validate_drmd_xml.
#'
#' @description \code{validate_drmd_xml} will validate a XML file against schema.
#'
#' @details Function will handle upload of a single XML file.
#'
#' @param xml_file A file path.
#' @param as_list Convert to list.
#' @param validate Validate against DRMD schema definition.
#' @return A list or a xml document.
#'
#' @examples
#' fl <- system.file(package = "eCerto", "extdata", "drmd", "BAM-M375a.xml")
#' # fl <- "c:/Users/jlisec/Documents/Projects/BAMTool_Backup/DRMD/BAM_template.xml"
#' str(validate_drmd_xml(drmc = xml2::read_xml(x = fl)))
#'
#' @noRd
#' @keywords internal
validate_drmd_xml <- function(drmc) {
  schema <- xml2::read_xml(x = system.file(package = "eCerto", "extdata", "drmd", "v0.3.0", "xsd", "drmd.xsd"))
  test <- xml2::xml_validate(drmc, schema)
  if (!test) { for (x in attr(test, "errors")) message(x) }
  return(drmc)
}

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
#' @examples
#'
#' fl <- system.file(package = "eCerto", "extdata", "drmd", "BAM-M375a.xml")
#' # fl <- "C:/Users/jlisec/Documents/Projects/BAMTool_Backup/DRMD/BAM-A001_DRMD_20251105.xml"
#' tmp <- read_drmd_xml(xml_file = fl, validate = TRUE)
#'
#' @noRd
#' @keywords internal
read_drmd_xml <- function(xml_file, as_list = TRUE, validate = FALSE) {
  drmc <- xml2::read_xml(x = xml_file)
  if (validate) { validate_drmd_xml(drmc) }
  if (as_list) drmc <- xml2::as_list(drmc, ns = xml2::xml_ns(drmc))
  return(drmc)
}
