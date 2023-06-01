#' @title fnc_prepFigS1.
#' @description \code{prepFigS1} will prepare stability data for the lts plot function.
#' @details tbd.
#' @param s The imported stability data table.
#' @param an Name of the current analyte.
#' @param apm Analyte parameter list.
#' @param U_Def The deviation to be shown in the plot. Can be `U` if `mt` is present. Defaults to `2s`.
#' @param mt materialtabelle to possibly contain data on `U_abs`, `cert_val` and `unit`.
#' @examples
#' # s <- s_Data()
#' s <- eCerto:::test_Stability_Excel()
#' # an <- input$s_sel_analyte
#' # apm <- getValue(rv, c("General", "apm"))
#' apm <- list("Mn"=list("confirmed"=TRUE))
#' x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm=apm)
#' eCerto:::plot_lts_data(x=x_prep)
#' # mt <- getValue(rv, c("General", "materialtabelle"))
#' mt <- data.frame("analyte"="Mn", "cert_val"=1, "U_abs"=1, "sd"=1, "unit"="unit")
#' x_prep <- eCerto:::prepFigS1(s = s, an = "Mn", apm=apm, U_Def="U", mt=mt)
#' eCerto:::plot_lts_data(x=x_prep)
#' @return A list of length=2 containing measurement data ('val') and analyte definition ('def').
#' @noRd
#' @keywords internal
prepFigS1 <- function(s, an, apm = NULL, U_Def = c("2s", "U"), mt = NULL) {
  U_Def <- match.arg(U_Def)
  l <- s[,"analyte"]==an
  # Convert to format used in LTS module
  # load SD, CertVal, unit and U from certification if available
  CertVal <- mean(s[l,"Value"], na.rm=T)
  U <- 2*stats::sd(s[l,"Value"], na.rm=T)
  unit_col <- tolower(colnames(s))=="unit"
  KW_Unit <- ifelse(any(unit_col), unique(s[l,unit_col])[1], NA)
  if (!is.null(U_Def) && !is.null(mt) && an %in% names(apm) && apm[[an]][["confirmed"]]) {
    CertVal <- mt[mt[,"analyte"] %in% an, "cert_val"]
    U <- ifelse(U_Def=="U", 1, 2) * mt[mt[,"analyte"] %in% an, ifelse(U_Def=="U", "U_abs", "sd")]
    KW_Unit <- mt[which(mt[,"analyte"] == an), "unit"]
  }
  KW_Def <- ifelse("KW_Def" %in% colnames(s), unique(s[l,"KW_Def"])[1], an)
  KW_Unit <- ifelse("KW_Unit" %in% colnames(s), unique(s[l,"KW_Unit"])[1], KW_Unit)
  x <- list(
    "val"=s[l,],
    "def"=data.frame(
      "CertVal" = CertVal,
      "U"= U,
      "U_Def" = U_Def,
      "KW" = ifelse(an==KW_Def, NA, an),
      "KW_Def" = KW_Def,
      "KW_Unit" = KW_Unit,
      stringsAsFactors = FALSE
    )
  )
  return(x)
}
