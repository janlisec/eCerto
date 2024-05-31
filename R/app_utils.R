#' @title xlsxSheetNames.
#' @description Loads names of Excel sheets.
#' @param filepath the path to a single or multiple excel file(s)
#' @return the names of sheets
#' @importFrom openxlsx getSheetNames
#' @noRd
#' @keywords internal
xlsxSheetNames <- function(filepath) {
  a <- lapply(shiny::isolate(filepath), function(x) {
    ext <- tools::file_ext(x)
    if (tolower(ext) == "xlsx") {
      openxlsx::getSheetNames(x)
    } else {
      if (is.null(shiny::getDefaultReactiveDomain())) {
        stop("Please select only Excel (.xlsx) files.")
      } else {
        shiny::showModal(
          shiny::modalDialog("Please select only Excel (.xlsx) files.", title = "Wrong Filetype?")
        )
      }
      return(NULL)
    }
  })
  if (length(unique(a)) != 1) {
    if (is.null(shiny::getDefaultReactiveDomain())) {
      stop("Sheet names are different within files.")
    } else {
      shiny::showModal(
        shiny::modalDialog("Sheet names are different within files.", title = "Different sheetnames?")
      )
    }
  }
  return(a[[1]])
}

#' @title pn.
#' @description Format a number by rounding to a precision in same width as
#'   character using scientific notation for numbers < precision and rounding
#'   to precision otherwise.
#' @param n numeric vector
#' @param p requested precision after the decimal sign
#' @return numbers formatted
#' @examples
#' pn(n = c(1.23456, NA, 0, 0.00001, -0.00001), p = 8)
#' pn(n = c(-Inf, NA, NaN), p = 8)
#' @noRd
#' @keywords internal
pn <- function(n = NULL, p = 4L) {
  # n : numeric vector
  # p : precision after the decimal sign
  # output : numbers formatted in identical width as character using scientific notation for numbers < p and rounding to p otherwise
  if (any(is.finite(n))) {
    w <- max(nchar(round(n)), na.rm = TRUE) + p + 1 # determine maximum width required
    o <- rep(paste(rep(" ", w), collapse = ""), length(n))
    o[is.finite(n)] <- sprintf(paste0("%*.", p, "f"), w, n[is.finite(n)])
    s <- is.finite(n) & round(n, p) == 0 & abs(n) > 0 # requires scientific notation
    if (any(s)) o[which(s)] <- sprintf(paste0("%*.", max(c(p - 4, 1)), "E"), w, n[which(s)])
    return(o)
  } else {
    return(n)
  }
}

#' @title update_reactivecell.
#' @description Update single or multiple cells of the final `materialtabelle` with new values.
#' @param r Reactive containing the data frame to be updated.
#' @param colname Name of the column.
#' @param analyterow Name of the analyte-row. If NULL, whole column is updated.
#' @param value New value to be updated.
#' @return Nothing. Will update the data frame in the reactive `r`.
#' @noRd
#' @keywords internal
update_reactivecell <- function(r, colname, analyterow = NULL, value) {
  if (!is.data.frame(r())) {
    stop("r is not a data frame")
  }
  if (!colname %in% colnames(r())) {
    stop("reactive data frame does not contain column ", colname)
  }
  if (!is.null(analyterow) && nrow(merge(analyterow, r())) == 0) {
    stop("reactive data frame does not contain row ", analyterow)
  }
  if (is.data.frame(value)) {
    stop("value is a dataframe, but should be a scalar")
  }
  if (!is.null(analyterow) && length(value) > 1) {
    warning("value to be inserted is not scalar, i.e. more than one. Take only first!")
    value <- value[1]
  }

  # message("reactivecell: Update ",  deparse(substitute(r())), "; column: ", colname)
  # extract original row to be edit into variable (1/3)
  df <- r()
  if (is.null(analyterow)) {
    newRow <- df
  } else {
    newRow <- df[df[["analyte"]] == analyterow, ]
  }

  # edit cell (2/3)
  newRow[[colname]] <- value

  # update (3/3)
  if (is.null(analyterow)) {
    df <- newRow
  } else {
    df[df[["analyte"]] == analyterow, ] <- newRow
  }

  r(df)
}

#' @title to_startPage.
#' @description If called, function will redirect the user to page `Start`.
#' @param session session.
#' @param  value value.
#' @keywords internal
#' @noRd
to_startPage <- function(session, value = "Certification") {
  # this function will break if shiny input IDs get changed
  shiny::updateNavbarPage(session = session, inputId = "navbarpage", selected = "Start")
  shiny::updateSelectInput(session = session, inputId = "Start-excelfile-moduleSelect", selected = value)
}

#' @title listNames
#' @description Provides names of nested list elements, but ignores data frame column names.
#'     Refer to  <https://stackoverflow.com/q/68453593/6946122> for details.
#' @param l Nested list or R6 containing reactiveValues.
#' @param maxDepth The maximum depth, the names of list should be returned.
#' @param split Should the returning list be returned as nested  (TRUE) or as point-separated list (FALSE)?
#' @return A list if split == TRUE, otherwise a character vector of names.
#' @keywords internal
#' @noRd
#' @examples
#' test <- list(
#'   "b" = list(
#'     "df1" = data.frame(col = c(1, 2)),
#'     "e" = list(z = NULL)
#'   ),
#'   "c" = NULL,
#'   "df2" = data.frame(c12 = c(1, 2), c34 = c(3, 4))
#' )
#' listNames(test)
#' listNames(test, 3)
#' listNames(test, 3, TRUE)
listNames <- function(l, maxDepth = 2, split = FALSE) {
  if (R6::is.R6(l) | shiny::is.reactivevalues(l)) {
    # decompose first if it is a R6 object
    l <- sapply(l$get(), function(x) {
      if (shiny::is.reactivevalues(x)) shiny::reactiveValuesToList(x)
    })
  }
  depth <- 0
  listNames_rec <- function(l, depth) {
    if (!is.list(l) | is.data.frame(l) | depth >= maxDepth) {
      TRUE
    } else {
      depth <- depth + 1
      lapply(l, listNames_rec, depth)
    }
  }
  nms <- names(unlist(listNames_rec(l, depth)))
  if (split == TRUE) {
    nms <- strsplit(nms, split = ".", fixed = TRUE)
  }
  return(nms)
}

#' @title show_view
#' @description Show View Returns a list of panels, which are marked to be shown in the
#'   accordingly used RData from previous analysis. This is currently not evaluated
#'   but could be useful in the future. Keep for now and don't delete,
#' @param rv The eCerto R6 object.
#' @return A character vector of panels to be shown or more precisely the parent list names
#'   which contain a sub item 'show' that is set to TRUE.
#' @keywords internal
#' @noRd
#' @examples
#' rv <- eCerto::eCerto$new(eCerto:::init_rv()) # initiate persistent variables
#' shiny::isolate({
#'   setValue(rv, c("Certification_processing", "CertValPlot", "show"), TRUE)
#' })
#' print(eCerto:::show_view(rv))
#' shiny::isolate({
#'   setValue(rv, c("Certification_processing", "mstats", "show"), TRUE)
#' })
#' print(eCerto:::show_view(rv))
show_view <- function(rv) {
  nms <- shiny::isolate(listNames(rv, maxDepth = 3, split = TRUE))
  visible <- c()
  for (n in nms) {
    i <- any(n %in% "show")
    if (i && !is.null(shiny::isolate(getValue(rv, n))) && shiny::isolate(getValue(rv, n))) {
      visible <- c(visible, n[2])
    }
  }
  return(visible)
}

#' @title sub_header.
#' @description Format a text as bold paragraph and with specific left/bottom margin.
#' @param txt The sub_header text to format.
#' @param l Left margin in percent.
#' @param b Bottom margin left in percent.
#' @return HTML to include in UI.
#' @keywords internal
#' @noRd
#' @examples
#' eCerto:::sub_header("test")
#' eCerto:::sub_header("test", l = 5, b = 0)
sub_header <- function(txt = "test", l = 0, b = 5, unit = c("px", "%")) {
  u <- match.arg(unit)
  shiny::div(
    style = paste0("margin-left: ", l, u, "; margin-bottom: ", b, u, "; font-weight: 700"),
    txt
  )
}

#' @title calc_bxp_width.
#' @description Calculate the optimal width for Fig.C1 depending on the number of labs.
#' @param n Number of labs with finite values.
#' @param w_axes Number of pixels reserved for axes.
#' @param w_point Number of pixels reserved per data point.
#' @return Optimal figure width in pixels.
#' @keywords internal
#' @noRd
#' @examples
#' eCerto:::calc_bxp_width(n = 2)
#' eCerto:::calc_bxp_width(n = 20)
calc_bxp_width <- function(n, w_axes = 100, w_point = 40) {
  round(w_axes + (w_point * n) * 1.08)
}

#' @title h_statement.
#' @description Prepare a analyte specific statement regarding the homogeneity.
#' @param n Number of labs with finite values.
#' @param w_axes Number of pixels reserved for axes.
#' @param w_point Number of pixels reserved per data point.
#' @return Optimal figure width in pixels.
#' @keywords internal
#' @noRd
#' @examples
#' h <- eCerto:::prepTabH1(x = eCerto:::test_homog()$data)
#' eCerto:::h_statement(x = h, a = "Fe.axial")
h_statement <- function(x, a) {
  P_col <- ifelse("P" %in% colnames(x), "P", "P_adj")
  stopifnot(c("analyte", "H_type", P_col, "s_bb", "s_bb_min") %in% colnames(x))
  idx <- interaction(x[, "analyte"], x[, "H_type"]) == a
  a_name <- ifelse(length(unique(x[, "H_type"])) == 1, as.character(x[idx, "analyte"]), a)
  a_sd <- max(x[idx, c("s_bb", "s_bb_min")])
  a_type <- ifelse(names(which.max(x[idx, c("s_bb", "s_bb_min")])) == "s_bb", "s<sub>bb</sub>", "s<sub>bb,min</sub>")
  a_P <- x[idx, P_col]
  if (a_P < 0.05) {
    s1 <- "<font color=\"#FF0000\"><b>significantly different</b></font>"
    s2 <- "<b>Please check your method and data!</b>"
  } else {
    s1 <- "<font color=\"#00FF00\">not significantly different</font>"
    s2 <- ""
  }
  return(
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::HTML(
          "The tested items are ", s1, "(ANOVA", ifelse(P_col == "P_adj", "P-value<sub>adj</sub>", "P-value"), "=", pn(a_P, 2), "using alpha-level = 0.05).",
          "<br>The uncertainty value for analyte<b>", a_name, "</b>was determined as<b>", a_type, "=", pn(a_sd), "</b>.", s2
        )
      )
    )
  )
}

#' @title get_UF_cols.
#' @description Helper function to get column indexes for U and F columns in Tab.C3.
#' @param mt materialtabelle.
#' @param type Code for the specific column set to retrieve.
#' @return Named vector of column inices.
#' @keywords internal
#' @noRd
#' @examples
#' mt <- eCerto:::init_materialtabelle(LETTERS[1:3])
#' eCerto:::get_UF_cols(mt = mt, type = "F")
#' eCerto:::get_UF_cols(mt = mt, type = "U_round")
get_UF_cols <- function(mt = NULL, type = c("U", "F", "U_round")[1]) {
  u_calc_cols <- "u_char"
  f_calc_cols <- "mean"
  u_round_cols <- c("u_char", "u_com", "U")
  if (!is.null(attr(mt, "col_code"))) {
    cc <- attr(mt, "col_code")
    # if user defined U cols are present
    if (any(grep("U", cc[, "ID"]))) {
      idx <- grep("U", cc[, "ID"])
      add_cols <- NULL
      if (any(cc[idx, "ID"] %in% colnames(mt))) add_cols <- cc[idx, "ID"]
      if (any(cc[idx, "Name"] %in% colnames(mt))) add_cols <- cc[idx, "Name"]
      u_calc_cols <- c(u_calc_cols, add_cols)
      u_round_cols <- c(u_round_cols, add_cols)
    }
    # if user defined F cols are present
    if (any(grep("F", cc[, "ID"]))) {
      idx <- grep("F", cc[, "ID"])
      add_cols <- NULL
      if (any(cc[idx, "ID"] %in% colnames(mt))) add_cols <- cc[idx, "ID"]
      if (any(cc[idx, "Name"] %in% colnames(mt))) add_cols <- cc[idx, "Name"]
      f_calc_cols <- c(f_calc_cols, add_cols)
    }
  }
  switch(type,
    "U" = unlist(sapply(u_calc_cols, function(x) {
      which(colnames(mt) == x)
    })),
    "U_round" = unlist(sapply(u_round_cols, function(x) {
      which(colnames(mt) == x)
    })),
    "F" = unlist(sapply(f_calc_cols, function(x) {
      which(colnames(mt) == x)
    }))
  )
}

#' @title get_input_data.
#' @description Helper function to get input data for an analyte.
#' @param rv rv.
#' @param type type.
#' @param excl_file excl_file.
#' @return Input data frame in either full or compact version.
#' @keywords internal
#' @noRd
#' @examples
#' rv <- eCerto:::test_rv(type = "SR3")
#' isolate(get_input_data(rv = rv))
#' isolate(get_input_data(rv = rv, excl_file = TRUE))
#' isolate(get_input_data(rv = rv, type = "s"))
#' isolate(get_input_data(rv = rv, type = "s", excl_file = TRUE))
get_input_data <- function(rv, type = c("compact", "standard"), excl_file = FALSE) {
  type <- match.arg(type)
  df <- getValue(rv, c("Certification", "data"))
  an <- rv$cur_an
  df <- df[df[, "analyte"] == an, ]
  if (!"File" %in% colnames(df)) df <- cbind(df, "File" = "")
  if (type == "compact") {
    # ensure that "Lab" is a factor
    if (!is.factor(df[, "Lab"])) df[, "Lab"] <- factor(df[, "Lab"], levels = unique(df[, "Lab"]))
    fn <- rv$c_lab_codes()
    p <- rv$a_p("precision")[an]
    n_reps <- as.character(sort(unique(df$replicate)))
    if (min(as.numeric(n_reps))!=1) warning("No replicate with ID=1 found. Please check import data format (probably an additional column is present).")
    data <- plyr::ldply(split(df, df$Lab), function(x) {
      out <- rep(NA, length(n_reps))
      out[x$replicate] <- x$value
      matrix(out, ncol = length(n_reps), dimnames = list(NULL, paste0("R", n_reps)))
    }, .id = "Lab")
    id_idx <- plyr::ldply(split(df, df$Lab), function(x) {
      out <- rep(NA, length(n_reps))
      out[x$replicate] <- x$ID
      matrix(out, ncol = length(n_reps), dimnames = list(NULL, paste0("R", n_reps)))
    }, .id = "Lab")
    out <- data.frame(
      data[, "Lab", drop = F],
      round(data[, -1, drop = F], digits = p),
      "mean" = round(apply(data[, -1, drop = F], 1, mean, na.rm = T), digits = p),
      "sd" = round(apply(data[, -1, drop = F], 1, stats::sd, na.rm = T), digits = p),
      "File" = unname(fn[levels(df$Lab)])
    )
    if (excl_file) {
      out <- out[, -which(colnames(out) == "File")]
    }
    attr(out, "id_idx") <- id_idx
    return(out)
  } else {
    df <- df[, c("ID", "Lab", "value", "unit", "replicate", "File")]
    if (excl_file) {
      df <- df[, -which(colnames(df) == "File")]
    }
    return(df)
  }
}

#' @title color_temperature_levels.
#' @description Calculate the optimal width for Fig.C1 depending on the number of labs.
#' @param x Numeric vector of temperatures.
#' @return data.frame with pch and bg information for vector x.
#' @keywords internal
#' @noRd
#' @examples
#' eCerto:::color_temperature_levels(x = c(-80, -80, 4, 4, 4, 23))
color_temperature_levels <- function(x) {
  if (!is.numeric(x)) {
    x <- try(as.numeric(x), silent = TRUE)
    if (inherits(x, "try-error")) stop("[color_temperature_levels] Can not convert data to numeric.")
  }
  temps <- cut(x, breaks = c(-274, -80, -20, 4, 23, 40, 60, 1000))
  temp_cols <- c("darkblue", "#1b98e0", "lightblue", "yellow", "orange", "red", "darkred")
  temp_pchs <- c(24, 21:23, 22, 21, 25)
  return(data.frame("Temp" = x, "pchs" = temp_pchs[temps], "cols" = temp_cols[temps]))
}

#' @title orderPvalue.
#' @description Groups means from Scheffe-test according to P-value.
#' @param means means.
#' @param alpha alpha.
#' @param pmat pmat.
#' @return Input data frame in either full or compact version.
#' @keywords internal
#' @noRd
#' @examples
#' rv <- eCerto:::test_rv(type = "SR3")
#' isolate(get_input_data(rv = rv))
#' isolate(get_input_data(rv = rv, excl_file = TRUE))
#' isolate(get_input_data(rv = rv, type = "s"))
#' isolate(get_input_data(rv = rv, type = "s", excl_file = TRUE))
orderPvalue <- function(means, alpha, pmat) {
  # helper functions
  last_char <- function(x) {
    x <- sub(" +$", "", x)
    return(substr(x, nchar(x), nchar(x)))
  }
  symb <- c(letters[1:26], LETTERS[1:26], rep(" ", 2000))
  n <- nrow(means)
  idx <- (1:n)[order(means[, 2], decreasing = TRUE)]
  w <- means[order(means[, 2], decreasing = TRUE), ]
  M <- rep("", n)
  k <- 1
  j <- 1
  i <- 1
  cambio <- n
  cambio1 <- 0
  chk <- 0
  M[1] <- symb[k]
  while (j < n) {
    chk <- chk + 1
    if (chk > n) {
      break
    }
    for (i in j:n) {
      if (pmat[idx[i], idx[j]] > alpha) {
        if (last_char(M[i]) != symb[k]) {
          M[i] <- paste0(M[i], symb[k])
        }
      } else {
        k <- k + 1
        cambio <- i
        cambio1 <- 0
        ja <- j
        M[cambio] <- paste0(M[cambio], symb[k])
        for (v in ja:cambio) {
          if (pmat[idx[v], idx[cambio]] <= alpha) {
            j <- j + 1
            cambio1 <- 1
          } else {
            break
          }
        }
        break
      }
    }
    if (cambio1 == 0) {
      j <- j + 1
    }
  }
  output <- data.frame("mean" = as.numeric(w[, 2]), groups = M, row.names = as.character(w[, 1]))
  if (k > 52) {
    message("\nThe number of estimated groups (", k, ") exceeded the maximum number of available labels (52).\n")
  }
  invisible(output)
}

#' @title encode_fmt.
#' @description Helper function to get the correct fmt for Tab.C1 styling.
#' @param rv rv.
#' @return Character vector of length 1.
#' @keywords internal
#' @noRd
encode_fmt <- function(x) {
  switch(x,
    "Significance level" = "alpha",
    "P-value" = "pval",
    "Test statistic" = "cval",
    "Critical value a=0.05" = "cval05",
    "Critical value a=0.01" = "cval01"
  )
}

#' @title welcome_screen.
#' @description Show the start-up welcome_screen.
#' @param id id.
#' @return tagList/HTML.
#' @keywords internal
#' @noRd
welcome_screen <- function(id = id) {
  ns <- shiny::NS(id)
  card_style <- "background-color: var(--_sidebar-bg);"
  shiny::tagList(
    bslib::layout_columns(
      shiny::tagList(
        bslib::card(
          style = "background-color: rgb(0,175,240); color: white; text-shadow: 2px 2px 0px #D2001E;",
          bslib::layout_columns(
            col_widths = c(2, 10),
            shiny::img(src = "www/hex-eCerto.png", alt = "eCerto Hex-Logo", margin = "auto", width = "90%"),
            shiny::div(
              # shiny::div(style = "font-size: large;", "Are you looking for a software to compute statistical tests on data generated in Reference Material production?"),
              # shiny::div(style = "font-size: xx-large;", "Welcome to eCerto!")
              shiny::h3("Are you looking for a software to compute statistical tests on data generated in Reference Material production?"),
              shiny::h1(shiny::HTML("Welcome to <i>eCerto</i>!"))
            )
          )
        )
      ),
      shiny::tagList(
        bslib::layout_columns(col_widths = 6,
          bslib::card(
            style = card_style,
            shiny::div("Click on", shiny::actionLink(inputId = ns("getHelp"), label = shiny::HTML("<strong>this Link</strong>")), shiny::HTML("when you are <span style='color: red;'>a first time user</span> to get help!"))
          ),
          bslib::card(
            style = card_style,
            shiny::div("Read the extensive", shiny::actionLink(inputId = ns("showHelp"), label = shiny::HTML("<strong>Online Help</strong>")), shiny::HTML("(see top menue) if you want to know everything!"))
          ),
          bslib::card(
            style = card_style,
            shiny::div(shiny::HTML("Open some <strong>Test Data</strong> using the 'Load Test Data' button in the menu! You also can import a real life data set from Zenodo"))
          ),
          bslib::card(
            style = card_style,
            shiny::div(shiny::HTML("Import your own data from <strong>Excel files</strong> slecting the 'File format' and using the 'Browse' button at the top!"))
          )
        )
      ),
      col_widths =  bslib::breakpoints(
        sm = c(12, 12),
        xl = c(6, 6)
      )
    )
  )
}

#' @title verify_suggested.
#' @description Check if packages are available and stop function otherwise.
#' @param pkg Package names to be checked.
#' @return NULL.
#' @keywords internal
#' @noRd
verify_suggested <- function(pkg) {
  # verify that suggested packages are available
  check_pkg <- sapply(pkg, requireNamespace, quietly = TRUE)
  if (!all(check_pkg)) {
    msg <- paste0(
      "The use of this function requires package", ifelse(sum(!check_pkg) > 1, "s", ""),
      paste(names(check_pkg)[!check_pkg], collapse = ", "),
      ". Please install."
    )
    stop(msg)
  }
  invisible(NULL)
}

#' @title auto_fill.
#' @description Fill all empty values (NA) of a vector with the last valid value.
#' @param x Vector of values possibly containing NA values.
#' @return NULL.
#' @keywords internal
#' @noRd
auto_fill <- function(x) {
  stopifnot(length(x)>=2)
  stopifnot(!is.na(x[1]))
  idx <- is.na(x)
  if (any(idx)) {
    for (i in which(idx)) x[i] <- x[i-1]
  }
  return(x)
}

