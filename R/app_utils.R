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
      warning_or_modal("Please select only Excel (.xlsx) files.")
      return(NULL)
    }
  })
  if (length(unique(a)) != 1) {
    warning_or_modal("Sheet names are different within files.")
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
#' mt <- cbind(mt, data.frame("F1"=rep(1,3), "U1"=rep(0,3)))
#' attr(mt, "col_code") <- data.frame("ID"=c("F1","U1"), "Name"=c("F_name","U_name"))
#' eCerto:::get_UF_cols(mt)
#' eCerto:::get_UF_cols(mt, type="F")
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
    data <- ldply_base(split(df, df$Lab), function(x) {
      out <- rep(NA, length(n_reps))
      out[x$replicate] <- x$value
      matrix(out, ncol = length(n_reps), dimnames = list(NULL, paste0("R", n_reps)))
    }, .id = "Lab")
    id_idx <- ldply_base(split(df, df$Lab), function(x) {
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
    e_msg(paste0("The number of estimated groups (", k, ") exceeded the maximum number of available labels (52)."))
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
#' @return Vector of values without NA values (which are substituted).
#' @keywords internal
#' @noRd
auto_fill <- function(x, also_fill = c("")) {
  stopifnot(length(x)>=2)
  stopifnot(!is.na(x[1]))
  idx <- is.na(x) | x %in% also_fill
  if (any(idx)) {
    for (i in which(idx)) x[i] <- x[i-1]
  }
  return(x)
}

#' @title get_fun_name.
#' @description Get the name of a calling function.
#' @param n Function level to go up.
#' @return Character.
#' @keywords internal
#' @noRd
get_fun_name <- function (n = 0) {
  cur_call <- sys.call(sys.parent(n + 1))
  return(as.character(cur_call)[1])
}

#' @title e_msg.
#' @description Prepare a standard message to be shown in the console during an
#'     R-Shiny app being active. The message will be amended by information on
#'     the environment (current function or module/observer) to facilitate
#'     error traceback.
#' @details Will return NULL in case that golem config silent = TRUE.
#' @param x A character vector containing the message parts.
#' @return Character.
#' @keywords internal
#' @noRd
e_msg <- function(x) {
  if (get_golem_config("silent")) {
    invisible(NULL)
  } else {
    curr_mod <- NA
    curr_fnc <- get_fun_name(n=1)
    curr_fnc <- rev(strsplit(curr_fnc, "\n")[[1]])[1]
    if (curr_fnc %in% c("observe", "<reactive>", "eventReactiveValueFunc")) {
      if (requireNamespace("rlang", quietly = TRUE)) {
        y <- rlang::trace_back(globalenv())
        i <- length(y$call)
        y <- attr(attr(y$call[[i]], "srcref"), "srcfile")
        curr_mod <- basename(y$filename)
      }
    }
    message("[", curr_fnc, "]: ", x, ifelse(is.na(curr_mod), "", paste0(" (", curr_mod, ")")))
  }
}

#' @title warning_or_modal.
#' @description Return a modal with an error message when called in a Shiny-App
#'     or return a e_msg otherwise.
#' @param x A character vector containing the message parts.
#' @param type modal type.
#' @return Character.
#' @keywords internal
#' @noRd
warning_or_modal <- function(x, type = c("warning", "info", "success", "error")) {
  type <- match.arg(type)
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    shinyWidgets::show_alert(
      title = NULL,
      type = "error",
      text = x,
      closeOnClickOutside = TRUE
    )
  } else {
    warning(x)
    e_msg(x)
  }
}

#' @title decimal_count.
#' @description Function to count the number of digits on the right of
#'     a decimal point (sometimes called the mantissa).
#' @param x A numeric.
#' @examples
#' \dontrun{
#'   decimal_count(5.89)
#'   sapply(c(5.89, 2, 56.454545, NA, 5.1), decimal_count)
#' }
#' @return Numeric.
#' @keywords internal
#' @noRd
decimal_count <- function(x) {
  # Check
  stopifnot(class(x) == "numeric")
  # If NA, return NA
  if (is.na(x)) {
    x <- NA_integer_
  } else {
    # If contains a period
    if (grepl("\\.", x)) {
      x <- gsub("0+$", "", x)
      x <- gsub("^.+[.]", "", x)
      x <- nchar(x)
    } else {
      x <- 0L
    }
  }
  return(x)
}

#' @title HTML2markdown.
#' @description Function to convert HTML tags into the markdown equivalent.
#' @param x A character vector.
#' @examples
#' \dontrun{
#'   x <- c("x<sub>i</sub>", "This is <i>formatted</i> <b>HTM<sup>L</sup></b>")
#'   HTML2markdown(x)
#' }
#' @return Numeric.
#' @keywords internal
#' @noRd
HTML2markdown <- function(x) {
  # Checks
  stopifnot(class(x) == "character")
  x <- gsub("<i>", "*", x)
  x <- gsub("</i>", "*", x)
  x <- gsub("<sub>", "~", x)
  x <- gsub("</sub>", "~", x)
  x <- gsub("<sup>", "^", x)
  x <- gsub("</sup>", "^", x)
  x <- gsub("<b>", "**", x)
  x <- gsub("</b>", "**", x)
  x <- gsub("<strong>", "**", x)
  x <- gsub("</strong>", "**", x)
  return(x)
}

#' @title render_report
#' @description generic render function
#' @param file output file.
#' @param fmt output format of render.
#' @param rmd_template rmd_template.
#' @param params parameter list provided to rmd_template.
#' @param quiet Set FALSE to get rmarkdown messages printed to the console.
#' @keywords internal
#' @noRd
render_report <- function(file = "filename", fmt = NULL, rmd_template, params = list(), quiet = TRUE) {
  if (is.character(fmt) && length(fmt)==1) {
    fmt <- switch(
      fmt,
      "html" = rmarkdown::html_document(),
      "docx" = rmarkdown::word_document(),
      "pdf" = rmarkdown::pdf_document(),
      fmt)
  }
  do_render <- function() {
    rmarkdown::render(
      input = rmd_template,
      output_file = file,
      output_format = fmt,
      params = params,
      envir = new.env(parent = globalenv()),
      quiet = quiet
    )
  }
  if (!is.null(shiny::getDefaultReactiveDomain())) {
    shiny::withProgress(expr = {
        shiny::incProgress(0.5)
        do_render()
      }, message = "Rendering Report.."
    )
  } else {
    do_render()
  }
  return(file)
}

#' @title render_report_V
#' @param file description
#' @param inp_data description
#' @param V_pars description
#' @examples
#' \dontrun{
#'   # the following code will generate a Validation report from a template in a temporary file
#'   fl <- system.file("extdata", "eCerto_Testdata_VModule.xlsx", package = "eCerto")
#'   inp_data <- eCerto:::read_Vdata(file = fl, fmt = eCerto:::check_fmt_Vdata(fl))
#'     V_pars <- list(
#'       "opt_figV1_anal" = "PFBA",
#'       "opt_figV1_level" = c(1,8),
#'       "opt_tabV1_precision" = 4,
#'       "opt_tabV1_alpha" = 0.5,
#'       "opt_tabV1_k" = 3,
#'       "opt_tabV1_unitcali" = "",
#'       "opt_tabV1_unitsmpl" = "",
#'       "opt_tabV1_convfac" = 1,
#'       "opt_tabV1_fltLevels" = FALSE,
#'       "txt_trueness" = "txt_trueness",
#'       "txt_precision" = "txt_precision",
#'       "txt_uncertainty" = "txt_uncertainty",
#'       "opt_tabV1_useLevels" = TRUE,
#'       "opt_tabV1_useAnalytes" = TRUE
#'     )
#'     rep_fl <- render_report_V(inp_data=inp_data, V_pars=V_pars)
#'     readLines(rep_fl, n=10)
#' }
#' @keywords internal
#' @noRd
render_report_V <- function(file = tempfile(fileext = ".html"), inp_data, V_pars) {
  fmt <- tolower(tools::file_ext(file))
  params <- list(
    "inp_data" = inp_data,
    "logo_file" = "BAMLogo2015.png",
    "V_pars" = V_pars,
    "helptext_v_fig_V1" = readLines(get_local_file("v_fig_V1.[Rr][Mm][Dd]$")),
    "helptext_v_tab_V1" = readLines(get_local_file("v_tab_V1.[Rr][Mm][Dd]$")),
    "helptext_v_formula_collection" = readLines(get_local_file("v_formula_collection.[Rr][Mm][Dd]$"))
  )
  rmd_template <- get_local_file("report_vorlage_validation.[Rr][Mm][Dd]$")
  render_report(file = file, fmt = fmt, rmd_template = rmd_template, params = params)
}

#' @title render_report_M.
#' @param file filename.
#' @param mt materialtabelle.
#' @param gen General parameters.
#' @keywords internal
#' @noRd
render_report_M <- function(file = tempfile(fileext = ".html"), mt, gen) {
  fmt <- tolower(tools::file_ext(file))
  params = list(
    "materialtabelle" = mt,
    "General" = gen,
    "logo_file" = "BAMLogo2015.png"
  )
  rmd_template <- get_local_file("report_vorlage_material.[Rr][Mm][Dd]$")
  render_report(file = file, fmt = fmt, rmd_template = rmd_template, params = params)
}

#' @title render_report_A.
#' @param file filename.
#' @param rv rv.
#' @keywords internal
#' @noRd
render_report_A <- function(file = tempfile(fileext = ".html"), rv) {
  fmt <- tolower(tools::file_ext(file))
  params = list(
    "General" = shiny::reactiveValuesToList(getValue(rv, "General")),
    "Certification" = shiny::reactiveValuesToList(getValue(rv, "Certification")),
    "Certification_processing" = shiny::reactiveValuesToList(getValue(rv, "Certification_processing")),
    "selected_tab" = rv$cur_an,
    "logo_file" = "BAMLogo2015.png"
  )
  rmd_template <- get_local_file("report_vorlage_analyt.[Rr][Mm][Dd]$")
  render_report(file = file, fmt = fmt, rmd_template = rmd_template, params = params)
}

#' @title render_report_H.
#' @param file filename.
#' @param rv rv.
#' @param xlab xlab.
#' @param adjust adjust.
#' @keywords internal
#' @noRd
render_report_H <- function(file = tempfile(fileext = ".html"), rv, xlab, adjust) {
  fmt <- tolower(tools::file_ext(file))
  params = list(
    "Homogeneity" = shiny::reactiveValuesToList(getValue(rv, "Homogeneity")),
    "xlab" = xlab,
    "precision" = rv$a_p("precision"),
    "adjust" = adjust
  )
  rmd_template <- get_local_file("report_vorlage_homogeneity.[Rr][Mm][Dd]$")
  render_report(file = file, fmt = fmt, rmd_template = rmd_template, params = params)
}

#' @title render_report_S.
#' @param file filename.
#' @param rv rv.
#' @param s_dat s_dat.
#' @param s_pars s_pars.
#' @param U_def U_def.
#' @param t_cert t_cert.
#' @param p_type p_type.
#' @keywords internal
#' @noRd
render_report_S <- function(file = tempfile(fileext = ".html"), rv, s_dat, s_pars, U_def, t_cert, p_type) {
  fmt <- tolower(tools::file_ext(file))
  params = list(
    "Stability" = shiny::reactiveValuesToList(getValue(rv, "Stability")),
    "Options" = list(
      "s_Data" = s_dat[!(rownames(s_dat) %in% s_pars$s_samples_filtered),],
      "apm" = getValue(rv, c("General", "apm")),
      "U_Def" = U_def,
      "mt" = getValue(rv, c("General", "materialtabelle")),
      "type" = p_type,
      "t_cert" = t_cert,
      "slope_of_means" = s_pars$slope_of_means,
      "show_legend" = s_pars$show_legend,
      "show_ids" = s_pars$show_ids
    )
  )
  rmd_template <- get_local_file("report_vorlage_stability.[Rr][Mm][Dd]$")
  render_report(file = file, fmt = fmt, rmd_template = rmd_template, params = params)
}

#' @title render_report_L.
#' @param file filename.
#' @param x x.
#' @keywords internal
#' @noRd
render_report_L <- function(file = tempfile(fileext = ".pdf"), x) {
  # this needs to be "pdf_document", so that render will switch to lualatex as specified in Rmd template
  fmt <- "pdf_document"
  # remove filtered values from report (if any)
  if (any(x[[1]][["val"]][,"Filter"])) {
    x[[1]][["val"]] <- x[[1]][["val"]][!x[[1]][["val"]][,"Filter"],]
  }
  params <- list(
    "dat" = x
  )
  rmd_template <- get_local_file("report_vorlage_lts.[Rr][Mm][Dd]$")
  render_report(file = file, fmt = fmt, rmd_template = rmd_template, params = params)
}

#' @title render_report_D.
#' @param file filename.
#' @param D D.
#' @keywords internal
#' @noRd
render_report_D <- function(file = tempfile(fileext = ".pdf"), D) {
  fmt <- tolower(tools::file_ext(file))
  params <- list(
    "D" = D,
    "logo_file" = "BAMLogo2015.png"
  )
  rmd_template <- get_local_file("report_vorlage_drmd.[Rr][Mm][Dd]$")
  render_report(file = file, fmt = fmt, rmd_template = rmd_template, params = params)
}

#' @title ldply_base
#' @description A base R implementation of plyr::ldply
#' @param .data A list or vector.
#' @param .fun Function to apply to each item.
#' @param .progress Show progress bar if 'text'.
#' @param .id Name of the index column (used if .data is a named list). Pass NULL to avoid creation of the index column. For compatibility, omit this argument or pass NA to avoid converting the index column to a factor; in this case, ".id" is used as colum name.
#' @param ... Arguments to .fun.
#' @examples
#' x <- list(a = data.frame(x = 1:2, y = 5:6), b = data.frame(x = 3:4, y = 7:8))
#' ldply_base(x)
#' ldply_base(x, .id = NULL)
#' ldply_base(unname(x))
#' ldply_base(x, .id = "test")
#' # compare against standard plyr::ldply
#' #plyr::ldply(x, .id="test")
#' str(ldply_base(x))
#' #str(plyr::ldply(x))
#' x <- c("01.01.2025","02.01.2025")
#' ldply_base(x, as.Date.character, tryFormats = "%d.%m.%Y")
#' #plyr::ldply(x, as.Date.character, tryFormats = "%d.%m.%Y")
#' @export
ldply_base <- function(.data, .fun = identity, .progress = "none", .id = NA, ...) {
  n <- length(.data)

  if (is.character(.fun)) { .fun <- match.fun(.fun) }

  if (.progress == "text") { pb <- utils::txtProgressBar(min = 0, max = n, style = 3) }

  result <- vector("list", n)
  for (i in seq_along(.data)) {
    if (.progress == "text") utils::setTxtProgressBar(pb, i)
    result[[i]] <- .fun(.data[[i]], ...)
  }

  if (.progress == "text") close(pb)

  # are all elements atomic and no matrix
  if (all(vapply(result, is.atomic, logical(1))) && !any(vapply(result, is.matrix, logical(1)))) {
    # do all elements share the same class
    classes <- vapply(result, function(x) paste(class(x), collapse = ","), character(1))
    if (length(unique(classes)) == 1) {
      # combine and re-assign class
      combined <- unlist(result, use.names = FALSE)
      class(combined) <- strsplit(unique(classes), ",")[[1]]
      df <- data.frame("V1" = combined, row.names = NULL, check.names = FALSE)
    } else {
      # fall back case for different classes
      df <- data.frame(lapply(do.call(rbind, lapply(result, as.list)), I), row.names = NULL, check.names = FALSE)
    }
  } else {
    # complex elements
    df <- do.call(rbind, lapply(result, function(x) {
      if (is.atomic(x) && !is.matrix(x)) {
        as.data.frame(as.list(x), check.names = FALSE)
      } else {
        as.data.frame(x, check.names = FALSE)
      }
    }))
    rownames(df) <- NULL

  }

  # add .id column if .id is NA and list is named; omit id column for .id=NULL in any case
  if (!is.null(.id)) {
    if (!is.na(.id) | is.na(.id) && !is.null(names(.data))) {
      if (!is.null(names(.data))) {
        ids <- rep(names(.data), times=sapply(result,nrow))
        if (!is.na(.id)) ids <- factor(ids)
      } else {
        ids <- rep(seq_len(n), times = sapply(result, nrow))
      }
      df <- cbind(stats::setNames(data.frame(ids), ifelse(is.na(.id), ".id", .id)), df)
    }
  }

  return(df)
}