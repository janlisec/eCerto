#' @title RDataImport.
#'
#' @description \code{RDataImport} will provide a module to upload/backup Rdata
#'   files for certification trial data.
#'
#' @param id Name when called as a module in a shiny app.
#' @param rv The eCerto object.
#'
#' @return A reactive object `rdata`, but returned only for notifying the side effect
#'     to notiy the navbarpanel to update.
#'
#' @examples
#' if (interactive()) {
#' rv <- eCerto$new()
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(eCerto:::m_RDataImport_UI(id = "test")),
#'  server = function(input, output, session) {
#'    eCerto:::m_RDataImport_Server(id = "test", rv = rv)
#'    shiny::observeEvent(rv$e_present(), { print(rv$e_present()) })
#'  }
#' )
#' }
#'
#' @noRd
#' @keywords internal
m_RDataImport_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fileInput(
      inputId = ns("in_file_ecerto_backup"),
      label = "Load Previous Analysis",
      multiple = FALSE,
      accept = c("RData")
    )
  )
}

#' @noRd
#' @keywords internal
m_RDataImport_Server = function(id, rv) {

  shiny::moduleServer(id, function(input, output, session) {

    rvreturn <- shiny::reactiveVal(NULL)
    continue <- shiny::reactiveVal(NULL) # NULL -> don't continue

    ns <- session$ns
    silent <- get_golem_config("silent")

    # Upload
    rdata <- shiny::eventReactive(input$in_file_ecerto_backup, {
      file.type <- tools::file_ext(input$in_file_ecerto_backup$datapath)
      shiny::validate(
        shiny::need(tolower(file.type) == "rdata","Only RData allowed."),
        shiny::need(length(file.type) == 1, "Please select only one RData file.")
      )
      file.type <- "RData"
      load_envir <- new.env()
      tryCatch({
        load(input$in_file_ecerto_backup$datapath[1], envir = load_envir)
      }, error = function(e) {
        stop(shiny::safeError(e))
      })
      # check if 'res' is contained in loaded workspace
      obj <- ls(envir = load_envir)
      obj <- obj[obj %in% "res"]
      return(base::get(x = obj, envir = load_envir))
    }, ignoreNULL = TRUE)

    # Is anything already uploaded via Excel? If so, show Window Dialog
    shiny::observeEvent(rdata(), {
      test <- rv$e_present()
      if (any(test)){
        if (!silent) message("[RDataImport] Found existing data. Overwrite?")
        if (!is.null(shiny::getDefaultReactiveDomain())) {
          shiny::showModal(
            shiny::modalDialog(
              title = "Existent data",
              shiny::HTML("Modul(s) <u>", paste(names(which(test)), collapse=", "), "</u> are already existent. Are you sure you want to continue?"),
              footer = shiny::tagList(
                shiny::actionButton(inputId = ns("cancel"), label = "Cancel"),
                shiny::actionButton(inputId = ns("overwrite"), label = "Overwrite", class = "btn btn-danger")
              )
            )
          )
        }
      } else {
        if (!silent) message("[RDataImport] RData uploaded")
        continue(TRUE)
      }
    })

    # the observers from before
    # shall be overwritten?
    shiny::observeEvent(input$overwrite, {
      continue(TRUE)
      shiny::showNotification("Overwritten")
      shiny::removeModal()
    })
    # shall be cancelled?
    shiny::observeEvent(input$cancel, {
      shiny::removeModal()
    })

    shiny::observeEvent(continue(), {
      tmp <- fnc_load_RData(x = rdata())
      continue(NULL)
      rvreturn(tmp)
    }, ignoreNULL = TRUE)

    shiny::observeEvent(rvreturn(),{
      rv_rdatanames <- listNames(rvreturn(), split = TRUE)
      # overwrite
      for (n in rv_rdatanames) {
        setValue(rv, n, getValue(rvreturn(), n))
      }
      # set current analyte to trigger C Modul elements
      rv$cur_an <- unname(rv$a_p("name")[1])
    }, ignoreNULL = TRUE)

  })

}

