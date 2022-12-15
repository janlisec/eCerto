#' @title RDataImport.
#'
#' @description \code{RDataImport} will provide a module to upload/backup Rdata
#'   files for certification trial data.
#'
#' @param id Name when called as a module in a shiny app.
#' @param modules c("Certification", "Homogeneity", "Stability").
#' @param uploadsources Indicates the uploadsource of each \code{module}. For example
#'     if Certification has been uploaded, the argument would look like
#'     reactiveVal(list("Certification"="Excel", "Homogeneity"=NULL, "Stability"=NULL)).
#'
#' @return A reactive object `rdata`, but returned only for notifying the side effect
#'     to notiy the navbarpanel to update.
#'
#' @examples
#' if (interactive()) {
#' shiny::shinyApp(
#'  ui = shiny::fluidPage(eCerto:::m_RDataImport_UI(id = "test")),
#'  server = function(input, output, session) {
#'    eCerto:::m_RDataImport_Server(
#'      id = "test",
#'      modules = reactiveVal(c("Certification","Stability","Homogeneity")),
#'      uploadsources = reactiveVal(list("Certification" = "Excel"))
#'    )
#'  }
#' )
#' }
#'
#' @noRd
#' @keywords internal
m_RDataImport_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::wellPanel(
      sub_header("Load", b=0),
      shiny::fileInput(
        inputId = ns("in_file_ecerto_backup"),
        label = "Select Previous Analysis",
        multiple = FALSE,
        accept = c("RData")
      )
    )
  )
}

#' @noRd
#' @keywords internal
m_RDataImport_Server = function(id, modules, uploadsources) {

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
        shiny::need(length(file.type) == 1,"Please select only one RData file.")
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
      test <- sapply(modules(), function(x) {!is.null(uploadsources()[[x]])}, simplify = "array")
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
      rv <- fnc_load_RData(x = rdata())
      continue(NULL)
      #browser()
      rvreturn(rv)
    }, ignoreNULL = TRUE)

    return(rvreturn)

  })

}

