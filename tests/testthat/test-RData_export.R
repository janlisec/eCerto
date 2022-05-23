testthat::test_that(
  desc = "RDataExport can set user and study_id correctly",
  code = {
    rv_test <- eCerto::eCerto$new(eCerto::init_rv())
    suppressMessages(
      shiny::testServer(
        app = eCerto::m_RDataexport_Server,
        args = list(
          rv = rv_test
        ),
        expr = {
          session$setInputs(study_id = "S", user = "U")
          testthat::expect_equal(input$user, getValue(rv_test, c("General", "user")))
          testthat::expect_equal(input$study_id, getValue(rv_test, c("General", "study_id")))
          # ToDo: because we have only access to the server part in testthat
          # we can not (!) test if the update function for the text input works as well
          # we would need a shinytest miniapp for this purpose as described in:
          # https://community.rstudio.com/t/how-do-i-test-an-observeevent-that-updates-an-input-how-do-i-test-a-module-using-shinytest/100537/2
          # <-- works only in miniapp
          # session$flushReact()
          # setValue(rv_test, c("General", "user"), "U_new")
          # setValue(rv_test, c("General", "study_id"), "S_new")
          # session$flushReact()
          # testthat::expect_equal(input$study_id, "S_new")
          # -->
        }
      )
    )
  }
)

# test_that(
#   desc = "RDataExport works for General$user",
#   code = {
#     rv_test <- eCerto::eCerto$new(eCerto::init_rv())
#     shiny::isolate({setValue(rv_test, c("Certification","data"), test_Certification_Excel()) })
#     shiny::isolate({setValue(rv_test, c("General", "user"), "FK4") })
#     shiny::isolate({set_uploadsource(rv_test, "Certification", uploadsource = "Excel") })
#
#     # suppressMessages(
#     shiny::testServer(
#       app = eCerto::m_RDataexport_Server,
#       args = list(
#         rv = rv_test
#       ),
#       expr = {
#
#         session$flushReact()
#         session$setInputs(ecerto_backup = "click")
#         session$flushReact()
#         print(input$user)
#       }
#     )
#     # )
#   })

