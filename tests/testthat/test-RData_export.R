test_that(
  desc = "RDataExport sets user",
  code = {
    rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
    shiny::isolate({setValue(rv_test, c("Certification","data"), test_Certification_Excel()) })
    shiny::isolate({setValue(rv_test, c("General", "user"), "FK4") })
    shiny::isolate({setValue(rv_test, c("General", "study_id"), "funf") })
    shiny::isolate({set_uploadsource(rv_test, "Certification", uploadsource = "Excel") })

    # suppressMessages(
      shiny::testServer(
        app = ecerto::m_RDataexport_Server,
        args = list(
          rv = rv_test
        ),
        expr = {
          session$flushReact()
          print(input$study_id)
          # expect_equal(input$user,"FK4")
        }
      )
    # )
  })

# test_that(
#   desc = "RDataExport works for General$user",
#   code = {
#     rv_test <- ecerto::reactiveClass$new(ecerto::init_rv())
#     shiny::isolate({setValue(rv_test, c("Certification","data"), test_Certification_Excel()) })
#     shiny::isolate({setValue(rv_test, c("General", "user"), "FK4") })
#     shiny::isolate({set_uploadsource(rv_test, "Certification", uploadsource = "Excel") })
# 
#     # suppressMessages(
#     shiny::testServer(
#       app = ecerto::m_RDataexport_Server,
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

