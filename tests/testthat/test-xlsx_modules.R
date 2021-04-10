# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

shiny::testServer(
  app = .uploadTabsetsServer, {
    session$setInputs
  }
)