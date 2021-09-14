test_that(
  desc = "datreturn contains same elements for test and init",
  code =  {
    datreturn_test <- ecerto:::test_datreturn()
    datreturn_init = reactiveClass$new(init_datreturn())
    expect_equal(
      sort(shiny::isolate({datreturn_init$names()})), 
      sort(shiny::isolate({datreturn_test$names()})))
})
