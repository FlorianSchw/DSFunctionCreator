
test_that("createDSfunction errors", {

  # Creating differing test data.frames in the respective connections for testing if columns check works correctly


  # Actual Test Start
  expect_error(createDSfunction(), "Please provide a name for the new DataSHIELD function!", fixed = TRUE)
  expect_error(createDSfunction(function_name = "XXXX"), "Please provide a name that characterises the input object!", fixed = TRUE)
  expect_error(createDSfunction(function_name = "XXXX",
                                input_object = "YYYY"), "Please provide the class of the input object of the new DataSHIELD function!", fixed = TRUE)
  expect_error(createDSfunction(function_name = "XXXX",
                                input_object = "YYYY",
                                object_typ = "data.frame"), "You want to include the call to the server-side but have not provided the typ of server-side function ('assign', 'aggregate')!", fixed = TRUE)

})


