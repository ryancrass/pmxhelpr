##Test Output
test_that("Function returns a mrgsolve model object", {
  expect_s4_class(model_mread_load("model"), class = "mrgmod")
})


##Test Argument Handling
test_that("Error if incorrect model does not exist in library", {
  expect_error(model_mread_load("test"),
               regexp = "`test` does not exist in the pmxhelpr model library")
})
