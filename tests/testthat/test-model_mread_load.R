##Test Output
test_that("Function returns a mrgsolve model object", {
  expect_s4_class(model_mread_load("model"), class = "mrgmod")
})

test_that("Returned model object can be simulated", {
  mod <- model_mread_load("model")
  sim <- mrgsolve::mrgsim(mod, end = 24)
  expect_s4_class(sim, "mrgsims")
  expect_true(nrow(sim) > 0)
})

test_that("Second call returns model object (cache behavior)", {
  mod1 <- model_mread_load("model")
  mod2 <- model_mread_load("model")
  expect_s4_class(mod2, "mrgmod")
})


##Test Argument Handling
test_that("Error if incorrect model does not exist in library", {
  expect_error(model_mread_load("test"),
               regexp = "`test` does not exist in the pmxhelpr model library")
})

test_that("Error if model name with special characters does not exist", {
  expect_error(model_mread_load("nonexistent_model_xyz"),
               regexp = "does not exist in the pmxhelpr model library")
})
