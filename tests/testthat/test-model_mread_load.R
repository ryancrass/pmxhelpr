test_that("Error if incorrect model does not existin in library", {
  expect_error(model_mread_load("test"))
})
