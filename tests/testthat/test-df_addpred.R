##Test Output
test_that("output is a data.frame", {
  expect_s3_class(df_addpred(data=data_sad,model=model_mread_load("model")),
               "data.frame")
})

test_that("output data.frame contains same number of rows as input data.frame", {
  expect_equal(nrow(df_addpred(data=data_sad,model=model_mread_load("model"))),
               nrow(data_sad))
})

test_that("output data.frame contains variable PRED", {
  expect_named(dplyr::select(df_addpred(data=data_sad,model=model_mread_load("model")), PRED),
               "PRED")
})

test_that("output data.frame$PRED is the same length as rows in input data.frame ", {
  expect_equal(length(df_addpred(data=data_sad,model=model_mread_load("model"))$PRED),
               nrow(data_sad))
})

test_that("output is returned if non-default option specified for argument `output_var`", {
  expect_s3_class(df_addpred(data=data_sad,model=model_mread_load("model"), output_var = "Y"),
                  "data.frame")
})


##Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(df_addpred(data="",model=model_mread_load("model")))
})

test_that("Error if incorrect class for arugmument `model`", {
  expect_error(df_addpred(data=data_sad,model="model"))
})

test_that("Error if incorrect class for arugmument `output_var`", {
  expect_error(df_addpred(data=data_sad,model="model", output_var = "PRED"))
})
