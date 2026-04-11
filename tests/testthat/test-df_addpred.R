##Test Output
test_that("output is a data.frame", {
  expect_s3_class(df_addpred(data=dplyr::filter(data_sad, CMT != 3),model=model_mread_load("pkmodel")),
               "data.frame")
})

test_that("output data.frame contains same number of rows as input data.frame", {
  expect_equal(nrow(df_addpred(data=data_sad,model=model_mread_load("pkmodel"))),
               nrow(data_sad))
})

test_that("output data.frame contains variable PRED", {
  expect_named(dplyr::select(df_addpred(data=dplyr::filter(data_sad, CMT != 3),model=model_mread_load("pkmodel")), PRED),
               "PRED")
})

test_that("output is returned if non-default option specified for argument `output_var`", {
  expect_s3_class(df_addpred(data=dplyr::filter(data_sad, CMT != 3),model=model_mread_load("pkmodel"), output_var = "Y"),
                  "data.frame")
})


##Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(df_addpred(data="",model=model_mread_load("pkmodel")),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if incorrect class for arugmument `model`", {
  expect_error(df_addpred(data=dplyr::filter(data_sad, CMT != 3),model="pkmodel"),
               regexp = "argument `model` must be class `mrgmod`")
})

test_that("Error if incorrect class for arugmument `output_var`", {
  expect_error(df_addpred(data=dplyr::filter(data_sad, CMT != 3),model=model_mread_load("pkmodel"), output_var = "PRED"),
  regexp = "argument `output_var` must be captured as output in `model`")
})

##Test NSE Bare Names
test_that("df_addpred accepts bare names and matches string output", {
  model <- model_mread_load(model = "pkmodel")
  d1 <- df_addpred(dplyr::filter(data_sad, CMT != 3), model, output_var = IPRED)
  d2 <- df_addpred(dplyr::filter(data_sad, CMT != 3), model, output_var = "IPRED")
  expect_identical(d1, d2)
})
