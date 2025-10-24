## Test Output
test_that("function returns a data.frame", {
  expect_s3_class(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 1,
                                      dv_var = "ODV"),
                  "data.frame")
})

test_that("output data.frame contains replicates x nrow(data) rows when obsonly = FALSE", {
  expect_equal(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   dv_var = "ODV") |> nrow(),
               nrow(data_sad)*10)
})

test_that("output data.frame contains replicates x nrow(dplyr::filter(data, EVID ==0)) rows when obsonly = TRUE", {
  expect_equal(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   dv_var = "ODV", obsonly = TRUE) |> nrow(),
               nrow(dplyr::filter(data_sad, EVID ==0))*10)
})

test_that("output data.frame contains variable SIMDV if non-default output_vars option specified for DV", {
  expect_named(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   dv_var = "ODV", output_vars = c(DV = "Y")) |> dplyr::select(SIMDV),
               "SIMDV")
})

test_that("output data.frame contains variable PRED", {
  expect_named(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   dv_var = "ODV") |> dplyr::select(PRED),
               "PRED")
})

test_that("output data.frame contains variable IPRED", {
  expect_named(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   dv_var = "ODV") |> dplyr::select(IPRED),
               "IPRED")
})

test_that("output data.frame contains variable SIMDV", {
  expect_named(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   dv_var = "ODV") |> dplyr::select(SIMDV),
               "SIMDV")
})

test_that("output data.frame contains variable OBSDV", {
  expect_named(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   dv_var = "ODV") |> dplyr::select(OBSDV),
               "OBSDV")
})

test_that("output data.frame contains variable requested with argument `irep_name`", {
  expect_named(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   dv_var = "ODV", irep_name = "IREP") |> dplyr::select(IREP),
               "IREP")
})


## Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(df_mrgsim_replicate(data="",model=model_mread_load("model"), replicates = 1,dv_var = "ODV"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if incorrect class for arugmument `model`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model="model", replicates = 1, dv_var = "ODV"),
               regexp = "argument `model` must be class `mrgmod`")
})

test_that("Error if incorrect class for arugmument `replicates`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = "a", dv_var = "ODV"),
               regexp = "argument `replicates` must be coercible to class `integer`")
})

test_that("Error if TIME variable specified in time_vars does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 1,
                                   dv_var = "ODV", time_vars = c(TIME = "test")),
               regexp = "must be variables in `data`")
})

test_that("Error if NTIME variable specified in time_vars does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,dv_var = "ODV",
                                   time_vars = c(NTIME = "test")),
               regexp = "must be variables in `data`")
})

test_that("No error if TIME variable specified in time_vars is NA", {
  expect_no_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,dv_var = "ODV",
                                   time_vars = c(TIME = NA)))
})

test_that("Error if NTIME variable specified in time_vars is NA ", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,dv_var = "ODV",
                                      time_vars = c(NTIME = NA)),
               regexp = "Selections can't have missing values.")
})

test_that("Error if DV variable specified in dv_vars does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,dv_var = "DV"),
               regexp = "argument `dv_var` must be variables in `data`")
})

test_that("Error if variables specified by num_vars do not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,dv_var = "ODV",
                                   num_vars = "test"))
})

test_that("Error if variables specified by char_vars do not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,dv_var = "ODV",
                                   char_vars = "test"),
               regexp = "argument `char_vars` must be variables in `data`")
})

test_that("Error if variable specified by irep_name does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,dv_var = "ODV",
                                   char_vars = "test"),
               regexp = "argument `char_vars` must be variables in `data`")
})

test_that("Error if incorrect class for arugmument `seed`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2, dv_var = "ODV",
                                   seed = "A"),
               regexp = "argument `seed` must be coercible to class `integer`")
})


