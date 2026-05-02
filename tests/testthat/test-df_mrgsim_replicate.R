#####Test df_mrgsim_replicate#####

## Test Output
test_that("function returns a data.frame", {
  expect_s3_class(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                      model=model_mread_load("pkmodel"), replicates = 1,
                                      dv_var = "ODV"),
                  "data.frame")
})

test_that("output data.frame contains replicates x nrow(data) rows when obsonly = FALSE", {
  expect_equal(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 10,
                                   dv_var = "ODV") |> nrow(),
               nrow(dplyr::filter(data_sad, CMT != 3))*10)
})

test_that("output data.frame contains replicates x nrow(dplyr::filter(data, EVID ==0)) rows when obsonly = TRUE", {
  expect_equal(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 10,
                                   dv_var = "ODV", obsonly = TRUE) |> nrow(),
               nrow(dplyr::filter(data_sad, EVID ==0 & CMT != 3))*10)
})

test_that("output data.frame contains variable SIMDV if non-default sim_dv_var specified", {
  expect_named(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 10,
                                   dv_var = "ODV", sim_dv_var = "Y") |> dplyr::select(SIMDV),
               "SIMDV")
})

test_that("output data.frame contains expected output variables PRED, IPRED, SIMDV, OBSDV", {
  out <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                             model=model_mread_load("pkmodel"), replicates = 10,
                             dv_var = "ODV")
  expect_true(all(c("PRED", "IPRED", "SIMDV", "OBSDV") %in% colnames(out)))
})

test_that("output data.frame contains variable requested with argument `irep_name`", {
  expect_named(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 10,
                                   dv_var = "ODV", irep_name = "IREP") |> dplyr::select(IREP),
               "IREP")
})


## Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(df_mrgsim_replicate(data="",
                                   model=model_mread_load("pkmodel"), replicates = 1,dv_var = "ODV"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if incorrect class for arugmument `model`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model="model", replicates = 1, dv_var = "ODV"),
               regexp = "argument `model` must be class `mrgmod`")
})

test_that("Error if incorrect class for arugmument `replicates`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = "a", dv_var = "ODV"),
               regexp = "argument `replicates` must be coercible to class `integer`")
})

test_that("Error if time_var does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 1,
                                   dv_var = "ODV", time_var = "test"),
               regexp = "argument `time_var` must be variable.*in `data`")
})

test_that("Error if ntime_var does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 2,dv_var = "ODV",
                                   ntime_var = "test"),
               regexp = "argument `ntime_var` must be variable.*in `data`")
})

test_that("No error if time_var and ntime_var are specified as the same variable", {
  expect_no_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                      model=model_mread_load("pkmodel"), replicates = 2,dv_var = "ODV",
                                   time_var = "NTIME", ntime_var = "NTIME"))
})

test_that("Error if DV variable specified in dv_vars does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 2,dv_var = "DV"),
               regexp = "argument `dv_var` must be variable.*in `data`")
})

test_that("Error if variables specified by num_vars do not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 2,dv_var = "ODV",
                                   num_vars = "test"))
})

test_that("Error if variables specified by char_vars do not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 2,dv_var = "ODV",
                                   char_vars = "test"),
               regexp = "argument `char_vars` must be variable.*in `data`")
})

test_that("Same seed produces identical output", {
  out1 <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                              model=model_mread_load("pkmodel"), replicates = 2,
                              dv_var = "ODV", seed = 12345)
  out2 <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                              model=model_mread_load("pkmodel"), replicates = 2,
                              dv_var = "ODV", seed = 12345)
  expect_identical(out1$SIMDV, out2$SIMDV)
})

test_that("Error if incorrect class for arugmument `seed`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 2, dv_var = "ODV",
                                   seed = "A"),
               regexp = "argument `seed` must be coercible to class `integer`")
})

##Test NSE Bare Names
test_that("df_mrgsim_replicate accepts bare names", {
  model <- model_mread_load(model = "pkmodel")
  s1 <- df_mrgsim_replicate(dplyr::filter(data_sad, CMT != 3), model, replicates = 2,
                              dv_var = ODV, irep_name = SIM,
                              num_vars = c("CMT", "EVID", "MDV"),
                              char_vars = c("USUBJID"))
  expect_true(nrow(s1) > 0)
  expect_true("SIM" %in% colnames(s1))
})

test_that("df_mrgsim_replicate errors on zero-row data", {
  data <- dplyr::filter(data_sad, CMT != 3)[0, ]
  expect_error(
    df_mrgsim_replicate(data = data, model = model_mread_load("pkmodel"),
                        replicates = 2, dv_var = "ODV"),
    regexp = "must have at least one row"
  )
})


