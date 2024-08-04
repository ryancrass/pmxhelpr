test_that("Error if incorrect class for arugmument `data`", {
  expect_error(df_mrgsim_replicate(data="",model=model_mread_load("model"), replicates = 2, output_vars = c(DV = "ODV")))
})

test_that("Error if incorrect class for arugmument `model`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model="model", replicates = 2,
                                   output_vars = c(DV = "ODV")))
})

test_that("Error if incorrect class for arugmument `replicates`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = "a",
                                   output_vars = c(DV = "ODV")))
})

test_that("Error if TIME variable specified in time_vars does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,
                                   output_vars = c(DV = "ODV"), time_vars = c(TIME = "test")))
})

test_that("Error if NTIME variable specified in time_vars does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,
                                   output_vars = c(DV = "ODV"), time_vars = c(NTIME = "test")))
})

test_that("Error if DV variable specified in output_vars does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,
                                   output_vars = c(DV = "DV")))
})

test_that("Error if variables specified by char_vars do not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,
                                   output_vars = c(DV = "ODV"), char_vars = "test"))
})

test_that("Error if variable specified by irep_name does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2,
                                   output_vars = c(DV = "ODV"), char_vars = "test"))
})

test_that("Error if incorrect class for arugmument `seed`", {
  expect_error(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 2, seed = "A",
                                   output_vars = c(DV = "ODV")))
})

test_that("output data.frame contains replicates x nrow(data) rows when obsonly = FALSE", {
  expect_equal(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   output_vars = c(DV = "ODV")) |> nrow(),
               nrow(data_sad)*10)
})

test_that("output data.frame contains variable PRED", {
  expect_named(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   output_vars = c(DV = "ODV")) |> dplyr::select(PRED),
               "PRED")
})

test_that("output data.frame contains variable IPRED", {
  expect_named(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   output_vars = c(DV = "ODV")) |> dplyr::select(IPRED),
               "IPRED")
})

test_that("output data.frame contains variable SIMDV", {
  expect_named(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   output_vars = c(DV = "ODV")) |> dplyr::select(SIMDV),
               "SIMDV")
})

test_that("output data.frame contains variable OBSDV", {
  expect_named(df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"), replicates = 10,
                                   output_vars = c(DV = "ODV")) |> dplyr::select(OBSDV),
               "OBSDV")
})
