#####df_pcdv#####

##Test Output
test_that("Output is a `data.frame`", {
  expect_s3_class(df_pcdv(df_addpred(data_sad, model = model_mread_load("pkmodel")),
                          dvpred_vars = c(DV = "ODV")),
                  class = "data.frame")
})

test_that("output data.frame is the same number of rows as input data frame", {
  expect_equal(df_pcdv(df_addpred(data_sad, model = model_mread_load("pkmodel")),dvpred_vars = c(DV = "ODV")) |> nrow(),
               df_addpred(data_sad, model = model_mread_load("pkmodel")) |> nrow())
})

test_that("output data.frame contains variable `PCDV`", {
  expect_named(df_pcdv(df_addpred(data_sad, model = model_mread_load("pkmodel")),dvpred_vars = c(DV = "ODV")) |> dplyr::select(PCDV),
               "PCDV")
})

test_that("output data.frame contains binning variable specified in `bin_var`", {
  expect_named(df_pcdv(df_addpred(data_sad, model = model_mread_load("pkmodel")),
                       dvpred_vars = c(DV = "ODV")) |> dplyr::select(NTIME),
               "NTIME")
})

test_that("output `PCDV' is modified when the `lower_bound` argument is specified", {
  a <- df_pcdv(df_addpred(data_sad, model = model_mread_load("pkmodel")),
               dvpred_vars = c(DV = "ODV")) |> dplyr::select(PCDV)
  b <- df_pcdv(df_addpred(data_sad, model = model_mread_load("pkmodel")),
               dvpred_vars = c(DV = "ODV"), lower_bound = 5) |> dplyr::select(PCDV)

  test <- a == b
  result <- unique(test[!is.na(test)])

  expect_false(result)
})


##Test Argument Handling

test_that("Error if incorrect class for argument `data`", {
  expect_error(df_pcdv(df_addpred("data", model = model_mread_load("pkmodel")),
                       dvpred_vars = c(DV = "ODV")),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if `bin_var` does not exist in `data`", {
  expect_error(df_pcdv(df_addpred(data_sad, model = model_mread_load("pkmodel")),
                       dvpred_vars = c(DV = "ODV"), bin_var = "NTFD"),
               regexp = "argument `bin_var` must be variables in `data`")
})

test_that("Error if `strat_vars` do not exist in `data`", {
  expect_error(df_pcdv(df_addpred(data_sad, model = model_mread_load("pkmodel")),
                       dvpred_vars = c(DV = "ODV"), strat_vars = "FOOD_f"),
               regexp = "argument `strat_vars` must be variables in `data`")
})

test_that("Error if `dvpred_vars` do not exist in `data`", {
  expect_error(df_pcdv(df_addpred(data_sad, model = model_mread_load("pkmodel")),
                       dvpred_vars = c(DV = "DV")),
               regexp = "must be variables in `data`")
})
