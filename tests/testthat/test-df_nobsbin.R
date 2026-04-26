#####df_nobsbin#####

##Test Output
test_that("Output is a `data.frame`", {
  expect_s3_class(df_nobsbin(data_sad),
                  class = "data.frame")
})

test_that("output data.frame is the same number of rows as unique binning variables when strat_var = NULL", {
  expect_equal(df_nobsbin(dplyr::filter(data_sad, CMT == 2)) |> nrow(),
               length(unique((data_sad$NTIME))))
})

test_that("output data.frame is the same number of rows as unique binning variables x unique strat_var", {
  expect_equal(df_nobsbin(dplyr::filter(data_sad, CMT == 2), strat_vars = "FOOD") |> nrow(),
               dplyr::distinct(dplyr::select(data_sad, NTIME, FOOD)) |> nrow())
})

test_that("output data.frame contains variable `n_obs`", {
  expect_named(df_nobsbin(data_sad) |> dplyr::select(n_obs),
               "n_obs")
})

test_that("output data.frame contains variable `n_miss`", {
  expect_named(df_nobsbin(data_sad) |> dplyr::select(n_miss),
               "n_miss")
})

test_that("output data.frame contains variable `CMT`", {
  expect_named(df_nobsbin(data_sad) |> dplyr::select(CMT),
               "CMT")
})

##Test Argument Handling

test_that("Error if incorrect class for arugmument `data`", {
  expect_error(df_nobsbin("data"),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if `bin_var` does not exist in `data`", {
  expect_error(df_nobsbin(data_sad, bin_var = "NTFD"),
               regexp = "argument `bin_var` must be variable.*in `data`")
})

test_that("Error if `strat_vars` do not exist in `data`", {
  expect_error(df_nobsbin(data_sad, strat_var = "FOOD_f"),
               regexp = "argument `strat_vars` must be variable.*in `data`")
})
