#####df_nobsbin#####

##Test Output
test_that("Output is a `data.frame`", {
  expect_s3_class(df_nobsbin(data_sad),
                  class = "data.frame")
})

test_that("output data.frame is the same number of rows as unique binning variables when strat_var = NULL", {
  expect_equal(df_nobsbin(data_sad) |> nrow(),
               length(unique((data_sad$NTIME))))
})

test_that("output data.frame is the same number of rows as unique binning variables x unique strat_var", {
  expect_equal(df_nobsbin(data_sad, strat_vars = "FOOD") |> nrow(),
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
               regexp = "argument `bin_var` must be variables in `data`")
})

test_that("Error if `strat_vars` do not exist in `data`", {
  expect_error(df_nobsbin(data_sad, strat_var = "FOOD_f"),
               regexp = "argument `strat_vars` must be variables in `data`")
})





#####df_pcdv#####

##Test Output
test_that("Output is a `data.frame`", {
  expect_s3_class(df_pcdv(df_addpred(data_sad, model = model_mread_load("model")),
                          dvpred_vars = c(DV = "ODV")),
                  class = "data.frame")
})

test_that("output data.frame is the same number of rows as input data frame", {
  expect_equal(df_pcdv(df_addpred(data_sad, model = model_mread_load("model")),dvpred_vars = c(DV = "ODV")) |> nrow(),
               df_addpred(data_sad, model = model_mread_load("model")) |> nrow())
})

test_that("output data.frame contains variable `PCDV`", {
  expect_named(df_pcdv(df_addpred(data_sad, model = model_mread_load("model")),dvpred_vars = c(DV = "ODV")) |> dplyr::select(PCDV),
               "PCDV")
})

test_that("output data.frame contains binning variable specified in `bin_var`", {
  expect_named(df_pcdv(df_addpred(data_sad, model = model_mread_load("model")),
                       dvpred_vars = c(DV = "ODV")) |> dplyr::select(NTIME),
               "NTIME")
})

test_that("output `PCDV' is modified when the `lower_bound` argument is specified", {
    a <- df_pcdv(df_addpred(data_sad, model = model_mread_load("model")),
          dvpred_vars = c(DV = "ODV")) |> dplyr::select(PCDV)
    b <- df_pcdv(df_addpred(data_sad, model = model_mread_load("model")),
            dvpred_vars = c(DV = "ODV"), lower_bound = 5) |> dplyr::select(PCDV)

    test <- a == b
    result <- unique(test[!is.na(test)])

  expect_false(result)
})


##Test Argument Handling

test_that("Error if incorrect class for argument `data`", {
  expect_error(df_pcdv(df_addpred("data", model = model_mread_load("model")),
                       dvpred_vars = c(DV = "ODV")),
               regexp = "argument `data` must be a `data.frame`")
})

test_that("Error if `bin_var` does not exist in `data`", {
  expect_error(df_pcdv(df_addpred(data_sad, model = model_mread_load("model")),
                       dvpred_vars = c(DV = "ODV"), bin_var = "NTFD"),
               regexp = "argument `bin_var` must be variables in `data`")
})

test_that("Error if `strat_vars` do not exist in `data`", {
  expect_error(df_pcdv(df_addpred(data_sad, model = model_mread_load("model")),
                       dvpred_vars = c(DV = "ODV"), strat_vars = "FOOD_f"),
               regexp = "argument `strat_vars` must be variables in `data`")
})

test_that("Error if `dvpred_vars` do not exist in `data`", {
  expect_error(df_pcdv(df_addpred(data_sad, model = model_mread_load("model")),
                       dvpred_vars = c(DV = "DV")),
               regexp = "must be variables in `data`")
})







#####plot_vpc_exactbins####

##Test Output
test_that("Output is a `ggplot` plot object", {
  expect_s3_class(plot_vpc_exactbins(sim = df_mrgsim_replicate(data=data_sad,model=model_mread_load("model"),
                                                            replicates = 10,
                                                            dv_var = "ODV")),
               class = "ggplot")
})

test_that("Output plot contains a caption with number of replicates by default", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_equal(
    plot_vpc_exactbins(sim = testsim)$labels$caption,
    "Replicates = 10"
  )
})

test_that("Output plot does not contains a caption with number of replicates when `show_rep = FALSE`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_no_match(
    names(plot_vpc_exactbins(sim = testsim, show_rep = FALSE)$labels),
    regexp = "caption"
  )
})


##Test Argument Handling
test_that("Error if incorrect class for arugmument `sim`", {
  expect_error(plot_vpc_exactbins(sim = "simdata"),
               regexp = "argument `sim` must be a `data.frame`")
})

test_that("Error if TIME variable specified in time_vars does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim,time_vars = c(TIME = "ATFD")),
    regexp = "must be variables in `sim`"
    )
})

test_that("Error if NTIME variable specified in time_vars does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim,time_vars = c(NTIME = "NTFD")),
    regexp = "must be variables in `sim`"
  )
})

test_that("No error if TIME and NTIME specified as same variabl in time_vars", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_no_error(
    plot_vpc_exactbins(sim = testsim,time_vars = c(TIME = "NTIME", NTIME = "NTIME")),
  )
})

test_that("Error if PRED variable specified in output_vars does not exist in `sim` and pcvpc = TRUE", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim,output_vars = c(PRED = "CPRED"), pcvpc = TRUE),
    regexp = "must be variables in `sim`"
  )
})

test_that("No error if PRED variable specified in output_vars does not exist in `sim` and pcvpc = FALSE", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_no_error(
    plot_vpc_exactbins(sim = testsim,output_vars = c(PRED = "CPRED"), pcvpc = FALSE),
  )
})

test_that("Error if SIMDV variable specified in output_vars does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim,output_vars = c(SIMDV = "DVSIM")),
    regexp = "must be variables in `sim`"
  )
})

test_that("Error if OBSDV variable specified in output_vars does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim,output_vars = c(OBSDV = "DV")),
    regexp = "must be variables in `sim`"
  )
})

test_that("Error if argument for `loq` is not class numeric", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim, loq = "1"),
    regexp = "argument `loq` must be class `numeric`"
  )
})


test_that("Error if variable specified by `strat_var` does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim, strat_var = "FOOD_f"),
    regexp = "argument `strat_var` must be variables in `sim`"
  )
})

test_that("Error if variable specified by `irep_name` does not exist in `sim`", {
  testsim <- df_mrgsim_replicate(data=data_sad,
                                 model=model_mread_load("model"),
                                 replicates = 10,
                                 dv_var = "ODV")

  expect_error(
    plot_vpc_exactbins(sim = testsim, irep_name = "IREP"),
    regexp = "argument `irep_name` must be variables in `sim`"
  )
})
