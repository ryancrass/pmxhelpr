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

test_that("output data.frame contains variable SIMDV if non-default dv_output specified", {
  expect_named(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 10,
                                   dv_var = "ODV", dv_output = "Y") |> dplyr::select(SIMDV),
               "SIMDV")
})

test_that("output data.frame contains expected output variables PRED, IPRED, SIMDV, OBSDV", {
  out <- df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                             model=model_mread_load("pkmodel"), replicates = 10,
                             dv_var = "ODV")
  expect_true(all(c("PRED", "IPRED", "SIMDV", "OBSDV") %in% colnames(out)))
})

test_that("output data.frame contains variable requested with argument `rep_var`", {
  expect_named(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 10,
                                   dv_var = "ODV", rep_var = "IREP") |> dplyr::select(IREP),
               "IREP")
})

test_that("df_mrgsim_replicate carries user-specified columns via carry_out and recover", {
  out <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                             model = model_mread_load("pkmodel"),
                             replicates = 2, dv_var = "ODV",
                             carry_out = c("WTBL", "DOSE"),
                             recover  = c("USUBJID"))
  # Requested columns appear
  expect_true(all(c("WTBL", "DOSE", "USUBJID") %in% colnames(out)))
  # Unlisted input columns do not appear
  expect_false("LLOQ" %in% colnames(out))
  expect_false("BLQ"  %in% colnames(out))
  expect_false("PART" %in% colnames(out))
})


## Test Argument Handling
test_that("Error if incorrect class for arugmument `data`", {
  expect_error(df_mrgsim_replicate(data="",
                                   model=model_mread_load("pkmodel"), replicates = 1,dv_var = "ODV"),
               regexp = "`data` must be a <data.frame>")
})

test_that("Error if incorrect class for arugmument `model`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model="model", replicates = 1, dv_var = "ODV"),
               regexp = "`model` must be class <mrgmod>")
})

test_that("Error if incorrect class for arugmument `replicates`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = "a", dv_var = "ODV"),
               regexp = "`replicates` must be coercible to <integer>")
})

test_that("Error if time_var does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 1,
                                   dv_var = "ODV", time_var = "test"),
               regexp = "`time_var` must name variable.*in `data`")
})

test_that("Error if ntime_var does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 2,dv_var = "ODV",
                                   ntime_var = "test"),
               regexp = "`ntime_var` must name variable.*in `data`")
})

test_that("No error if time_var and ntime_var are specified as the same variable", {
  expect_no_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                      model=model_mread_load("pkmodel"), replicates = 2,dv_var = "ODV",
                                   time_var = "NTIME", ntime_var = "NTIME"))
})

test_that("Error if DV variable specified in dv_vars does not exist in `data`", {
  expect_error(df_mrgsim_replicate(data=dplyr::filter(data_sad, CMT != 3),
                                   model=model_mread_load("pkmodel"), replicates = 2,dv_var = "DV"),
               regexp = "`dv_var` must name variable.*in `data`")
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
               regexp = "`seed` must be coercible to <integer>")
})

##Test NSE Bare Names
test_that("df_mrgsim_replicate accepts bare names", {
  model <- model_mread_load(model = "pkmodel")
  s1 <- df_mrgsim_replicate(dplyr::filter(data_sad, CMT != 3), model, replicates = 2,
                              dv_var = ODV, rep_var = SIM)
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

## The removed `parallel` argument must not fall through `...` to mrgsim_df()
test_that("Error if the removed parallel argument is supplied", {
  expect_error(df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                   model = model_mread_load("pkmodel"),
                                   replicates = 2, dv_var = "ODV",
                                   parallel = TRUE),
               regexp = "`parallel` was removed in pmxhelpr 0.7.0")
})

test_that("model-output arguments keep their deprecated names through 0.8.x", {
  withr::local_options(lifecycle_verbosity = "warning")
  d <- dplyr::filter(data_sad, CMT %in% c(1, 2), ID %in% unique(ID)[1:3])
  mod <- model_mread_load("pkmodel")
  rep <- function(...) df_mrgsim_replicate(d, mod, replicates = 2, dv_var = ODV, ...)
  ## pred_var has no replacement and its value is ignored
  lifecycle::expect_deprecated(old <- rep(pred_var = "CP"), "pred_var")
  expect_identical(old, rep())
  lifecycle::expect_deprecated(old <- rep(sim_dv_var = "Y"), "dv_output")
  expect_identical(old, rep(dv_output = "Y"))
  lifecycle::expect_deprecated(old <- rep(sim_dv_var = Y))
  expect_identical(old, rep(dv_output = "Y"))
  lifecycle::expect_deprecated(old <- rep(ipred_var = "IPRED"), "ipred_output")
  expect_identical(old, rep())
  expect_error(rep(dv_output = "Y", sim_dv_var = "Y"), "Pass `dv_output` only",
               class = "pmxhelpr_error_bad_arg")
  lifecycle::expect_deprecated(old <- df_mrgsim_addpred(d, mod, output_var = "Y"), "ipred_output")
  expect_identical(old, df_mrgsim_addpred(d, mod, ipred_output = "Y"))
  expect_error(df_mrgsim_addpred(d, mod, ipred_output = "Y", output_var = "Y"),
               "Pass `ipred_output` only", class = "pmxhelpr_error_bad_arg")
})

test_that("the model-output check names the *_output arguments", {
  d <- dplyr::filter(data_sad, CMT %in% c(1, 2))
  expect_error(df_mrgsim_replicate(d, model_mread_load("pkmodel"), replicates = 1, dv_var = ODV,
                                   dv_output = "NOPE", ipred_output = "NOPE2"),
               "`dv_output` or `ipred_output`", class = "pmxhelpr_error_model")
})

test_that("`seed` controls the simulation and replicates are independent draws", {
  run <- function(seed) {
    df_mrgsim_replicate(dplyr::filter(data_sad, CMT %in% c(1, 2)),
                        model_mread_load("pkmodel"), replicates = 2,
                        dv_var = ODV, seed = seed)
  }
  a <- run(1)
  expect_identical(a$SIMDV, run(1)$SIMDV)
  expect_false(identical(a$SIMDV, run(2)$SIMDV))
  expect_false(identical(a$SIMDV[a$SIM == 1], a$SIMDV[a$SIM == 2]))
  expect_equal(a$PRED[a$SIM == 1], a$PRED[a$SIM == 2])
})

#####output names already taken####

test_that("output columns are built even when their names are already taken", {
  d <- dplyr::filter(data_sad, CMT %in% c(1, 2), ID %in% unique(ID)[1:3])
  mod <- model_mread_load("pkmodel")
  rep <- function(data = d, ...) df_mrgsim_replicate(data, mod, replicates = 1, dv_var = ODV, ...)
  ## the model also outputs IPRED: it is replaced, without a warning, by the chosen output
  expect_no_warning(y <- rep(ipred_output = "Y"))
  expect_false("Y" %in% names(y))
  expect_equal(sum(names(y) == "IPRED"), 1)
  expect_identical(y$IPRED, rep(dv_output = "Y")$SIMDV)    # pkmodel's Y, by another route
  ## one output for both IPRED and SIMDV
  expect_no_warning(i <- rep(dv_output = "IPRED"))
  expect_identical(i$SIMDV, i$IPRED)
  ## data columns named OBSDV or SIMDV are replaced with a warning
  d_obs <- dplyr::mutate(d, OBSDV = -1)
  expect_warning(o <- rep(d_obs), "`OBSDV` column; it is replaced by the `dv_var` column `ODV`",
                 class = "pmxhelpr_warning_overwrite")
  expect_identical(o$OBSDV, rep()$OBSDV)
  expect_false("ODV" %in% names(o))
  d_sim <- dplyr::mutate(d, SIMDV = -1)
  expect_warning(s <- rep(d_sim, recover = "SIMDV"), class = "pmxhelpr_warning_overwrite")
  expect_identical(s$SIMDV, rep()$SIMDV)
  expect_no_warning(rep())
})


##### params: one parameter set per replicate #####

params_data <- dplyr::filter(data_sad, CMT != 3)
params_model <- model_mread_load("pkmodel")

test_that("params = NULL leaves the simulation unchanged", {
  expect_identical(
    df_mrgsim_replicate(params_data, params_model, 3, dv_var = ODV, params = NULL),
    df_mrgsim_replicate(params_data, params_model, 3, dv_var = ODV)
  )
})

test_that("each replicate runs its params row, with its own PRED", {
  draws <- data.frame(TVCL = c(10, 20, 40), TVVC = c(30, 35.7, 40))
  sim <- df_mrgsim_replicate(params_data, params_model, params = draws, dv_var = ODV)
  expect_equal(sort(unique(sim$SIM)), 1:3)
  base <- dplyr::rename(params_data, OBSDV = ODV)
  for (i in 1:3) {
    ref <- df_mrgsim_addpred(base, mrgsolve::param(params_model, as.list(draws[i, ])))
    expect_equal(sim$PRED[sim$SIM == i], ref$PRED)
  }
  expect_false(isTRUE(all.equal(sim$PRED[sim$SIM == 1], sim$PRED[sim$SIM == 3])))
  expect_false(isTRUE(all.equal(sim$IPRED[sim$SIM == 1], sim$IPRED[sim$SIM == 3])))
})

test_that("params rows at the model's values reproduce the base PRED", {
  draws <- data.frame(TVCL = rep(mrgsolve::param(params_model)$TVCL, 2))
  sim <- df_mrgsim_replicate(params_data, params_model, params = draws, dv_var = ODV)
  base <- df_mrgsim_replicate(params_data, params_model, 2, dv_var = ODV)
  expect_equal(sim$PRED, base$PRED)
})

test_that("params works with recover and without a replicates count", {
  draws <- data.frame(TVCL = c(15, 25))
  sim <- df_mrgsim_replicate(params_data, params_model, params = draws, dv_var = ODV,
                             recover = "USUBJID")
  expect_equal(nrow(sim), 2 * nrow(params_data))
  expect_true("USUBJID" %in% names(sim))
})

test_that("params is validated", {
  sim <- function(...) df_mrgsim_replicate(params_data, params_model, dv_var = ODV, ...)
  expect_error(sim(), "required unless", class = "pmxhelpr_error_bad_arg")
  expect_error(sim(params = list(TVCL = 1)), class = "pmxhelpr_error")
  expect_error(sim(params = data.frame(NOPE = 1)), "not a parameter",
               class = "pmxhelpr_error_bad_arg")
  expect_error(sim(params = data.frame(DOSE = 100)), "also a column of `data`",
               class = "pmxhelpr_error_bad_arg")
  expect_error(sim(replicates = 3, params = data.frame(TVCL = c(10, 20))),
               "needs one per replicate", class = "pmxhelpr_error_bad_arg")
  expect_error(sim(params = data.frame(TVCL = c(10, NA))), "no missing values",
               class = "pmxhelpr_error_bad_arg")
  expect_error(sim(params = data.frame(TVCL = c("a", "b"))), "numeric",
               class = "pmxhelpr_error_bad_arg")
  expect_error(sim(params = data.frame(TVCL = 1)[, 0, drop = FALSE]), "at least one",
               class = "pmxhelpr_error_bad_arg")
})
