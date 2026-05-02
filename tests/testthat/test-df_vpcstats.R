#####Test df_vpcstats#####

## Setup: shared simulation output
testsim_raw <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                   model = model_mread_load("pkmodel"),
                                   replicates = 10,
                                   dv_var = "ODV")

## Helper: preprocess + compute stats (mirrors plot_vpc_cont flow)
run_vpcstats <- function(sim, strat_var_str = NULL, pcvpc = FALSE,
                         lower_bound = 0, loq = NULL,
                         pi = c(0.05, 0.95), ci = c(0.05, 0.95),
                         time_var_str = "TIME", ntime_var_str = "NTIME",
                         pred_var_str = "PRED", ipred_var_str = "IPRED",
                         sim_dv_var_str = "SIMDV", obs_dv_var_str = "OBSDV") {
  sim <- pmxhelpr:::df_vpcpreprocess(sim, time_var_str, ntime_var_str,
                                     pred_var_str, ipred_var_str,
                                     sim_dv_var_str, obs_dv_var_str,
                                     strat_var_str, pcvpc, lower_bound, loq)
  pmxhelpr:::df_vpcstats(sim, pi, ci, "BIN_MID", strat_var_str, "SIM", loq)
}

## Test Output
test_that("df_vpcstats returns a data.frame", {
  result <- run_vpcstats(testsim_raw)
  expect_s3_class(result, "data.frame")
})

test_that("df_vpcstats returns expected columns", {
  result <- run_vpcstats(testsim_raw)
  expected_cols <- c("BIN_MID", "nbin",
                     "q5_low", "q5_med", "q5_hi",
                     "q50_low", "q50_med", "q50_hi",
                     "q95_low", "q95_med", "q95_hi",
                     "obs5", "obs50", "obs95")
  expect_true(all(expected_cols %in% colnames(result)))
})

test_that("df_vpcstats returns one row per unique bin value", {
  result <- run_vpcstats(testsim_raw)
  obs_bins <- unique(testsim_raw$NTIME[testsim_raw$MDV == 0 & testsim_raw$EVID == 0 &
                                         !is.na(testsim_raw$NTIME)])
  expect_equal(nrow(result), length(obs_bins))
})

## Test Stratification
test_that("df_vpcstats with strat_var returns rows for each bin x stratum", {
  testsim_strat <- df_mrgsim_replicate(
    data = dplyr::filter(data_sad, CMT != 3),
    model = model_mread_load("pkmodel"),
    replicates = 10,
    dv_var = "ODV",
    char_vars = "FOOD")
  testsim_strat <- dplyr::mutate(testsim_strat, FOOD_f = factor(FOOD))

  result <- run_vpcstats(testsim_strat, strat_var_str = "FOOD_f")
  expect_true("FOOD_f" %in% colnames(result))
  obs_rows <- testsim_strat$MDV == 0 & testsim_strat$EVID == 0 &
    !is.na(testsim_strat$NTIME)
  expect_equal(nrow(result),
               nrow(unique(testsim_strat[obs_rows, c("NTIME", "FOOD_f")])))
})

## Test LLOQ handling
test_that("df_vpcstats with loq returns NA for observed quantiles below threshold", {
  result <- run_vpcstats(testsim_raw, loq = 1e6)
  expect_true(all(is.na(result$obs5)))
  expect_true(all(is.na(result$obs50)))
  expect_true(all(is.na(result$obs95)))
})

## Test quantile ordering
test_that("simulated quantile medians are ordered q5_med <= q50_med <= q95_med", {
  result <- run_vpcstats(testsim_raw)
  expect_true(all(result$q5_med <= result$q50_med, na.rm = TRUE))
  expect_true(all(result$q50_med <= result$q95_med, na.rm = TRUE))
})

#####Test Preprocessing#####

test_that("df_vpcpreprocess validates column existence", {
  expect_error(
    run_vpcstats(testsim_raw, time_var_str = "NONEXIST"),
    regexp = "must be variable.*in `sim`"
  )
})

test_that("pcvpc = TRUE produces different medians than pcvpc = FALSE", {
  stats_no_pc <- run_vpcstats(testsim_raw, pcvpc = FALSE)
  stats_pc    <- run_vpcstats(testsim_raw, pcvpc = TRUE)
  expect_false(identical(stats_no_pc$q50_med, stats_pc$q50_med))
})

test_that("pcvpc = TRUE with loq sets BLQ values to NA before prediction correction", {
  loq_val <- stats::quantile(testsim_raw$OBSDV, 0.25, na.rm = TRUE)
  stats_pc_loq <- run_vpcstats(testsim_raw, pcvpc = TRUE, loq = loq_val)
  stats_pc     <- run_vpcstats(testsim_raw, pcvpc = TRUE)
  expect_s3_class(stats_pc_loq, "data.frame")
  expect_false(identical(stats_pc_loq$q50_med, stats_pc$q50_med))
})
