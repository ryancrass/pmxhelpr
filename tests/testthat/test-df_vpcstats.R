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
  pmxhelpr:::df_vpcstats(sim, pi, ci, "BIN_MID", strat_var_str, "SIM")
}

## Test Output
test_that("df_vpcstats returns a data.frame", {
  result <- run_vpcstats(testsim_raw)
  expect_s3_class(result, "data.frame")
})

test_that("df_vpcstats returns expected columns", {
  result <- run_vpcstats(testsim_raw)
  expected_cols <- c("BIN_MID", "nbin", "nobs", "nmiss",
                     "q5_low", "q5_med", "q5_hi",
                     "q50_low", "q50_med", "q50_hi",
                     "q95_low", "q95_med", "q95_hi",
                     "obs5", "obs50", "obs95")
  expect_true(all(expected_cols %in% colnames(result)))
})

test_that("df_vpcstats: nobs + nmiss == nbin always", {
  result_default <- run_vpcstats(testsim_raw)
  expect_equal(result_default$nobs + result_default$nmiss, result_default$nbin)

  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  result_loq <- run_vpcstats(testsim_raw, loq = loq_val)
  expect_equal(result_loq$nobs + result_loq$nmiss, result_loq$nbin)
})

test_that("df_vpcstats: loq increases nmiss (more rows below threshold flagged)", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  result_default <- run_vpcstats(testsim_raw)
  result_loq <- run_vpcstats(testsim_raw, loq = loq_val)
  ## Same bins, same nbin; loq strictly raises nmiss in at least some bins.
  expect_equal(result_loq$nbin, result_default$nbin)
  expect_true(any(result_loq$nmiss > result_default$nmiss))
})

test_that("min_bin_count gates on nobs (quantifiable obs), not total record count", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  result <- plot_vpc_cont(sim = testsim_raw, loq = loq_val, vpcstats = TRUE)
  ## Pick a threshold that is below max(nbin) but above max(nobs) in some bin
  threshold <- max(result$nobs) + 1L
  ## Quantile-summary frame still contains all bins; the gate is applied at
  ## the plot-build call. Verify the gate by inspecting the plot's data.
  p <- plot_vpc_cont(sim = testsim_raw, loq = loq_val,
                     min_bin_count = threshold)
  built <- ggplot2::ggplot_build(p)
  ## All ribbon/line layers should now have zero rows since no bin meets
  ## the nobs threshold.
  ribbon_layers <- vapply(built$data,
                          function(d) "ymin" %in% colnames(d) && nrow(d) > 0,
                          logical(1))
  expect_false(any(ribbon_layers))
})

test_that("df_vpcstats returns one row per unique bin value", {
  result <- run_vpcstats(testsim_raw)
  obs_bins <- unique(testsim_raw$NTIME[testsim_raw$EVID == 0 &
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
  obs_rows <- testsim_strat$EVID == 0 & !is.na(testsim_strat$NTIME)
  expect_equal(nrow(result),
               nrow(unique(testsim_strat[obs_rows, c("NTIME", "FOOD_f")])))
})

## Test LLOQ handling
test_that("df_vpcstats with loq returns -Inf for observed quantiles when all obs censored", {
  ## Internal contract: df_vpcstats preserves -Inf BLQ encoding from preprocess.
  ## plot_vpc_cont applies var_infna before returning vpcstats=TRUE output.
  result <- run_vpcstats(testsim_raw, loq = 1e6)
  expect_true(all(is.infinite(result$obs5) & result$obs5 < 0))
  expect_true(all(is.infinite(result$obs50) & result$obs50 < 0))
  expect_true(all(is.infinite(result$obs95) & result$obs95 < 0))
})

test_that("plot_vpc_cont(vpcstats=TRUE) converts fully-censored obs quantiles to NA", {
  result <- plot_vpc_cont(sim = testsim_raw, loq = 1e6, vpcstats = TRUE)
  expect_true(all(is.na(result$obs5)))
  expect_true(all(is.na(result$obs50)))
  expect_true(all(is.na(result$obs95)))
})

## Std VPC + loq does NOT touch SIMDV; sim quantiles unchanged across loq settings.
test_that("std VPC sim quantiles are unaffected by loq", {
  base <- run_vpcstats(testsim_raw)
  with_loq <- run_vpcstats(testsim_raw,
                            loq = stats::quantile(testsim_raw$OBSDV, 0.25, na.rm = TRUE))
  expect_equal(base$q5_med,  with_loq$q5_med)
  expect_equal(base$q50_med, with_loq$q50_med)
  expect_equal(base$q95_med, with_loq$q95_med)
})

## Std VPC + loq materially shifts the lower obs quantile (BLQ ranks at -Inf).
test_that("std VPC obs5 shifts when loq censors a fraction of obs", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  base <- run_vpcstats(testsim_raw)
  with_loq <- run_vpcstats(testsim_raw, loq = loq_val)
  ## obs5 in censored bins should be -Inf (rank-based) where base is finite
  censored_bins <- is.infinite(with_loq$obs5) & with_loq$obs5 < 0
  expect_true(any(censored_bins))
  expect_false(identical(base$obs5, with_loq$obs5))
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

##### Test pi / ci validation #####

test_that("df_vpcstats errors on pi outside [0,1]", {
  expect_error(run_vpcstats(testsim_raw, pi = c(-0.1, 0.95)),
               regexp = "in \\[0, 1\\]")
})

test_that("df_vpcstats errors on reversed pi", {
  expect_error(run_vpcstats(testsim_raw, pi = c(0.95, 0.05)),
               regexp = "must be ordered")
})

test_that("df_vpcstats errors on pi of length != 2", {
  expect_error(run_vpcstats(testsim_raw, pi = 0.5),
               regexp = "length-2 numeric")
})

test_that("df_vpcstats errors on ci outside [0,1]", {
  expect_error(run_vpcstats(testsim_raw, ci = c(0, 1.5)),
               regexp = "in \\[0, 1\\]")
})

test_that("plot_vpc_cont errors on scalar ci at boundary", {
  expect_error(plot_vpc_cont(sim = testsim_raw, ci = 1, vpcstats = TRUE),
               regexp = "must be a single numeric value in \\(0, 1\\)")
  expect_error(plot_vpc_cont(sim = testsim_raw, ci = 0, vpcstats = TRUE),
               regexp = "must be a single numeric value in \\(0, 1\\)")
})
