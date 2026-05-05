#####Test df_vpcstats#####

## Setup: shared simulation output. Drop LLOQ so loq stays NULL through the
## pipeline by default; tests that exercise loq behavior pass it explicitly.
testsim_raw <- df_mrgsim_replicate(data = dplyr::filter(data_sad, CMT != 3),
                                   model = model_mread_load("pkmodel"),
                                   replicates = 10,
                                   dv_var = "ODV")
testsim_raw$LLOQ <- NULL

## Helper: preprocess + compute, returning list(stats, obs) — matches public
## df_vpcstats() shape. df_vpccompute always emits both std and pc columns;
## individual tests select between them via the std (unprefixed) or pc_* cols.
run_vpcstats <- function(sim, strat_var_str = NULL,
                         lower_bound = 0, loq = NULL, mode = "auto",
                         pi = c(0.05, 0.95), ci = 0.90,
                         time_var_str = "TIME", ntime_var_str = "NTIME",
                         pred_var_str = "PRED",
                         sim_dv_var_str = "SIMDV", obs_dv_var_str = "OBSDV") {
  pre <- pmxhelpr:::df_vpcpreprocess(
    sim,
    time_var_str = time_var_str, ntime_var_str = ntime_var_str,
    pred_var_str = pred_var_str,
    sim_dv_var_str = sim_dv_var_str, obs_dv_var_str = obs_dv_var_str,
    strat_var_str = strat_var_str, irep_name_str = "SIM",
    loq = loq
  )
  pmxhelpr:::df_vpccompute(
    pre, pi = pi, ci = ci, bin_var = "BIN_MID",
    strat_var = strat_var_str, irep_name = "SIM",
    lower_bound = lower_bound, mode = mode
  )
}

## Test Output
test_that("df_vpcstats returns a list with stats and obs data.frames", {
  result <- run_vpcstats(testsim_raw)
  expect_type(result, "list")
  expect_named(result, c("stats", "obs"))
  expect_s3_class(result$stats, "data.frame")
  expect_s3_class(result$obs, "data.frame")
})

test_that("df_vpcstats returns expected columns without loq", {
  result <- run_vpcstats(testsim_raw)
  expected_cols <- c("BIN_MID", "obs_n", "obs_n_blq", "obs_prop_blq",
                     "sim_low_low", "sim_low_med", "sim_low_hi",
                     "sim_med_low", "sim_med_med", "sim_med_hi",
                     "sim_hi_low",  "sim_hi_med",  "sim_hi_hi",
                     "obs_low", "obs_med", "obs_hi")
  expect_true(all(expected_cols %in% colnames(result$stats)))
  ## sim_prop_blq_* gated on loq presence
  expect_false(any(c("sim_prop_blq_low", "sim_prop_blq_med", "sim_prop_blq_hi")
                    %in% colnames(result$stats)))
})

test_that("df_vpcstats appends sim_prop_blq_* when loq is supplied", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  result <- run_vpcstats(testsim_raw, loq = loq_val)
  expect_true(all(c("sim_prop_blq_low", "sim_prop_blq_med", "sim_prop_blq_hi")
                   %in% colnames(result$stats)))
})

test_that("df_vpcstats: obs_prop_blq == obs_n_blq / obs_n and obs_n_blq <= obs_n", {
  result_default <- run_vpcstats(testsim_raw)
  expect_equal(result_default$stats$obs_prop_blq,
               result_default$stats$obs_n_blq / result_default$stats$obs_n)
  expect_true(all(result_default$stats$obs_n_blq <= result_default$stats$obs_n))

  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  result_loq <- run_vpcstats(testsim_raw, loq = loq_val)
  expect_equal(result_loq$stats$obs_prop_blq,
               result_loq$stats$obs_n_blq / result_loq$stats$obs_n)
  expect_true(all(result_loq$stats$obs_n_blq <= result_loq$stats$obs_n))
})

test_that("df_vpcstats: loq increases obs_n_blq (more obs below threshold flagged)", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  result_default <- run_vpcstats(testsim_raw)
  result_loq <- run_vpcstats(testsim_raw, loq = loq_val)
  expect_equal(result_loq$stats$obs_n, result_default$stats$obs_n)
  expect_true(any(result_loq$stats$obs_n_blq > result_default$stats$obs_n_blq))
})

test_that("df_vpcstats: sim_prop_blq_low <= sim_prop_blq_med <= sim_prop_blq_hi", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  result <- run_vpcstats(testsim_raw, loq = loq_val)
  expect_true(all(result$stats$sim_prop_blq_low <= result$stats$sim_prop_blq_med, na.rm = TRUE))
  expect_true(all(result$stats$sim_prop_blq_med <= result$stats$sim_prop_blq_hi, na.rm = TRUE))
})

test_that("std VPC sim_prop_blq is driven by SIMDV < loq comparison", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  result <- run_vpcstats(testsim_raw, loq = loq_val)
  ## Reference uses the same expression as the implementation (handles any
  ## NA SIMDV the same way) so this validates the two-stage aggregation
  ## rather than the formula choice. Std-VPC mode means no SIMDV encoding
  ## was applied in preprocess, so SIMDV here is the raw simulation output.
  ref <- testsim_raw |>
    dplyr::filter(EVID == 0) |>
    dplyr::group_by(NTIME, SIM) |>
    dplyr::summarise(p = mean((SIMDV < loq_val) | is.na(SIMDV)),
                     .groups = "drop") |>
    dplyr::group_by(NTIME) |>
    dplyr::summarise(p_med = stats::quantile(p, 0.5, na.rm = TRUE),
                     .groups = "drop")
  expect_equal(result$stats$sim_prop_blq_med, ref$p_med, ignore_attr = TRUE)
})

test_that("sim_prop_blq is std-only; the pc flavor does not emit a pc_sim_prop_blq_* set", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  result <- run_vpcstats(testsim_raw, loq = loq_val)
  expect_true(all(c("sim_prop_blq_low", "sim_prop_blq_med", "sim_prop_blq_hi") %in%
                    colnames(result$stats)))
  expect_false(any(c("pc_sim_prop_blq_low", "pc_sim_prop_blq_med", "pc_sim_prop_blq_hi") %in%
                     colnames(result$stats)))
})

test_that("min_bin_count gates on (obs_n - obs_n_blq), not total record count", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  result <- df_vpcstats(testsim_raw, loq = loq_val)
  threshold <- max(result$stats$obs_n - result$stats$obs_n_blq) + 1L
  p <- plot_vpc_cont(data = testsim_raw, loq = loq_val,
                     min_bin_count = threshold)
  built <- ggplot2::ggplot_build(p)
  ribbon_layers <- vapply(built$data,
                          function(d) "ymin" %in% colnames(d) && nrow(d) > 0,
                          logical(1))
  expect_false(any(ribbon_layers))
})

test_that("df_vpcstats returns one row per unique bin value", {
  result <- run_vpcstats(testsim_raw)
  obs_bins <- unique(testsim_raw$NTIME[testsim_raw$EVID == 0 &
                                         !is.na(testsim_raw$NTIME)])
  expect_equal(nrow(result$stats), length(obs_bins))
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
  expect_true("FOOD_f" %in% colnames(result$stats))
  obs_rows <- testsim_strat$EVID == 0 & !is.na(testsim_strat$NTIME)
  expect_equal(nrow(result$stats),
               nrow(unique(testsim_strat[obs_rows, c("NTIME", "FOOD_f")])))
})

## Test LLOQ handling
test_that("df_vpcstats with loq returns -Inf for observed quantiles when all obs censored", {
  ## Internal contract: df_vpccompute applies var_infna to stats columns
  ## before returning, so callers always see NA (not -Inf) for fully-censored
  ## bins. This validates that the cleanup happens.
  result <- run_vpcstats(testsim_raw, loq = 1e6)
  expect_true(all(is.na(result$stats$obs_low)))
  expect_true(all(is.na(result$stats$obs_med)))
  expect_true(all(is.na(result$stats$obs_hi)))
})

test_that("df_vpcstats() converts fully-censored obs quantiles to NA", {
  result <- df_vpcstats(testsim_raw, loq = 1e6)
  expect_true(all(is.na(result$stats$obs_low)))
  expect_true(all(is.na(result$stats$obs_med)))
  expect_true(all(is.na(result$stats$obs_hi)))
})

## Std VPC + loq does NOT touch SIMDV; sim quantiles unchanged across loq settings.
test_that("std VPC sim quantiles are unaffected by loq", {
  base <- run_vpcstats(testsim_raw)
  with_loq <- run_vpcstats(testsim_raw,
                            loq = stats::quantile(testsim_raw$OBSDV, 0.25, na.rm = TRUE))
  expect_equal(base$stats$sim_low_med, with_loq$stats$sim_low_med)
  expect_equal(base$stats$sim_med_med, with_loq$stats$sim_med_med)
  expect_equal(base$stats$sim_hi_med,  with_loq$stats$sim_hi_med)
})

## Std VPC + loq materially shifts the lower obs quantile (BLQ ranks at -Inf
## then masked to NA by var_infna).
test_that("std VPC obs_low shifts when loq censors a fraction of obs", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  base <- run_vpcstats(testsim_raw)
  with_loq <- run_vpcstats(testsim_raw, loq = loq_val)
  ## obs_low in fully-censored bins is NA (rank-based -Inf masked); base is finite
  censored_bins <- is.na(with_loq$stats$obs_low)
  expect_true(any(censored_bins))
  expect_false(identical(base$stats$obs_low, with_loq$stats$obs_low))
})

##### Test mode argument (rank vs drop) #####

test_that("var_predcorr passes -Inf through unchanged (rank-mode invariant)", {
  out <- pmxhelpr::var_predcorr(c(-Inf, 5, 10), c(2, 3, 4), lower_bound = 0)
  expect_true(is.infinite(out[1]) && out[1] < 0)
  expect_true(all(is.finite(out[-1])))
})

test_that("std VPC mode='drop' shifts obs_low vs default rank mode", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  rank_stats <- run_vpcstats(testsim_raw, loq = loq_val, mode = "rank")
  drop_stats <- run_vpcstats(testsim_raw, loq = loq_val, mode = "drop")
  ## In rank, BLQ-heavy bins return -Inf at obs_low (then NA); in drop, those
  ## bins return a finite quantile (the lowest quantifiable obs). At least one
  ## bin must differ.
  rank_na <- is.na(rank_stats$stats$obs_low)
  drop_finite <- is.finite(drop_stats$stats$obs_low)
  expect_true(any(rank_na & drop_finite))
})

test_that("pcVPC rank-mode propagates -Inf through prediction-correction; masked to NA", {
  rank_stats <- run_vpcstats(testsim_raw, loq = 1e6, mode = "rank")
  ## With loq=1e6 every obs is BLQ; rank mode keeps -Inf encoding through
  ## var_predcorr in the pc branch, then var_infna masks to NA before return.
  expect_true(any(is.na(rank_stats$stats$pc_obs_low)))
})

test_that("mode='auto' applies rank to std flavor and drop to pc flavor", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  auto_stats <- run_vpcstats(testsim_raw, loq = loq_val, mode = "auto")
  rank_stats <- run_vpcstats(testsim_raw, loq = loq_val, mode = "rank")
  drop_stats <- run_vpcstats(testsim_raw, loq = loq_val, mode = "drop")
  ## std columns: auto == rank
  expect_equal(auto_stats$stats$obs_low, rank_stats$stats$obs_low)
  expect_equal(auto_stats$stats$sim_med_med, rank_stats$stats$sim_med_med)
  ## pc columns: auto == drop
  expect_equal(auto_stats$stats$pc_obs_low, drop_stats$stats$pc_obs_low)
  expect_equal(auto_stats$stats$pc_sim_med_med, drop_stats$stats$pc_sim_med_med)
})

## Test quantile ordering
test_that("simulated quantile medians are ordered sim_low_med <= sim_med_med <= sim_hi_med", {
  result <- run_vpcstats(testsim_raw)
  expect_true(all(result$stats$sim_low_med <= result$stats$sim_med_med, na.rm = TRUE))
  expect_true(all(result$stats$sim_med_med <= result$stats$sim_hi_med,  na.rm = TRUE))
})

#####Test Preprocessing#####

test_that("df_vpcpreprocess validates column existence", {
  expect_error(
    run_vpcstats(testsim_raw, time_var_str = "NONEXIST"),
    regexp = "must be variable.*in `data`"
  )
})

test_that("std and pc median quantiles differ when prediction-correction is meaningful", {
  result <- run_vpcstats(testsim_raw)
  expect_false(identical(result$stats$sim_med_med, result$stats$pc_sim_med_med))
})

test_that("loq with pcvpc semantics: pc-flavor BLQ encoding before pred-correction shifts pc medians", {
  loq_val <- stats::quantile(testsim_raw$OBSDV, 0.25, na.rm = TRUE)
  with_loq <- run_vpcstats(testsim_raw, loq = loq_val)
  no_loq   <- run_vpcstats(testsim_raw)
  expect_s3_class(with_loq$stats, "data.frame")
  expect_false(identical(with_loq$stats$pc_sim_med_med, no_loq$stats$pc_sim_med_med))
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

test_that("df_vpcstats errors on ci outside (0,1)", {
  expect_error(run_vpcstats(testsim_raw, ci = 1.5),
               regexp = "must be a single numeric value in \\(0, 1\\)")
})

test_that("plot_vpc_cont errors on scalar ci at boundary", {
  expect_error(df_vpcstats(testsim_raw, ci = 1),
               regexp = "must be a single numeric value in \\(0, 1\\)")
  expect_error(df_vpcstats(testsim_raw, ci = 0),
               regexp = "must be a single numeric value in \\(0, 1\\)")
})
