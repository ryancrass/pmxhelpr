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
test_that("df_vpcstats returns a pmx_stats container with stats, obs, and config slots", {
  result <- run_vpcstats(testsim_raw)
  expect_type(result, "list")
  expect_s3_class(result, "vpc_stats")
  expect_s3_class(result, "pmx_stats")
  expect_named(result, c("stats", "obs", "config"))
  expect_s3_class(result$stats, "data.frame")
  expect_s3_class(result$obs, "data.frame")
  expect_type(result$config, "list")
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
    carry_out = "FOOD")
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

test_that("mode='drop' drops pc OBSDV and SIMDV BLQ before pred-correction", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  drop_stats <- run_vpcstats(testsim_raw, loq = loq_val, mode = "drop")
  no_loq     <- run_vpcstats(testsim_raw, mode = "drop")
  ## SIMDV encoding-then-drop in pc flavor must shift pc sim medians vs no-loq
  expect_false(identical(drop_stats$stats$pc_sim_med_med,
                         no_loq$stats$pc_sim_med_med))
  ## pc quantile cols must be finite or NA — never -Inf (var_infna applied)
  pc_q_cols <- c("pc_obs_low", "pc_sim_low_med")
  for (col in pc_q_cols) {
    expect_false(any(is.infinite(drop_stats$stats[[col]])),
                 info = paste0("drop-mode pc col `", col, "` contained -Inf"))
  }
})

test_that("pc flavor with loq=NULL excludes MDV==1 SIMDV rows from pc quantiles", {
  sim_extreme <- testsim_raw
  mdv1_idx <- which(sim_extreme$EVID == 0 & sim_extreme$MDV == 1)
  sim_extreme$SIMDV[mdv1_idx] <- 1e6

  base    <- run_vpcstats(testsim_raw)
  extreme <- run_vpcstats(sim_extreme)

  expect_equal(base$stats$pc_sim_med_med, extreme$stats$pc_sim_med_med)
  expect_equal(base$stats$pc_obs_med,     extreme$stats$pc_obs_med)
})

test_that("mode='rank' pc flavor: -Inf survives pred-correction, then masked to NA", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  rank_stats <- run_vpcstats(testsim_raw, loq = loq_val, mode = "rank")
  ## var_infna runs on quantile cols, so no -Inf should leak out
  q_cols <- c("pc_obs_low", "pc_obs_med", "pc_obs_hi",
              "pc_sim_low_med", "pc_sim_med_med", "pc_sim_hi_med")
  for (col in q_cols) {
    expect_false(any(is.infinite(rank_stats$stats[[col]])),
                 info = paste0("pc rank-mode column `", col, "` contained -Inf"))
  }
})

test_that("pcVPC mode='rank' and mode='drop' yield different pc obs/sim quantiles", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  rank_stats <- run_vpcstats(testsim_raw, loq = loq_val, mode = "rank")
  drop_stats <- run_vpcstats(testsim_raw, loq = loq_val, mode = "drop")
  ## At least one quantile column must differ between modes in pc flavor
  expect_false(identical(rank_stats$stats$pc_obs_low,
                         drop_stats$stats$pc_obs_low))
  expect_false(identical(rank_stats$stats$pc_sim_low_med,
                         drop_stats$stats$pc_sim_low_med))
})

test_that("std VPC sim quantiles invariant across mode (rank vs drop) with loq", {
  loq_val <- stats::quantile(testsim_raw$OBSDV[testsim_raw$EVID == 0],
                              0.25, na.rm = TRUE)
  rank_stats <- run_vpcstats(testsim_raw, loq = loq_val, mode = "rank")
  drop_stats <- run_vpcstats(testsim_raw, loq = loq_val, mode = "drop")
  ## std SIMDV is never encoded; mode must not affect sim_*_med columns
  expect_equal(rank_stats$stats$sim_low_med, drop_stats$stats$sim_low_med)
  expect_equal(rank_stats$stats$sim_med_med, drop_stats$stats$sim_med_med)
  expect_equal(rank_stats$stats$sim_hi_med,  drop_stats$stats$sim_hi_med)
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

##### Direct tests for compute_vpcstat() #####

## Setup: preprocessed test data.
testsim_pre <- pmxhelpr:::df_vpcpreprocess(
  testsim_raw,
  time_var_str = "TIME", ntime_var_str = "NTIME",
  pred_var_str = "PRED",
  sim_dv_var_str = "SIMDV", obs_dv_var_str = "OBSDV",
  strat_var_str = NULL, irep_name_str = "SIM",
  loq = 1
)
.cof_args <- list(group_vars = "BIN_MID",
                  irep_name = "SIM",
                  pi = c(0.05, 0.95),
                  ci_bounds = c(0.05, 0.95))

test_that("compute_vpcstat: returns a data.frame with the expected column groups", {
  out <- pmxhelpr:::compute_vpcstat(testsim_pre,
                                       group_vars = .cof_args$group_vars,
                                       irep_name = .cof_args$irep_name,
                                       pi = .cof_args$pi,
                                       ci_bounds = .cof_args$ci_bounds,
                                       has_loq = TRUE, pcvpc = FALSE)
  expect_s3_class(out, "data.frame")
  expected <- c("BIN_MID",
                "obs_n", "obs_n_blq", "obs_prop_blq",
                "sim_prop_blq_low", "sim_prop_blq_med", "sim_prop_blq_hi",
                "sim_low_low", "sim_low_med", "sim_low_hi",
                "sim_med_low", "sim_med_med", "sim_med_hi",
                "sim_hi_low",  "sim_hi_med",  "sim_hi_hi",
                "obs_low", "obs_med", "obs_hi")
  expect_true(all(expected %in% colnames(out)))
})

test_that("compute_vpcstat: has_loq = FALSE omits the sim_prop_blq_* columns", {
  out <- pmxhelpr:::compute_vpcstat(testsim_pre,
                                       group_vars = .cof_args$group_vars,
                                       irep_name = .cof_args$irep_name,
                                       pi = .cof_args$pi,
                                       ci_bounds = .cof_args$ci_bounds,
                                       has_loq = FALSE, pcvpc = FALSE)
  expect_false(any(c("sim_prop_blq_low", "sim_prop_blq_med", "sim_prop_blq_hi")
                    %in% colnames(out)))
})

test_that("compute_vpcstat: pcvpc flag drives the sim_prop_blq detection branch", {
  ## With pcvpc = TRUE the detection uses is.na(SIMDV); with pcvpc = FALSE it
  ## uses (SIMDV < loq) | is.na(SIMDV). On unmodified preprocessed data
  ## (no pred-correction applied), SIMDV is raw and has no NAs, so the std
  ## branch counts BLQ via the < loq test and the pc branch counts zero.
  std <- pmxhelpr:::compute_vpcstat(testsim_pre,
                                       group_vars = .cof_args$group_vars,
                                       irep_name = .cof_args$irep_name,
                                       pi = .cof_args$pi,
                                       ci_bounds = .cof_args$ci_bounds,
                                       has_loq = TRUE, pcvpc = FALSE)
  pc  <- pmxhelpr:::compute_vpcstat(testsim_pre,
                                       group_vars = .cof_args$group_vars,
                                       irep_name = .cof_args$irep_name,
                                       pi = .cof_args$pi,
                                       ci_bounds = .cof_args$ci_bounds,
                                       has_loq = TRUE, pcvpc = TRUE)
  expect_true(any(std$sim_prop_blq_med > 0, na.rm = TRUE))
  expect_true(all(pc$sim_prop_blq_med == 0, na.rm = TRUE))
})

test_that("compute_vpcstat: NA-LOQ rows with finite SIMDV count as non-BLQ in denominator", {
  ## Regression for PR#19 review (jacobdum): when LOQ is NA on a row with a
  ## finite SIMDV, the comparison `SIMDV < LOQ` returns NA. The previous impl
  ## used `na.rm = TRUE`, dropping those rows from both numerator and
  ## denominator. The fix coalesces NA -> FALSE so such rows count as
  ## non-BLQ.
  ##
  ## One bin, one replicate, four rows:
  ##   LOQ = 5,  SIMDV = 3   -> BLQ
  ##   LOQ = 5,  SIMDV = 10  -> not BLQ
  ##   LOQ = NA, SIMDV = 20  -> not BLQ (under fix); dropped (under bug)
  ##   LOQ = NA, SIMDV = NA  -> BLQ
  ## Correct proportion = 2/4 = 0.5; buggy proportion = 2/3.
  pre <- data.frame(
    BIN_MID = rep(1, 4),
    SIM     = rep(1, 4),
    OBSDV   = c(3, 10, 20, NA),
    SIMDV   = c(3, 10, 20, NA),
    LOQ     = c(5, 5, NA, NA)
  )
  out <- pmxhelpr:::compute_vpcstat(pre,
                                       group_vars = "BIN_MID",
                                       irep_name = "SIM",
                                       pi = c(0.05, 0.95),
                                       ci_bounds = c(0.05, 0.95),
                                       has_loq = TRUE, pcvpc = FALSE)
  ## With a single replicate, sim_prop_blq_{low,med,hi} all equal the
  ## per-replicate proportion.
  expect_equal(out$sim_prop_blq_med, 0.5, ignore_attr = TRUE)
})

test_that("compute_vpcstat: var_infna is applied — no -Inf in quantile cols", {
  ## Push every observation BLQ via a huge loq; OBSDV becomes -Inf in
  ## preprocess, then ranks low at quantile, then should be masked to NA by
  ## var_infna inside compute_vpcstat.
  pre_all_blq <- pmxhelpr:::df_vpcpreprocess(
    testsim_raw,
    time_var_str = "TIME", ntime_var_str = "NTIME",
    pred_var_str = "PRED",
    sim_dv_var_str = "SIMDV", obs_dv_var_str = "OBSDV",
    strat_var_str = NULL, irep_name_str = "SIM",
    loq = 1e6
  )
  out <- pmxhelpr:::compute_vpcstat(pre_all_blq,
                                       group_vars = .cof_args$group_vars,
                                       irep_name = .cof_args$irep_name,
                                       pi = .cof_args$pi,
                                       ci_bounds = .cof_args$ci_bounds,
                                       has_loq = TRUE, pcvpc = FALSE)
  q_cols <- c("obs_low", "obs_med", "obs_hi",
              "sim_low_low", "sim_low_med", "sim_low_hi")
  for (col in q_cols) {
    expect_false(any(is.infinite(out[[col]])),
                 info = paste0("column `", col, "` contained -Inf"))
  }
})

test_that("compute_vpcstat: obs_n equals records-per-bin (single replicate)", {
  out <- pmxhelpr:::compute_vpcstat(testsim_pre,
                                       group_vars = .cof_args$group_vars,
                                       irep_name = .cof_args$irep_name,
                                       pi = .cof_args$pi,
                                       ci_bounds = .cof_args$ci_bounds,
                                       has_loq = FALSE, pcvpc = FALSE)
  ## Reference: rows per bin in replicate 1 (preprocess already filtered EVID==0)
  ref <- testsim_pre |>
    dplyr::filter(SIM == 1) |>
    dplyr::group_by(BIN_MID) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")
  expect_equal(out$obs_n, ref$n, ignore_attr = TRUE)
})

test_that("plot_vpc_cont errors on scalar ci at boundary", {
  expect_error(df_vpcstats(testsim_raw, ci = 1),
               regexp = "must be a single numeric value in \\(0, 1\\)")
  expect_error(df_vpcstats(testsim_raw, ci = 0),
               regexp = "must be a single numeric value in \\(0, 1\\)")
})

test_that("df_vpcstats warns when simout contains multiple CMT values after EVID filter", {
  testsim_multi <- testsim_raw
  obs_idx <- which(testsim_multi$EVID == 0)[1:10]
  testsim_multi$CMT[obs_idx] <- 3
  expect_warning(df_vpcstats(testsim_multi),
                 regexp = "Multiple unique values of `CMT`")
})
