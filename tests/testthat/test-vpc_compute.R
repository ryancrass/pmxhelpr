#####Test df_vpcstats#####

## Setup: shared simulation output. Drop LLOQ so loq stays NULL through the
## pipeline by default; tests that exercise loq behavior pass it explicitly.
testsim_raw <- sim_pk()
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
    strat_var_str = strat_var_str, rep_var_str = "SIM",
    loq = loq
  )
  pmxhelpr:::df_vpccompute(
    pre, pi = pi, ci = ci, bin_var = "BIN_MID",
    strat_var = strat_var_str, rep_var = "SIM",
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

test_that("pc simulated quantiles are NA in bins with no quantifiable obs", {
  result <- run_vpcstats(testsim_raw, loq = 1)
  s <- result$stats
  no_quant <- (s$obs_n - s$obs_n_blq) == 0
  expect_true(any(no_quant))
  pc_sim_cols <- paste0("pc_", pmxhelpr:::.vpc_sim_quantile_cols)
  expect_true(all(is.na(as.matrix(s[no_quant, pc_sim_cols]))))
  ## Std simulated quantiles are untouched
  expect_true(any(!is.na(s$sim_med_med[no_quant])))
})

test_that("df_vpcstats returns one row per unique bin value", {
  result <- run_vpcstats(testsim_raw)
  obs_bins <- unique(testsim_raw$NTIME[testsim_raw$EVID == 0 &
                                         !is.na(testsim_raw$NTIME)])
  expect_equal(nrow(result$stats), length(obs_bins))
})

## Test Stratification
test_that("df_vpcstats with strat_var returns rows for each bin x stratum", {
  testsim_strat <- sim_pk(carry_out = "FOOD")
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
    regexp = "must name variable.*in `data`"
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
  strat_var_str = NULL, rep_var_str = "SIM",
  loq = 1
)
.cof_args <- list(group_vars = "BIN_MID",
                  rep_var = "SIM",
                  pi = c(0.05, 0.95),
                  ci_bounds = c(0.05, 0.95))

test_that("compute_vpcstat: returns a data.frame with the expected column groups", {
  out <- pmxhelpr:::compute_vpcstat(testsim_pre,
                                       group_vars = .cof_args$group_vars,
                                       rep_var = .cof_args$rep_var,
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
                                       rep_var = .cof_args$rep_var,
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
                                       rep_var = .cof_args$rep_var,
                                       pi = .cof_args$pi,
                                       ci_bounds = .cof_args$ci_bounds,
                                       has_loq = TRUE, pcvpc = FALSE)
  pc  <- pmxhelpr:::compute_vpcstat(testsim_pre,
                                       group_vars = .cof_args$group_vars,
                                       rep_var = .cof_args$rep_var,
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
                                       rep_var = "SIM",
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
    strat_var_str = NULL, rep_var_str = "SIM",
    loq = 1e6
  )
  out <- pmxhelpr:::compute_vpcstat(pre_all_blq,
                                       group_vars = .cof_args$group_vars,
                                       rep_var = .cof_args$rep_var,
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
                                       rep_var = .cof_args$rep_var,
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


#####grouped_quantile / compute_vpcstat#####

test_that("grouped_quantile() matches stats::quantile() per group exactly", {
  set.seed(1)
  x <- c(rnorm(40), -Inf, -Inf, 3, NA, NA,   # mixed, with -Inf and NA
         NA, NA,                             # all-NA group
         7,                                  # single value
         2, 5,                               # two values
         -Inf, -Inf, -Inf,                   # all -Inf
         1, 1, 1, 2, 2)                      # ties
  gid <- c(rep(1L, 45), rep(2L, 2), 3L, rep(4L, 2), rep(6L, 3), rep(7L, 5))
  n_groups <- 8L                             # groups 5 and 8 are empty
  for (probs in list(c(0.05, 0.5, 0.95), c(0, 1), c(0.1, 0.9))) {
    got <- grouped_quantile(x, gid, n_groups, probs)
    ref <- t(vapply(seq_len(n_groups), function(g) {
      v <- x[gid == g]
      if (all(is.na(v))) return(rep(NA_real_, length(probs)))
      stats::quantile(v, probs, na.rm = TRUE, names = FALSE)
    }, numeric(length(probs))))
    expect_identical(got, ref)
  }
  ## Shuffled input order gives the same result
  o <- sample(length(x))
  expect_identical(grouped_quantile(x[o], gid[o], n_groups, c(0.05, 0.5, 0.95)),
                   grouped_quantile(x, gid, n_groups, c(0.05, 0.5, 0.95)))
  expect_identical(dim(grouped_quantile(numeric(0), integer(0), 0L, c(0.5, 0.9))),
                   c(0L, 2L))
})

## Reference: compute_vpcstat() as a per-group dplyr::summarise() (the
## implementation before the vectorised rewrite), quantile names dropped.
compute_vpcstat_reference <- function(data, group_vars, rep_var, pi, ci_bounds,
                                      has_loq, pcvpc) {
  q <- function(x, p) stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE)
  stage1 <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, rep_var)))) |>
    dplyr::summarise(
      obs_n = dplyr::n(), obs_n_blq = sum(!is.finite(.data[["OBSDV"]])),
      sim_prop_blq = if (!isTRUE(has_loq)) NA_real_
        else if (isTRUE(pcvpc)) mean(is.na(.data[["SIMDV"]]))
        else mean(dplyr::coalesce((.data[["SIMDV"]] < .data[["LOQ"]]) |
                                    is.na(.data[["SIMDV"]]), FALSE)),
      sim_low = q(.data[["SIMDV"]], pi[1]), sim_med = q(.data[["SIMDV"]], 0.5),
      sim_hi = q(.data[["SIMDV"]], pi[2]), .groups = "drop")
  sim_quant <- stage1 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      obs_n = dplyr::first(.data$obs_n), obs_n_blq = dplyr::first(.data$obs_n_blq),
      sim_low_low = q(.data$sim_low, ci_bounds[1]), sim_low_med = q(.data$sim_low, 0.5),
      sim_low_hi = q(.data$sim_low, ci_bounds[2]),
      sim_med_low = q(.data$sim_med, ci_bounds[1]), sim_med_med = q(.data$sim_med, 0.5),
      sim_med_hi = q(.data$sim_med, ci_bounds[2]),
      sim_hi_low = q(.data$sim_hi, ci_bounds[1]), sim_hi_med = q(.data$sim_hi, 0.5),
      sim_hi_hi = q(.data$sim_hi, ci_bounds[2]),
      sim_prop_blq_low = q(.data$sim_prop_blq, ci_bounds[1]),
      sim_prop_blq_med = q(.data$sim_prop_blq, 0.5),
      sim_prop_blq_hi = q(.data$sim_prop_blq, ci_bounds[2]), .groups = "drop")
  if (!isTRUE(has_loq)) {
    sim_quant <- dplyr::select(sim_quant, -dplyr::starts_with("sim_prop_blq"))
  }
  obs_quant <- data |>
    dplyr::filter(.data[[rep_var]] == 1) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      obs_prop_blq = sum(!is.finite(.data[["OBSDV"]])) / dplyr::n(),
      obs_low = q(.data[["OBSDV"]], pi[1]), obs_med = q(.data[["OBSDV"]], 0.5),
      obs_hi = q(.data[["OBSDV"]], pi[2]), .groups = "drop")
  dplyr::left_join(sim_quant, obs_quant, by = group_vars) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(.vpc_quantile_cols), var_infna))
}

test_that("compute_vpcstat() matches the per-group summarise() reference", {
  sim <- sim_pk(carry_out = c("FOOD", "LLOQ"))
  pre <- suppressMessages(df_vpcpreprocess(
    sim, time_var_str = "TIME", ntime_var_str = "NTIME", pred_var_str = "PRED",
    sim_dv_var_str = "SIMDV", obs_dv_var_str = "OBSDV", strat_var_str = "FOOD",
    rep_var_str = "SIM"))
  pre_pc <- pre
  pre_pc$SIMDV <- var_infna(var_loqcens(pre_pc$SIMDV, loq = pre_pc$LOQ))
  for (groups in list("BIN_MID", c("BIN_MID", "FOOD"))) {
    for (case in list(list(d = pre, has_loq = TRUE, pcvpc = FALSE),
                      list(d = pre, has_loq = FALSE, pcvpc = FALSE),
                      list(d = pre_pc, has_loq = FALSE, pcvpc = TRUE))) {
      args <- list(case$d, groups, "SIM", c(0.05, 0.95), c(0.05, 0.95),
                   case$has_loq, case$pcvpc)
      expect_identical(do.call(compute_vpcstat, args),
                       do.call(compute_vpcstat_reference, args))
    }
  }
})


##### Interval bins (bin_breaks) #####

test_that("vpc_interval_bins() cuts actual time into [a, b) intervals at their midpoints", {
  d <- data.frame(SIM = 1, TIME = c(0, 0.99, 1, 3.9, 4, 8, 9, NA))
  expect_warning(
    out <- pmxhelpr:::vpc_interval_bins(d, c(0, 1, 4, 8), "SIM"),
    "2 records per replicate fall outside `bin_breaks` \\(0-8\\) and are dropped",
    class = "pmxhelpr_warning_data"
  )
  expect_equal(out$TIME, c(0, 0.99, 1, 3.9, 4, 8))
  ## A record on a break joins the later bin; the last break is closed.
  expect_equal(out$BIN_MID, c(0.5, 0.5, 2.5, 2.5, 6, 6))
})

test_that("the out-of-range warning counts records per replicate", {
  d <- data.frame(SIM = rep(1:3, each = 3), TIME = rep(c(1, 2, 50), 3))
  expect_warning(pmxhelpr:::vpc_interval_bins(d, c(0, 24), "SIM"),
                 "^1 record per replicate falls outside .* and is dropped")
})

test_that("interval bins that isolate each nominal time reproduce nominal binning", {
  sim <- testsim_raw
  sim$TIME <- sim$NTIME
  ntimes <- sort(unique(sim$NTIME[sim$EVID == 0]))
  breaks <- c(ntimes[1] - 0.25, (head(ntimes, -1) + tail(ntimes, -1)) / 2,
              ntimes[length(ntimes)] + 0.25)
  nominal <- df_vpcstats(sim, loq = 1)
  binned <- df_vpcstats(sim, loq = 1, bin_breaks = breaks)
  midpoints <- (head(breaks, -1) + tail(breaks, -1)) / 2
  expect_equal(binned$stats$BIN_MID, midpoints[match(nominal$stats$BIN_MID, ntimes)])
  cols <- setdiff(names(nominal$stats), "BIN_MID")
  expect_identical(binned$stats[cols], nominal$stats[cols])
  expect_equal(binned$config$bin_breaks, breaks)
  expect_true("bin_breaks" %in% names(nominal$config))
  expect_null(nominal$config$bin_breaks)
})

test_that("df_vpcstats() bins by interval within each stratum", {
  sim <- sim_pk(5, carry_out = "FOOD")
  breaks <- c(0, 2, 8, 24, 72, 200)
  out <- df_vpcstats(sim, loq = 1, strat_var = FOOD, bin_breaks = breaks)
  expect_setequal(unique(out$stats$BIN_MID), (head(breaks, -1) + tail(breaks, -1)) / 2)
  obs1 <- sim[sim$SIM == 1 & sim$EVID == 0, ]
  expect_equal(sum(out$stats$obs_n), nrow(obs1))
  expect_equal(nrow(out$stats), length(unique(paste(
    findInterval(obs1$TIME, breaks, rightmost.closed = TRUE), obs1$FOOD))))
})

test_that("bin_breaks is validated and is a pipeline-only argument", {
  for (bad in list(5, c(0, 5, 2), c(0, 0, 5), c(0, NA, 5), c(0, Inf), "0,24")) {
    expect_error(df_vpcstats(testsim_raw, bin_breaks = bad), class = "pmxhelpr_error_bad_arg")
  }
  vs <- df_vpcstats(testsim_raw)
  expect_error(plot_vpc_cont(vs, bin_breaks = c(0, 24, 168)), "bin_breaks")
  expect_error(plot_vpc_cens(df_vpcstats(testsim_raw, loq = 1), bin_breaks = c(0, 24, 168)),
               "bin_breaks")
  expect_s3_class(suppressWarnings(plot_vpc_cont(testsim_raw, bin_breaks = c(0, 4, 24, 200))),
                  "ggplot")
  expect_s3_class(suppressWarnings(plot_vpc_cens(testsim_raw, loq = 1, bin_breaks = c(0, 4, 24, 200))),
                  "ggplot")
})

test_that("stratified stats are ordered by stratum, then bin", {
  sim <- sim_pk(5, carry_out = "FOOD")
  for (bins in list(NULL, bins_equal(4))) {
    s <- df_vpcstats(sim, strat_var = FOOD, bin_breaks = bins)$stats
    expect_equal(order(s$FOOD, s$BIN_MID), seq_len(nrow(s)))
  }
})
