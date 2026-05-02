#' Compute VPC summary statistics from preprocessed simulation data
#'
#' @description
#' Computes two-stage quantile summary statistics for VPC plots.
#' Can be called directly on preprocessed data or accessed via
#' `plot_vpc_cont(vpcstats = TRUE)`, which handles preprocessing automatically.
#'
#' @param sim Preprocessed simulation data containing `SIMDV`, `OBSDV`,
#'    `MDV`, and a replicate identifier column. Typically output from
#'    `df_mrgsim_replicate()` after internal preprocessing by `plot_vpc_cont()`.
#'    BLQ encoding (`-Inf` or `NA`) is expected to be applied upstream by
#'    `df_vpcpreprocess()`.
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles.
#'    Default is `c(0.05, 0.95)`.
#' @param ci Numeric vector of length 2 specifying confidence interval quantiles.
#'    Default is `c(0.05, 0.95)`.
#' @param bin_var String. Binning variable name. Default is `"BIN_MID"`.
#' @param strat_var_str String or `NULL`. Stratification variable name. Default is `NULL`.
#' @param irep_name_str String. Replicate identifier column name. Default is `"SIM"`.
#' @param loq Numeric value of the lower limit of quantification, or `NULL`.
#'    When supplied, the result includes simulated BLQ proportion CIs
#'    (`sim_prop_blq_low`, `sim_prop_blq_med`, `sim_prop_blq_hi`).
#' @param pcvpc Logical. `TRUE` if `sim` came from prediction-corrected
#'    preprocessing (`SIMDV` is on the PC scale and BLQ rows are `NA`),
#'    `FALSE` for std VPC (`SIMDV` is raw). Selects the BLQ-detection
#'    expression for `sim_prop_blq`: `is.na(SIMDV)` in pcVPC mode (raw `<loq`
#'    is meaningless on the PC scale), `(SIMDV < loq) | is.na(SIMDV)` in std
#'    VPC mode. Default is `FALSE`.
#'
#' @return A `data.frame` with bin counts (`nbin` total records, `nobsblq`
#'    BLQ-encoded observations), the observed BLQ proportion (`obs_prop_blq`),
#'    simulated quantile CIs (`q5_low`, `q5_med`, `q5_hi`, `q50_low`, `q50_med`,
#'    `q50_hi`, `q95_low`, `q95_med`, `q95_hi`), and observed quantiles
#'    (`obs5`, `obs50`, `obs95`), joined by `bin_var`. When `loq` is supplied,
#'    `sim_prop_blq_low/med/hi` columns are appended. Quantile values may be
#'    `-Inf` for fully BLQ-censored bins; callers should apply [`var_infna()`]
#'    before plotting.
#' @export df_vpcstats

df_vpcstats <- function(sim, pi, ci, bin_var, strat_var_str, irep_name_str,
                        loq = NULL, pcvpc = FALSE) {

  check_quantile_pair(pi, "pi")
  check_quantile_pair(ci, "ci")

  group_vars <- c(bin_var)
  if (!is.null(strat_var_str)) group_vars <- c(bin_var, strat_var_str)

  ## Stage 1: Per-replicate quantiles on simulated data.
  ## nbin counts all records; nobsblq counts BLQ-encoded observations
  ## (non-finite OBSDV). When `loq` is supplied, sim_prop_blq is the per-
  ## replicate fraction of simulated values flagged as BLQ. The detection
  ## expression depends on mode: in std VPC, SIMDV is raw, so we count
  ## `(SIMDV < loq) | is.na(SIMDV)`. In pcVPC, SIMDV is on the prediction-
  ## corrected scale where the raw `loq` no longer maps to a single
  ## threshold, so we trust the `is.na` encoding (BLQ rows were masked to
  ## NA before var_predcorr ran).
  stage1 <- sim |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, irep_name_str)))) |>
    dplyr::summarise(
      nbin    = dplyr::n(),
      nobsblq = sum(!is.finite(.data[["OBSDV"]])),
      sim_prop_blq = if (is.null(loq)) NA_real_
        else if (isTRUE(pcvpc)) mean(is.na(.data[["SIMDV"]]))
        else mean((.data[["SIMDV"]] < loq) | is.na(.data[["SIMDV"]])),
      q_lo    = stats::quantile(.data[["SIMDV"]], probs = pi[1], na.rm = TRUE),
      q50     = stats::quantile(.data[["SIMDV"]], probs = 0.5,   na.rm = TRUE),
      q_hi    = stats::quantile(.data[["SIMDV"]], probs = pi[2], na.rm = TRUE),
      .groups = "drop"
    )

  ## Stage 2: CIs across replicates
  sim_quant <- stage1 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      nbin    = dplyr::first(nbin),
      nobsblq = dplyr::first(nobsblq),
      q5_low  = stats::quantile(q_lo, probs = ci[1], na.rm = TRUE),
      q5_med  = stats::quantile(q_lo, probs = 0.5,   na.rm = TRUE),
      q5_hi   = stats::quantile(q_lo, probs = ci[2], na.rm = TRUE),
      q50_low = stats::quantile(q50,  probs = ci[1], na.rm = TRUE),
      q50_med = stats::quantile(q50,  probs = 0.5,   na.rm = TRUE),
      q50_hi  = stats::quantile(q50,  probs = ci[2], na.rm = TRUE),
      q95_low = stats::quantile(q_hi, probs = ci[1], na.rm = TRUE),
      q95_med = stats::quantile(q_hi, probs = 0.5,   na.rm = TRUE),
      q95_hi  = stats::quantile(q_hi, probs = ci[2], na.rm = TRUE),
      sim_prop_blq_low = stats::quantile(sim_prop_blq, probs = ci[1], na.rm = TRUE),
      sim_prop_blq_med = stats::quantile(sim_prop_blq, probs = 0.5,   na.rm = TRUE),
      sim_prop_blq_hi  = stats::quantile(sim_prop_blq, probs = ci[2], na.rm = TRUE),
      .groups = "drop"
    )
  if (is.null(loq)) {
    sim_quant <- dplyr::select(sim_quant, -dplyr::any_of(c(
      "sim_prop_blq_low", "sim_prop_blq_med", "sim_prop_blq_hi")))
  }

  ## Observed quantiles from first replicate
  obs_quant <- sim |>
    dplyr::filter(.data[[irep_name_str]] == 1) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      obs_prop_blq = sum(!is.finite(.data[["OBSDV"]])) / dplyr::n(),
      obs5  = stats::quantile(.data[["OBSDV"]], probs = pi[1], na.rm = TRUE),
      obs50 = stats::quantile(.data[["OBSDV"]], probs = 0.5,   na.rm = TRUE),
      obs95 = stats::quantile(.data[["OBSDV"]], probs = pi[2], na.rm = TRUE),
      .groups = "drop"
    )

  dplyr::left_join(sim_quant, obs_quant, by = group_vars)
}


#' Preprocess simulation data for VPC statistics
#'
#' @description
#' Internal function. Validates columns, renames to standard names, drops
#' dose rows, applies BLQ encoding, and (for pcVPC) prediction correction.
#'
#' BLQ encoding rules:
#' \itemize{
#'   \item OBSDV (both modes): positions where `MDV == 1`, `is.na(OBSDV)`, or
#'     (when `loq` is provided) `OBSDV < loq` are encoded as `-Inf` via
#'     [`var_loqcens()`].
#'   \item SIMDV (pcVPC only, when `loq` is provided): positions where
#'     `SIMDV < loq` are encoded as `-Inf` via [`var_loqcens()`].
#'   \item For drop-mode quantile semantics, encoded `-Inf` values are
#'     converted to `NA_real_` via [`var_infna()`] before downstream
#'     consumers ([`var_predcorr()`] in pcVPC; quantile aggregation in
#'     [`df_vpcstats()`]). For rank-mode, the `-Inf` encoding is preserved
#'     and ranks at the low end of [`stats::quantile()`].
#' }
#'
#' @param sim Simulated data from `df_mrgsim_replicate()` or equivalent.
#' @param time_var_str String name of the actual time column in `sim`.
#' @param ntime_var_str String name of the nominal time column in `sim`.
#' @param pred_var_str String name of the population prediction column in `sim`.
#' @param ipred_var_str String name of the individual prediction column in `sim`.
#' @param sim_dv_var_str String name of the simulated DV column in `sim`.
#' @param obs_dv_var_str String name of the observed DV column in `sim`.
#' @param strat_var_str String or `NULL`. Stratification variable name.
#' @param pcvpc Logical for prediction correction.
#' @param loq Numeric value of the lower limit of quantification, or `NULL`.
#' @param mode One of `"auto"` (default), `"rank"`, or `"drop"`. Selects how
#'    BLQ-encoded values are carried into downstream quantile aggregation.
#'    `"rank"` preserves `-Inf` encoding (BLQ ranks at the low end of the
#'    sorted vector — fully-censored bins return `-Inf` quantiles, masked to
#'    `NA` before plotting). `"drop"` converts `-Inf` to `NA` so BLQ rows are
#'    excluded from quantile computation entirely. `"auto"` resolves to
#'    `"rank"` for std VPC and `"drop"` for pcVPC, matching historical
#'    pmxhelpr behavior.
#' @inheritParams var_predcorr
#' @return A preprocessed data.frame with standardized column names. OBSDV
#'    (and SIMDV in pcVPC + `loq`) carries `-Inf` for BLQ rows in `"rank"`
#'    mode and `NA_real_` in `"drop"` mode.
#' @keywords internal

df_vpcpreprocess <- function(sim, time_var_str, ntime_var_str,
                             pred_var_str, ipred_var_str,
                             sim_dv_var_str, obs_dv_var_str,
                             strat_var_str, pcvpc, lower_bound, loq,
                             mode = c("auto", "rank", "drop")) {

  mode <- match.arg(mode)
  if (mode == "auto") mode <- if (isTRUE(pcvpc)) "drop" else "rank"

  check_df(sim, "sim")
  check_varsindf(sim, time_var_str, "sim", "time_var")
  check_varsindf(sim, ntime_var_str, "sim", "ntime_var")
  if (isTRUE(pcvpc)) check_varsindf(sim, pred_var_str, "sim", "pred_var")
  check_varsindf(sim, sim_dv_var_str, "sim", "sim_dv_var")
  check_varsindf(sim, obs_dv_var_str, "sim", "obs_dv_var")
  check_varsindf(sim, "MDV", "sim", "MDV")
  check_varsindf(sim, "EVID", "sim", "EVID")
  if (!is.null(strat_var_str)) check_varsindf(sim, strat_var_str, "sim", "strat_var")
  if (!is.null(strat_var_str)) check_factor(sim, strat_var_str, "strat_var")

  sim <- df_prep_timevars(sim, time_var_str, ntime_var_str)
  sim <- dplyr::rename(sim, dplyr::any_of(c(PRED = pred_var_str, IPRED = ipred_var_str,
                                             SIMDV = sim_dv_var_str, OBSDV = obs_dv_var_str)))
  sim <- dplyr::rename(sim, BIN_MID = NTIME)

  sim <- dplyr::filter(sim, EVID == 0)

  ## Encode OBSDV BLQ rows as -Inf in both modes
  sim$OBSDV <- var_loqcens(sim$OBSDV, loq = loq, mdv = sim$MDV)

  if (isTRUE(pcvpc)) {
    ## Encode SIMDV BLQ rows as -Inf when loq is provided
    if (!is.null(loq)) sim$SIMDV <- var_loqcens(sim$SIMDV, loq = loq)

    if (mode == "drop") {
      ## Convert -Inf to NA so PC excludes BLQ from median(PRED) via na.rm and
      ## NA propagates through corrected values; quantile drops them. In
      ## "rank" mode, -Inf passes through `var_predcorr` (the formula yields
      ## -Inf when y = -Inf and PRED is finite) and ranks low at quantile time.
      sim$OBSDV <- var_infna(sim$OBSDV)
      sim$SIMDV <- var_infna(sim$SIMDV)
    }

    pc_group_vars <- c("BIN_MID", strat_var_str)
    if ("CMT" %in% colnames(sim)) pc_group_vars <- c("BIN_MID", "CMT", strat_var_str)
    sim <- sim |>
      dplyr::group_by(dplyr::across(dplyr::all_of(pc_group_vars))) |>
      dplyr::mutate(OBSDV = var_predcorr(OBSDV, PRED, lower_bound),
                    SIMDV = var_predcorr(SIMDV, PRED, lower_bound)) |>
      dplyr::ungroup()
  } else if (mode == "drop") {
    ## Std VPC drop mode: convert OBSDV -Inf to NA so BLQ rows are dropped
    ## from quantile via na.rm. SIMDV is uncensored in std VPC, no change.
    sim$OBSDV <- var_infna(sim$OBSDV)
  }

  sim
}
