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
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles.
#'    Default is `c(0.05, 0.95)`.
#' @param ci Numeric vector of length 2 specifying confidence interval quantiles.
#'    Default is `c(0.05, 0.95)`.
#' @param bin_var String. Binning variable name. Default is `"BIN_MID"`.
#' @param strat_var_str String or `NULL`. Stratification variable name. Default is `NULL`.
#' @param irep_name_str String. Replicate identifier column name. Default is `"SIM"`.
#' @param loq Numeric value of the lower limit of quantification, or `NULL`.
#'    When specified, observed quantiles use censored quantile estimation.
#'
#' @return A `data.frame` with columns for simulated quantile CIs
#'    (`q5_low`, `q5_med`, `q5_hi`, `q50_low`, `q50_med`, `q50_hi`,
#'    `q95_low`, `q95_med`, `q95_hi`) and observed quantiles
#'    (`obs5`, `obs50`, `obs95`), joined by `bin_var`.
#' @export df_vpcstats

df_vpcstats <- function(sim, pi, ci, bin_var, strat_var_str, irep_name_str, loq) {

  check_quantile_pair(pi, "pi")
  check_quantile_pair(ci, "ci")

  group_vars <- c(bin_var)
  if (!is.null(strat_var_str)) group_vars <- c(bin_var, strat_var_str)

  ## Stage 1: Per-replicate quantiles on simulated data
  stage1 <- sim |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, irep_name_str)))) |>
    dplyr::summarise(
      nbin = dplyr::n(),
      q_lo = stats::quantile(.data[["SIMDV"]], probs = pi[1], na.rm = TRUE),
      q50  = stats::quantile(.data[["SIMDV"]], probs = 0.5,   na.rm = TRUE),
      q_hi = stats::quantile(.data[["SIMDV"]], probs = pi[2], na.rm = TRUE),
      .groups = "drop"
    )

  ## Stage 2: CIs across replicates
  sim_quant <- stage1 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      nbin    = dplyr::first(nbin),
      q5_low  = stats::quantile(q_lo, probs = ci[1], na.rm = TRUE),
      q5_med  = stats::quantile(q_lo, probs = 0.5,   na.rm = TRUE),
      q5_hi   = stats::quantile(q_lo, probs = ci[2], na.rm = TRUE),
      q50_low = stats::quantile(q50,  probs = ci[1], na.rm = TRUE),
      q50_med = stats::quantile(q50,  probs = 0.5,   na.rm = TRUE),
      q50_hi  = stats::quantile(q50,  probs = ci[2], na.rm = TRUE),
      q95_low = stats::quantile(q_hi, probs = ci[1], na.rm = TRUE),
      q95_med = stats::quantile(q_hi, probs = 0.5,   na.rm = TRUE),
      q95_hi  = stats::quantile(q_hi, probs = ci[2], na.rm = TRUE),
      .groups = "drop"
    )

  ## Observed quantiles from first replicate
  obs <- sim |>
    dplyr::filter(.data[[irep_name_str]] == 1)

  if (is.null(loq)) {
    obs_quant <- obs |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        obs5  = stats::quantile(.data[["OBSDV"]], probs = pi[1], na.rm = TRUE),
        obs50 = stats::quantile(.data[["OBSDV"]], probs = 0.5,   na.rm = TRUE),
        obs95 = stats::quantile(.data[["OBSDV"]], probs = pi[2], na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    obs_quant <- obs |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        obs5  = var_loqcens(.data[["OBSDV"]], p = pi[1], loq = loq),
        obs50 = var_loqcens(.data[["OBSDV"]], p = 0.5,   loq = loq),
        obs95 = var_loqcens(.data[["OBSDV"]], p = pi[2], loq = loq),
        .groups = "drop"
      )
  }

  dplyr::left_join(sim_quant, obs_quant, by = group_vars)
}


#' Preprocess simulation data for VPC statistics
#'
#' @description
#' Internal function. Validates columns, renames to standard names, and applies
#' prediction correction or BLQ handling.
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
#' @inheritParams var_predcorr
#' @return A preprocessed data.frame with standardized column names.
#' @keywords internal

df_vpcpreprocess <- function(sim, time_var_str, ntime_var_str,
                             pred_var_str, ipred_var_str,
                             sim_dv_var_str, obs_dv_var_str,
                             strat_var_str, pcvpc, lower_bound, loq) {

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

  if(is.null(loq)) {
    sim <- sim |>
      dplyr::filter(MDV == 0)
  }

  if (isTRUE(pcvpc)) {
    pc_group_vars <- c("BIN_MID", strat_var_str)
    if ("CMT" %in% colnames(sim)) pc_group_vars <- c("BIN_MID", "CMT", strat_var_str)
    if (!is.null(loq)) {
      sim <- sim |>
        dplyr::mutate(OBSDV = ifelse(OBSDV < loq, NA_real_, OBSDV),
                      SIMDV = ifelse(SIMDV < loq, NA_real_, SIMDV))
    }
    sim <- sim |>
      dplyr::filter(EVID == 0) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(pc_group_vars))) |>
      dplyr::mutate(OBSDV = var_predcorr(OBSDV, PRED, lower_bound),
                    SIMDV = var_predcorr(SIMDV, PRED, lower_bound)) |>
      dplyr::ungroup()
  } else {
    sim <- sim |>
      dplyr::mutate(MDV = 0L) |>
      dplyr::filter(EVID == 0)
  }

  sim
}
