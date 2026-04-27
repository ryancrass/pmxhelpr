#' Compute VPC summary statistics from preprocessed simulation data
#'
#' @description
#' Internal function. Computes two-stage quantile summary statistics for VPC plots
#' from data already preprocessed by `df_vpcpreprocess()`.
#'
#' @param sim Preprocessed simulation data containing `SIMDV`, `OBSDV`, and `irep_name` columns.
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles.
#' @param ci Numeric vector of length 2 specifying confidence interval quantiles.
#' @param bin_var_str String. Binning variable name.
#' @param strat_var_str String or `NULL`. Stratification variable name.
#' @param irep_name_str String. Replicate identifier column name.
#' @param loq Numeric value of the lower limit of quantification, or `NULL`.
#'
#' @return A data.frame with quantile summary columns.
#' @keywords internal

df_vpcstats <- function(sim, pi, ci, bin_var_str, strat_var_str, irep_name_str, loq) {

  group_vars <- c(bin_var_str)
  if (!is.null(strat_var_str)) group_vars <- c(bin_var_str, strat_var_str)

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

  dplyr::left_join(sim_quant, obs_quant)
}


#' Preprocess simulation data for VPC statistics
#'
#' @description
#' Internal function. Validates columns, renames to standard names, and applies
#' prediction correction or BLQ handling.
#'
#' @param sim Simulated data from `df_mrgsim_replicate()` or equivalent.
#' @param time_vars Named character vector for time column mapping.
#' @param output_vars Named character vector for output column mapping.
#' @param strat_var_str String or `NULL`. Stratification variable name.
#' @param pcvpc Logical for prediction correction.
#' @param loq Numeric value of the lower limit of quantification, or `NULL`.
#' @inheritParams var_pc
#' @return A preprocessed data.frame with standardized column names.
#' @keywords internal

df_vpcpreprocess <- function(sim, time_vars, output_vars, strat_var_str,
                             pcvpc, lower_bound, loq) {
  time_vars <- init_time_vars(time_vars)
  output_vars <- list_update(output_vars, c(PRED = "PRED", IPRED = "IPRED",
                                            SIMDV = "SIMDV", OBSDV = "OBSDV"))

  check_df(sim, "sim")
  check_varsindf(sim, time_vars[["TIME"]], "sim", "time_vars")
  check_varsindf(sim, time_vars[["NTIME"]], "sim", "time_vars")
  if (isTRUE(pcvpc)) check_varsindf(sim, output_vars[["PRED"]], "sim", "output_vars")
  check_varsindf(sim, output_vars[["SIMDV"]], "sim", "output_vars")
  check_varsindf(sim, output_vars[["OBSDV"]], "sim", "output_vars")
  check_varsindf(sim, "MDV", "sim", "MDV")
  if (!is.null(strat_var_str)) check_varsindf(sim, strat_var_str, "sim", "strat_var")
  if (!is.null(strat_var_str)) check_factor(sim, strat_var_str, "strat_var")

  sim <- rename_time_vars(sim, time_vars, output_vars)

  if (isTRUE(pcvpc)) {
    sim <- sim |>
      dplyr::filter(MDV == 0) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("NTIME", "CMT", strat_var_str)))) |>
      dplyr::mutate(OBSDV = var_pc(OBSDV, PRED, lower_bound),
                    SIMDV = var_pc(SIMDV, PRED, lower_bound)) |>
      dplyr::ungroup()
  } else {
    sim <- sim |>
      dplyr::mutate(MDV = ifelse(!is.null(loq), 0, MDV))
  }

  sim
}
