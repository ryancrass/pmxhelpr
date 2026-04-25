#' Compute VPC summary statistics
#'
#' @description
#' `df_vpcstats()` computes two-stage quantile summary statistics for
#'    visual predictive check (VPC) plots from simulated and observed data.
#'
#' @param sim Simulated data. Must contain columns matching: `bin_var` and `irep_name`.
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles. Default is `c(0.05, 0.95)`.
#' @param ci Numeric vector of length 2 specifying confidence interval quantiles. Default is `c(0.05, 0.95)`.
#' @param bin_var Binning variable name. Default is `NTIME`. Accepts bare names or strings.
#' @param strat_var Stratification variable name, or `NULL`. Accepts bare names or strings.
#' @param sim_dv_var Simulated dependent variable column in `sim`. Default is `SIMDV`. Accepts bare names or strings.
#' @param obs_dv_var Observed dependent variable column in `obs`. Default is `OBSDV`. Accepts bare names or strings.
#' @param irep_name Replicate identifier column in `sim`. Default is `SIM`. Accepts bare names or strings.
#' @param loq Numeric value of the lower limit of quantification, or `NULL`.
#'
#' @return A data.frame with columns: `bin_var`, `nbin`,
#'   `q5_low`, `q5_med`, `q5_hi`, `q50_low`, `q50_med`, `q50_hi`,
#'   `q95_low`, `q95_med`, `q95_hi`, `obs5`, `obs50`, `obs95`,
#'   and any stratification variable. Column names use the "5/50/95"
#'   convention regardless of actual `pi` values.
#'
#' @export df_vpcstats

df_vpcstats <- function(sim,
                        pi = c(0.05, 0.95),
                        ci = c(0.05, 0.95),
                        bin_var = NTIME,
                        strat_var = NULL,
                        sim_dv_var = SIMDV,
                        obs_dv_var = OBSDV,
                        irep_name = SIM,
                        loq = NULL) {

  strat_var_str   <- capture_col(rlang::enquo(strat_var))
  bin_var_str  <- rlang::as_name(rlang::ensym(bin_var))
  sim_dv_var  <- rlang::as_name(rlang::ensym(sim_dv_var))
  obs_dv_var  <- rlang::as_name(rlang::ensym(obs_dv_var))
  irep_name   <- rlang::as_name(rlang::ensym(irep_name))

  group_vars <- c(bin_var_str, strat_var_str)

  ## Stage 1: Per-replicate quantiles on simulated data
  stage1 <- sim |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, irep_name)))) |>
    dplyr::summarise(
      nbin = dplyr::n(),
      q_lo  = stats::quantile(.data[[sim_dv_var]], probs = pi[1], na.rm = TRUE),
      q50   = stats::quantile(.data[[sim_dv_var]], probs = 0.5,   na.rm = TRUE),
      q_hi  = stats::quantile(.data[[sim_dv_var]], probs = pi[2], na.rm = TRUE),
      .groups = "drop"
    )

  ## Stage 2: CIs across replicates
  sim_quant <- stage1 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      nbin = dplyr::first(nbin),
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

  ## Define observed by filtering to first replicate
  obs <- sim |>
    dplyr::filter(.data[[irep_name]] == 1)

  ## Observed quantiles
  if (is.null(loq)) {
    obs_quant <- obs |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        obs5  = stats::quantile(.data[[obs_dv_var]], probs = pi[1], na.rm = TRUE),
        obs50 = stats::quantile(.data[[obs_dv_var]], probs = 0.5,   na.rm = TRUE),
        obs95 = stats::quantile(.data[[obs_dv_var]], probs = pi[2], na.rm = TRUE),
        .groups = "drop"
      )
  } else {
    obs_quant <- obs |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        obs5  = quantile_lloq(.data[[obs_dv_var]], p = pi[1], loq = loq),
        obs50 = quantile_lloq(.data[[obs_dv_var]], p = 0.5,   loq = loq),
        obs95 = quantile_lloq(.data[[obs_dv_var]], p = pi[2], loq = loq),
        .groups = "drop"
      )
  }

  dplyr::left_join(sim_quant, obs_quant)
}


#' Compute quantile with left-censored (LLOQ) handling
#'
#' @description
#' Replaces values below `loq` (including `NA`) with `-Inf`, then computes
#' the quantile. Returns `NA` if the result is `-Inf`.
#'
#' @param x Numeric vector.
#' @param p Probability for quantile.
#' @param loq Numeric lower limit of quantification.
#'
#' @return A single numeric value, or `NA`.
#' @keywords internal

quantile_lloq <- function(x, p, loq) {
  x[is.na(x)] <- -Inf
  x[x < loq] <- -Inf
  q <- stats::quantile(x, probs = p, na.rm = TRUE)
  if (is.infinite(q) && q < 0) NA_real_ else as.numeric(q)
}
