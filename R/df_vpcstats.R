#' Compute VPC summary statistics
#'
#' @description
#' `df_vpcstats()` computes two-stage quantile summary statistics for
#'    visual predictive check (VPC) plots from simulated and observed data.
#'
#' @param sim Simulated data. Must contain `NTIME` and a column matching `irep_name`.
#' @param obs Observed data. Must contain `NTIME`.
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles. Default is `c(0.05, 0.95)`.
#' @param ci Numeric vector of length 2 specifying confidence interval quantiles. Default is `c(0.05, 0.95)`.
#' @param strat_var Character string of stratification variable name, or `NULL`.
#' @param sim_dv_var Character string of the simulated dependent variable column in `sim`. Default is `"SIMDV"`.
#' @param obs_dv_var Character string of the observed dependent variable column in `obs`. Default is `"OBSDV"`.
#' @param irep_name Character string of the replicate identifier column in `sim`. Default is `"SIM"`.
#' @param lloq Numeric value of the lower limit of quantification, or `NULL`.
#'
#' @return A list with two data.frames:
#'   \describe{
#'     \item{sim_quant}{Simulated quantile summary with columns: `NTIME`,
#'       `q5_low`, `q5_med`, `q5_hi`, `q50_low`, `q50_med`, `q50_hi`,
#'       `q95_low`, `q95_med`, `q95_hi`, and any stratification variable.}
#'     \item{obs_quant}{Observed quantile summary with columns: `NTIME`,
#'       `obs5`, `obs50`, `obs95`, and any stratification variable.}
#'   }
#' @keywords internal

df_vpcstats <- function(sim,
                        obs,
                        pi = c(0.05, 0.95),
                        ci = c(0.05, 0.95),
                        strat_var = NULL,
                        sim_dv_var = "SIMDV",
                        obs_dv_var = "OBSDV",
                        irep_name = "SIM",
                        lloq = NULL) {

  group_vars <- c("NTIME", strat_var)

  ## Stage 1: Per-replicate quantiles on simulated data
  stage1 <- sim |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_vars, irep_name)))) |>
    dplyr::summarise(
      q_lo  = stats::quantile(.data[[sim_dv_var]], probs = pi[1], na.rm = TRUE),
      q50   = stats::quantile(.data[[sim_dv_var]], probs = 0.5,   na.rm = TRUE),
      q_hi  = stats::quantile(.data[[sim_dv_var]], probs = pi[2], na.rm = TRUE),
      .groups = "drop"
    )

  ## Stage 2: CIs across replicates
  sim_quant <- stage1 |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
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

  ## Observed quantiles
  if (is.null(lloq)) {
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
        obs5  = quantile_lloq(.data[[obs_dv_var]], p = pi[1], lloq = lloq),
        obs50 = quantile_lloq(.data[[obs_dv_var]], p = 0.5,   lloq = lloq),
        obs95 = quantile_lloq(.data[[obs_dv_var]], p = pi[2], lloq = lloq),
        .groups = "drop"
      )
  }

  list(sim_quant = sim_quant, obs_quant = obs_quant)
}


#' Compute quantile with left-censored (LLOQ) handling
#'
#' @description
#' Replaces values below `lloq` (including `NA`) with `-Inf`, then computes
#' the quantile. Returns `NA` if the result is `-Inf`.
#'
#' @param x Numeric vector.
#' @param p Probability for quantile.
#' @param lloq Numeric lower limit of quantification.
#'
#' @return A single numeric value, or `NA`.
#' @keywords internal

quantile_lloq <- function(x, p, lloq) {
  x[is.na(x)] <- -Inf
  x[x < lloq] <- -Inf
  q <- stats::quantile(x, probs = p, na.rm = TRUE)
  if (is.infinite(q) && q < 0) NA_real_ else as.numeric(q)
}
