#' Compute VPC summary statistics from raw simulation data
#'
#' @description
#' Returns the VPC summary statistics and observation overlay underlying a
#' continuous-data VPC plot. Takes raw simulation output (e.g., from
#' [df_mrgsim_replicate()]) and always emits **both** standard and
#' prediction-corrected statistics in a single call. Preprocessing — column
#' renames, dose-row filtering, BLQ encoding, prediction-correction — is
#' applied internally.
#'
#' For a plotted VPC, use [plot_vpc_cont()], which is a thin wrapper that
#' calls `df_vpcstats()` and forwards the result to the internal plot
#' builder. The standard vs. prediction-corrected view is selected at plot
#' time via `plot_vpc_cont(pcvpc = TRUE/FALSE)`.
#'
#' @param data Raw simulation output. Must contain `EVID`, `MDV`, `PRED`, and
#'    columns named by `time_var`, `ntime_var`, `sim_dv_var`, `obs_dv_var`,
#'    and `irep_name`.
#' @param time_var Column containing the actual time variable in `data`.
#'    Accepts bare names or strings. Default is `TIME`.
#' @param ntime_var Column containing the nominal time variable in `data`.
#'    Accepts bare names or strings. Default is `NTIME`.
#' @param pred_var Column containing population predictions in `data`.
#'    Accepts bare names or strings. Default is `PRED`. Required for the
#'    prediction-corrected statistics.
#' @param sim_dv_var Column containing simulated DV in `data`.
#'    Accepts bare names or strings. Default is `SIMDV`.
#' @param obs_dv_var Column containing observed DV in `data`.
#'    Accepts bare names or strings. Default is `OBSDV`.
#' @param irep_name Replicate identifier column. Accepts bare names or strings.
#'    Default is `SIM`.
#' @param strat_var Stratification variable. Accepts bare names or strings.
#'    Default is `NULL`. Only a single stratifying variable is supported.
#' @param loq Numeric scalar, or `NULL`. When `NULL` and column `LLOQ` is
#'    present in `data`, per-row `LLOQ` values are used as the censoring
#'    threshold; the unique non-NA values are exposed via `config$loq` for
#'    plotting. A scalar `loq` broadcasts to a constant threshold across rows.
#' @param mode One of `"auto"` (default), `"rank"`, or `"drop"`. See
#'    [plot_vpc_cont()] for semantics. `"auto"` resolves to `"rank"` for the
#'    standard flavor and `"drop"` for the prediction-corrected flavor.
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles.
#'    Default is `c(0.05, 0.95)`.
#' @param ci Numeric scalar in `(0, 1)` specifying the simulation interval
#'    (e.g., `0.90` for a 90% CI). Default is `0.90`.
#' @inheritParams var_predcorr
#'
#' @family vpc
#' @return A list with two data.frames (class `c("vpc_stats", "list")`):
#'    \describe{
#'      \item{`stats`}{Wide summary statistics. Standard-VPC columns are
#'        unprefixed (`obs_low/med/hi`, `sim_low_low/med/hi`, etc.);
#'        prediction-corrected counterparts carry a `pc_` prefix
#'        (`pc_obs_low`, `pc_sim_low_med`, etc.). `obs_n`, `obs_n_blq`,
#'        `obs_prop_blq`, `sim_prop_blq_*`, `ci`, `pi_low`, `pi_hi` are
#'        single (not duplicated). `sim_prop_blq_*` is std-only — LOQ has
#'        no meaning on the prediction-corrected scale. `-Inf` quantile
#'        values from rank-mode fully-censored bins are masked to `NA`.
#'        Carries attributes `n_replicates`, `loq`, and `strat_var`.}
#'      \item{`obs`}{First-replicate observation rows with `MDV == 0`, used as
#'        the scatter overlay in [plot_vpc_cont()]. `OBSDV` carries the
#'        std-scale value; `PC_OBSDV` carries the prediction-corrected
#'        counterpart.}
#'    }
#' @export df_vpcstats
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' simout <- df_mrgsim_replicate(data = data_sad, model = model, replicates = 10,
#'                               dv_var = ODV)
#' result <- df_vpcstats(simout, loq = 1)
#' head(result$stats)
#' head(result$obs)

df_vpcstats <- function(data,
                        time_var = TIME,
                        ntime_var = NTIME,
                        pred_var = PRED,
                        sim_dv_var = SIMDV,
                        obs_dv_var = OBSDV,
                        strat_var = NULL,
                        loq = NULL,
                        irep_name = SIM,
                        lower_bound = 0,
                        mode = c("auto", "rank", "drop"),
                        pi = c(0.05, 0.95),
                        ci = 0.90) {

  time_var_str   <- resolve_var(rlang::enquo(time_var))
  ntime_var_str  <- resolve_var(rlang::enquo(ntime_var))
  pred_var_str   <- resolve_var(rlang::enquo(pred_var))
  sim_dv_var_str <- resolve_var(rlang::enquo(sim_dv_var))
  obs_dv_var_str <- resolve_var(rlang::enquo(obs_dv_var))
  strat_var_str  <- resolve_var(rlang::enquo(strat_var), nullable = TRUE)
  irep_name_str  <- resolve_var(rlang::enquo(irep_name))

  pre <- df_vpcpreprocess(
    data,
    time_var_str   = time_var_str,
    ntime_var_str  = ntime_var_str,
    pred_var_str   = pred_var_str,
    sim_dv_var_str = sim_dv_var_str,
    obs_dv_var_str = obs_dv_var_str,
    strat_var_str  = strat_var_str,
    irep_name_str  = irep_name_str,
    loq = loq
  )

  df_vpccompute(
    pre,
    pi = pi, ci = ci,
    bin_var = BIN_MID_VAR,
    strat_var = strat_var_str,
    irep_name = irep_name_str,
    lower_bound = lower_bound,
    mode = mode
  )
}
