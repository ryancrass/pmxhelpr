#' Plot a visual predictive check (VPC) for continuous data with exact time bins
#'
#' @description  `plot_vpc_cont()` generates a VPC plot using exact time bins
#' and returns a `ggplot2` object. Thin wrapper that delegates to
#' [df_vpcstats()] for computation and an internal builder for plot
#' construction.
#'
#' @param data Input dataset. `
#' @param strat_var Stratification variable.
#'    Accepts bare names or strings. Currently, only a single stratifying variable is supported.
#' @param pcvpc logical for prediction correction. Default is `FALSE`.
#' @param loq Numeric scalar, or `NULL`. Lower limit of quantification (LLOQ).
#'    When `NULL` and column `LLOQ` is present in `data`, per-row `LLOQ`
#'    values are used as the censoring threshold; a scalar `loq` broadcasts
#'    to a constant threshold across rows. For standard VPCs (`pcvpc = FALSE`)
#'
#'      + If a LOQ source is available (scalar `loq` or `LLOQ` column), all
#'      observations (including BLQ) are processed and censoring is performed
#'      at the quantile level. Filter to `EVID==0` so that doses are dropped.
#'      + If `loq=NULL` and `LLOQ` is NOT present in `data`, the dataset is
#'      filtered to `MDV==0` since `loq` is unknown.
#'
#'    For prediction-corrected VPCs (`pcvpc = TRUE`)
#'      + If a LOQ source is available, all `SIMDV` and `OBSDV` values
#'      `< LOQ` (per row) are set to missing (`NA_real_`) so that both
#'      observed and simulated data are censored in the same way before
#'      quantile calculation.
#'      + If `loq=NULL` and `LLOQ` is NOT present in `data`, filter to
#'      `MDV==0` since `loq` is unknown.
#'    Dashed horizontal line plotted at each unique LLOQ value by default for
#'    standard VPCs (controlled via `theme`); suppressed for `pcvpc = TRUE`
#'    since `loq` has no meaning on the prediction-corrected scale.
#' @param min_bin_count Minimum number of quantifiable observations
#'    (`obs_n - obs_n_blq` in the summary statistics frame) per exact bin
#'    required for inclusion in binned plot layers. BLQ-encoded records
#'    (`obs_n_blq`) do not count toward this threshold. This argument drops
#'    small bins from summary statistic plotting but retains the underlying
#'    observations as data points.
#' @param show_rep Display number of replicates as a plot caption. Default is `TRUE`.
#' @param mode One of `"auto"` (default), `"rank"`, or `"drop"`. Controls how
#'    BLQ-encoded values are carried through quantile aggregation. In `"rank"`
#'    mode, BLQ rows ranks low at `stats::quantile`; fully-censored quantiles
#'    return `-Inf` and are masked to `NA` before plotting. In `"drop"` mode,
#'    BLQ rows are excluded from quantile computation. `"auto"` resolves to
#'    `"rank"` for std VPC and `"drop"` for pcVPC, matching the package's
#'    historical behavior.
#'
#' @param shown Layer visibility settings created by [plot_vpc_shown()].
#'    Defaults can be viewed by running `plot_vpc_shown()` with no arguments.
#'
#' @param style A [ggstylekit::style_spec()] controlling plot aesthetics.
#'    Defaults to [style_vpc()]; view the defaults by running `style_vpc()`
#'    with no arguments.
#'
#' @param pi Numeric vector of length 2 specifying prediction interval quantiles. Default is `c(0.05, 0.95)`.
#' @param ci Numeric scalar in `(0, 1)` for simulation interval (e.g., `0.90` for 90% CI). Default is `0.90`.
#'
#' @param time_var Column containing the actual time variable in `data`.
#'    Accepts bare names or strings. Default is `TIME`.
#' @param ntime_var Column containing the nominal time variable in `data`.
#'    Accepts bare names or strings. Default is `NTIME`.
#' @param pred_var Column containing population predictions in `data`.
#'    Accepts bare names or strings. Default is `PRED`.
#' @param sim_dv_var Column containing simulated DV in `data`.
#'    Accepts bare names or strings. Default is `SIMDV`.
#' @param obs_dv_var Column containing observed DV in `data`.
#'    Accepts bare names or strings. Default is `OBSDV`.
#' @param irep_name Name of replicate variable in `data`. Accepts bare names or strings. Default is `SIM`.
#' @inheritParams plot_dvtime
#' @inheritParams var_predcorr
#'
#' @family vpc
#' @return A `pmx_vpc_plot` object (a `ggplot2` subclass). To access the
#'    underlying VPC summary statistics data.frame directly, use [df_vpcstats()].
#'    Adding `facet_wrap()` or `facet_grid()` to the result directly will warn,
#'    as stratification must be specified via `strat_var` at call time.
#' @export plot_vpc_cont
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1,2))
#' simout <- df_mrgsim_replicate(data = data_sad_pk, model = model, replicates = 100,
#' dv_var = ODV,
#' carry_out = c("LLOQ", "WTBL", "FOOD"),
#' recover  = c("USUBJID", "PART"),
#' irep_name = SIM)
#'
#' vpc_plot <- plot_vpc_cont(
#' data = simout,
#' pcvpc = TRUE,
#' loq = 1,
#' pi = c(0.05, 0.95),
#' ci = 0.90)

plot_vpc_cont <- function(data,
                          time_var = "TIME",
                          ntime_var = "NTIME",
                          pred_var = "PRED",
                          sim_dv_var = "SIMDV",
                          obs_dv_var = "OBSDV",
                          strat_var = NULL,
                          pcvpc = FALSE,
                          loq = NULL,
                          irep_name = "SIM",
                          min_bin_count = 1,
                          show_rep = TRUE,
                          lower_bound = 0,
                          mode = c("auto", "rank", "drop"),
                          shown = NULL,
                          style = NULL,
                          pi = c(0.05, 0.95),
                          ci = 0.90) {

  ## Precomputed-stats path: caller passed the container returned by
  ## df_vpcstats(). Skip preprocess + compute and recover plotting context
  ## from the container's $config slot. Pipeline args (strat_var, loq, mode,
  ## pi, ci, column-name args) cannot be honored on this path because the
  ## pipeline doesn't run again; the wrapper aborts if any are passed.
  ## Plot-only args (min_bin_count, show_rep, shown, style, pcvpc) are
  ## accepted on both paths.
  if (inherits(data, "vpc_stats")) {
    check_pipeline_args_dropped(
      call           = match.call(),
      plot_only_args = c("data", "min_bin_count", "show_rep", "shown",
                         "style", "pcvpc"),
      fn_name        = "plot_vpc_cont"
    )
    return(plot_build_vpc(
      data,
      min_bin_count = min_bin_count,
      show_rep      = show_rep,
      shown         = shown,
      style         = style,
      pcvpc         = pcvpc,
      bin_var       = BIN_MID_VAR
    ))
  }

  time_var_str   <- resolve_var(rlang::enquo(time_var))
  ntime_var_str  <- resolve_var(rlang::enquo(ntime_var))
  pred_var_str   <- resolve_var(rlang::enquo(pred_var))
  sim_dv_var_str <- resolve_var(rlang::enquo(sim_dv_var))
  obs_dv_var_str <- resolve_var(rlang::enquo(obs_dv_var))
  strat_var_str  <- resolve_var(rlang::enquo(strat_var), nullable = TRUE)
  irep_name_str  <- resolve_var(rlang::enquo(irep_name))

  out <- df_vpcstats(
    data = data,
    time_var   = time_var_str,
    ntime_var  = ntime_var_str,
    pred_var   = pred_var_str,
    sim_dv_var = sim_dv_var_str,
    obs_dv_var = obs_dv_var_str,
    strat_var  = strat_var_str,
    irep_name  = irep_name_str,
    loq = loq, lower_bound = lower_bound,
    mode = mode, pi = pi, ci = ci
  )

  plot_build_vpc(
    out,
    min_bin_count = min_bin_count,
    show_rep = show_rep,
    shown = shown,
    style = style,
    pcvpc = pcvpc,
    bin_var = BIN_MID_VAR
  )
}
