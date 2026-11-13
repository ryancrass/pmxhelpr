#' Plot a visual predictive check (VPC) for continuous data with exact time bins
#'
#' @description  `plot_vpc_cont()` generates a VPC plot using exact time bins
#' and returns a `ggplot2` object. Thin wrapper that delegates to
#' [df_vpcstats()] for computation and an internal builder for plot
#' construction.
#'
#' @param data Simulated replicate data (e.g. from [df_mrgsim_replicate()]), or
#'    a `vpc_stats` object from [df_vpcstats()].
#' @param strat_var Stratification variable.
#'    Accepts bare names or strings. Currently, only a single stratifying variable is supported.
#'    Facet layout is taken from the `style`: `facet_scales`, `facet_nrow`,
#'    and `facet_ncol`.
#' @param pcvpc logical for prediction correction. Default is `FALSE`. Not
#'    available when `PRED` differs by replicate (simulations with parameter
#'    uncertainty, see the `params` argument of [df_mrgsim_replicate()]).
#' @param loq Numeric LLOQ, or `NULL` (default) to use a per-row `LLOQ`
#'    column when `data` has one. Observations with `MDV == 1`, missing, or
#'    below the LOQ are treated as BLQ; they are not dropped, even with no LOQ
#'    source.
#'      + Standard VPC (`pcvpc = FALSE`): BLQ observations rank below all
#'      quantifiable values in the observed quantiles; simulated values are
#'      not censored.
#'      + pcVPC (`pcvpc = TRUE`): observed and simulated BLQ values are set to
#'      missing before prediction correction, so both are censored alike.
#'
#'    The LLOQ line (`loq_line` role) is drawn once per panel, at that panel's
#'    highest LLOQ when there are several, on standard
#'    VPCs only, since the LLOQ has no meaning on the prediction-corrected
#'    scale; hide it with `shown = plot_vpc_shown(loq_line = FALSE)`. The
#'    observed points show `MDV == 0` rows only.
#' @param min_bin_count Minimum number of quantifiable observations
#'    (`obs_n - obs_n_blq` in the summary statistics frame) per exact bin
#'    required for inclusion in binned plot layers. BLQ-encoded records
#'    (`obs_n_blq`) do not count toward this threshold. This argument drops
#'    small bins from summary statistic plotting but retains the underlying
#'    observations as data points.
#' @param show_rep Display number of replicates as a plot caption. Default is `TRUE`.
#'    A `caption` set in the style is drawn instead, whatever this value.
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
#'    with no arguments. Customize by passing `style = style_vpc(...)`.
#'    Pass the same style to
#'    [plot_vpc_legend()] so the legend matches.
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
#' @param rep_var Name of replicate variable in `data`. Accepts bare names or strings. Default is `SIM`.
#' @param irep_name `r lifecycle::badge("deprecated")` Use `rep_var`.
#' @inheritParams plot_dvtime
#' @inheritParams df_vpcstats
#' @inheritParams plot_build_vpc
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
#' # 20 replicates keeps the example fast; use >= 100 for a real VPC
#' simout <- df_mrgsim_replicate(data = data_sad_pk, model = model, replicates = 20,
#' dv_var = ODV,
#' carry_out = c("LLOQ", "WTBL", "FOOD"),
#' recover  = c("USUBJID", "PART"),
#' rep_var = SIM)
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
                          rep_var = "SIM",
                          min_bin_count = 0,
                          show_rep = TRUE,
                          lower_bound = 0,
                          mode = c("auto", "rank", "drop"),
                          shown = NULL,
                          style = NULL,
                          time_unit = "hours",
                          bin_style = c("line", "rect"),
                          pi = c(0.05, 0.95),
                          ci = 0.90,
                          bin_breaks = NULL,
                          irep_name) {

  check_boolean(show_rep, "show_rep")
  check_boolean(pcvpc, "pcvpc")
  time_unit <- normalize_time_unit(time_unit)
  bin_style <- match.arg(bin_style)

  ## Precomputed-stats path: caller passed the container returned by
  ## df_vpcstats(). Skip preprocess + compute and recover plotting context
  ## from the container's $config slot. Pipeline args (strat_var, loq, mode,
  ## pi, ci, column-name args) cannot be honored on this path because the
  ## pipeline doesn't run again; the wrapper aborts if any are passed.
  ## Plot-only args (min_bin_count, show_rep, shown, style, pcvpc, time_unit,
  ## bin_style) are
  ## accepted on both paths.
  if (inherits(data, "vpc_stats")) {
    check_pipeline_args_dropped(
      mc             = match.call(),
      plot_only_args = c("data", "min_bin_count", "show_rep", "shown",
                         "style", "pcvpc", "time_unit", "bin_style"),
      fn_name        = "plot_vpc_cont",
      stats_fn       = "df_vpcstats"
    )
    return(plot_build_vpc(
      data,
      min_bin_count = min_bin_count,
      show_rep      = show_rep,
      shown         = shown,
      style         = style,
      pcvpc         = pcvpc,
      time_unit     = time_unit,
      bin_style     = bin_style
    ))
  }

  data_cols <- names(data)
  time_var_str   <- resolve_var(rlang::enquo(time_var), cols = data_cols)
  ntime_var_str  <- resolve_var(rlang::enquo(ntime_var), cols = data_cols)
  pred_var_str   <- resolve_var(rlang::enquo(pred_var), cols = data_cols)
  sim_dv_var_str <- resolve_var(rlang::enquo(sim_dv_var), cols = data_cols)
  obs_dv_var_str <- resolve_var(rlang::enquo(obs_dv_var), cols = data_cols)
  strat_var_str  <- resolve_var(rlang::enquo(strat_var), nullable = TRUE, cols = data_cols)
  rep_var_q <- rlang::enquo(rep_var)
  if (use_deprecated_arg(!missing(irep_name), !missing(rep_var), "plot_vpc_cont",
                         "irep_name", "rep_var")) {
    rep_var_q <- rlang::enquo(irep_name)
  }
  rep_var_str    <- resolve_var(rep_var_q, cols = data_cols)

  out <- df_vpcstats(
    data = data,
    time_var   = time_var_str,
    ntime_var  = ntime_var_str,
    pred_var   = pred_var_str,
    sim_dv_var = sim_dv_var_str,
    obs_dv_var = obs_dv_var_str,
    strat_var  = strat_var_str,
    rep_var    = rep_var_str,
    loq = loq, lower_bound = lower_bound,
    mode = mode, pi = pi, ci = ci,
    bin_breaks = bin_breaks
  )

  plot_build_vpc(
    out,
    min_bin_count = min_bin_count,
    show_rep = show_rep,
    shown = shown,
    style = style,
    pcvpc = pcvpc,
    time_unit = time_unit,
    bin_style = bin_style
  )
}
