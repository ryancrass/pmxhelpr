#' Plot a censoring (BLQ-proportion) VPC
#'
#' @description `plot_vpc_cens()` generates a VPC plot of the proportion of
#'    below-limit-of-quantification (BLQ) observations over time and returns
#'    a `ggplot2` object. Mirrors [plot_vpc_cont()] structurally but plots
#'    the BLQ proportion rather than concentration quantiles: the simulated
#'    non-parametric confidence band is the empirical distribution of
#'    per-replicate BLQ proportions across `ci` bounds, and the observed
#'    proportion is a single point/line per bin.
#'
#'    Thin wrapper that delegates to [df_vpcstats()] for computation and to
#'    [plot_build_vpc()] (`type = "cens"`) for plot construction. The
#'    `sim_prop_blq_*` columns produced by [df_vpcstats()] when a LOQ
#'    source is available are the sole source of plot data.
#'
#' @param data Input dataset. Simulated replicate data (typically from
#'    [df_mrgsim_replicate()]) **or** a precomputed `vpc_stats` container
#'    (output of [df_vpcstats()]). When a precomputed container is passed,
#'    the pipeline does not re-run and pipeline arguments are rejected; pass
#'    raw data when you need pipeline control.
#' @param strat_var Stratification variable. Accepts bare names or strings.
#'    Currently, only a single stratifying variable is supported.
#'    Facet layout is taken from the `style`: `facet_scales`, `facet_nrow`,
#'    and `facet_ncol`.
#' @param loq Numeric scalar, or `NULL`. Lower limit of quantification (LLOQ).
#'    Either `loq` or an `LLOQ` column in `data` is **required** — without a
#'    LOQ source, `df_vpcstats()` does not emit the `sim_prop_blq_*` columns
#'    a cens VPC needs. When `NULL` and column `LLOQ` is present in `data`,
#'    per-row `LLOQ` values are used as the censoring threshold; a scalar
#'    `loq` broadcasts to a constant threshold across rows.
#' @param min_bin_count Minimum number of observations per bin
#'    required for inclusion in binned plot layers. Applied to total obs
#'    (`obs_n` in the summary statistics frame); BLQ-encoded records *do*
#'    count toward this threshold, in contrast to [plot_vpc_cont()] which
#'    requires quantifiable obs. Rationale: BLQ-heavy bins are the most
#'    informative on a cens VPC and should be retained.
#' @param show_rep Display number of replicates as a plot caption. Default is `TRUE`.
#'    A `caption` set in the style is drawn instead, whatever this value.
#'
#' @param shown Layer visibility settings created by [plot_vpc_shown()]. The
#'    cens builder reads four of the keys: `obs_point`, `obs_median_line`,
#'    `sim_median_line`, `sim_median_ci`. Defaults follow `plot_vpc_shown()`:
#'    the observed proportion line/points and simulated CI ribbon are shown;
#'    the simulated median line is off by default. Pass
#'    `plot_vpc_shown(sim_median_line = TRUE)` to enable it.
#'
#' @param style A [ggstylekit::style_spec()] controlling plot aesthetics.
#'    Defaults to [style_vpc()]; view the defaults by running `style_vpc()`
#'    with no arguments. Customize by passing `style = style_vpc(...)`.
#'    Pass the same style to
#'    [plot_vpc_legend()] so the legend matches. The cens builder reads the same four keys
#'    listed above (`obs_point`, `obs_median_line`, `sim_median_line`,
#'    `sim_median_ci`); other keys are ignored. The style's `logy` is
#'    ignored, with a warning.
#'
#' @param ci Numeric scalar in `(0, 1)` for the simulated CI bound on the BLQ
#'    proportion across replicates (e.g., `0.90` for 90% CI). Default is
#'    `0.90`. Honored only on the raw-data path; passing `ci` explicitly
#'    when `data` is a precomputed `vpc_stats` container raises an error.
#'    The container's stored `ci` (set when `df_vpcstats()` was called) is
#'    the source of truth on that path.
#'
#' @param time_var Column containing the actual time variable in `data`.
#'    Accepts bare names or strings. Default is `TIME`.
#' @param ntime_var Column containing the nominal time variable in `data`.
#'    Accepts bare names or strings. Default is `NTIME`.
#' @param pred_var Column containing population predictions in `data`.
#'    Accepts bare names or strings. Default is `PRED`. Required by
#'    [df_vpcpreprocess()] even though `PRED` does not appear in cens output.
#' @param sim_dv_var Column containing simulated DV in `data`.
#'    Accepts bare names or strings. Default is `SIMDV`.
#' @param obs_dv_var Column containing observed DV in `data`.
#'    Accepts bare names or strings. Default is `OBSDV`.
#' @param rep_var Name of replicate variable in `data`. Accepts bare names
#'    or strings. Default is `SIM`.
#'
#' @param irep_name `r lifecycle::badge("deprecated")` Use `rep_var`.
#' @inheritParams plot_dvtime
#' @inheritParams df_vpcstats
#' @inheritParams plot_build_vpc
#' @family vpc
#' @return A `pmx_vpc_plot` object (a `ggplot2` subclass). To access the
#'    underlying VPC summary statistics directly, use [df_vpcstats()] and
#'    inspect the `obs_prop_blq` and `sim_prop_blq_*` columns of the
#'    `stats` data.frame.
#' @export plot_vpc_cens
#'
#' @examples
#' model <- model_mread_load(model = "pkmodel")
#' data_sad_pk <- dplyr::filter(data_sad, CMT %in% c(1, 2))
#' # 20 replicates keeps the example fast; use >= 100 for a real VPC
#' simout <- df_mrgsim_replicate(data = data_sad_pk, model = model,
#'                               replicates = 20, dv_var = ODV,
#'                               carry_out = c("LLOQ", "WTBL", "FOOD"),
#'                               recover = c("USUBJID", "PART"),
#'                               rep_var = SIM)
#'
#' cens_plot <- plot_vpc_cens(data = simout, loq = 1, ci = 0.90)

plot_vpc_cens <- function(data,
                          time_var = "TIME",
                          ntime_var = "NTIME",
                          pred_var = "PRED",
                          sim_dv_var = "SIMDV",
                          obs_dv_var = "OBSDV",
                          strat_var = NULL,
                          loq = NULL,
                          rep_var = "SIM",
                          min_bin_count = 0,
                          show_rep = TRUE,
                          shown = NULL,
                          style = NULL,
                          time_unit = "hours",
                          bin_style = c("line", "rect"),
                          ci = 0.90,
                          bin_breaks = NULL,
                          irep_name) {

  check_boolean(show_rep, "show_rep")
  time_unit <- normalize_time_unit(time_unit)
  bin_style <- match.arg(bin_style)

  ## `data` is either the container returned by df_vpcstats() or raw
  ## simulated data. With a container the pipeline does not run again, so
  ## pipeline args (strat_var, loq, ci, column-name args) cannot be honored
  ## and the wrapper aborts if any are passed. plot_build_vpc(type = "cens")
  ## validates that the container carries the cens columns.
  stats <- data
  if (inherits(data, "vpc_stats")) {
    check_pipeline_args_dropped(
      mc             = match.call(),
      plot_only_args = c("data", "min_bin_count", "show_rep", "shown", "style",
                         "time_unit", "bin_style"),
      fn_name        = "plot_vpc_cens",
      stats_fn       = "df_vpcstats"
    )
  } else {
    data_cols <- names(data)
    time_var_str   <- resolve_var(rlang::enquo(time_var), cols = data_cols)
    ntime_var_str  <- resolve_var(rlang::enquo(ntime_var), cols = data_cols)
    pred_var_str   <- resolve_var(rlang::enquo(pred_var), cols = data_cols)
    sim_dv_var_str <- resolve_var(rlang::enquo(sim_dv_var), cols = data_cols)
    obs_dv_var_str <- resolve_var(rlang::enquo(obs_dv_var), cols = data_cols)
    strat_var_str  <- resolve_var(rlang::enquo(strat_var), nullable = TRUE, cols = data_cols)
    rep_var_q <- rlang::enquo(rep_var)
    if (use_deprecated_arg(!missing(irep_name), !missing(rep_var), "plot_vpc_cens",
                           "irep_name", "rep_var")) {
      rep_var_q <- rlang::enquo(irep_name)
    }
    rep_var_str    <- resolve_var(rep_var_q, cols = data_cols)

    stats <- df_vpcstats(
      data       = data,
      time_var   = time_var_str,
      ntime_var  = ntime_var_str,
      pred_var   = pred_var_str,
      sim_dv_var = sim_dv_var_str,
      obs_dv_var = obs_dv_var_str,
      strat_var  = strat_var_str,
      rep_var    = rep_var_str,
      loq        = loq,
      ci         = ci,
      bin_breaks = bin_breaks
    )
  }

  plot_build_vpc(
    stats,
    type          = "cens",
    min_bin_count = min_bin_count,
    show_rep      = show_rep,
    shown         = shown,
    style         = style,
    time_unit     = time_unit,
    bin_style     = bin_style
  )
}
