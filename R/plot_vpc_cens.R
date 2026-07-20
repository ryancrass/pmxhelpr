#' Plot a censoring (BLQ-proportion) VPC with exact time bins
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
#' @param loq Numeric scalar, or `NULL`. Lower limit of quantification (LLOQ).
#'    Either `loq` or an `LLOQ` column in `data` is **required** — without a
#'    LOQ source, `df_vpcstats()` does not emit the `sim_prop_blq_*` columns
#'    a cens VPC needs. When `NULL` and column `LLOQ` is present in `data`,
#'    per-row `LLOQ` values are used as the censoring threshold; a scalar
#'    `loq` broadcasts to a constant threshold across rows.
#' @param min_bin_count Minimum number of observations per exact bin
#'    required for inclusion in binned plot layers. Applied to total obs
#'    (`obs_n` in the summary statistics frame); BLQ-encoded records *do*
#'    count toward this threshold, in contrast to [plot_vpc_cont()] which
#'    requires quantifiable obs. Rationale: BLQ-heavy bins are the most
#'    informative on a cens VPC and should be retained.
#' @param show_rep Display number of replicates as a plot caption. Default is `TRUE`.
#'
#' @param shown Layer visibility settings created by [plot_vpc_shown()]. The
#'    cens builder reads four of the keys: `obs_point`, `obs_median_line`,
#'    `sim_median_line`, `sim_median_ci`. Defaults follow `plot_vpc_shown()`:
#'    the observed proportion line/points and simulated CI ribbon are shown;
#'    the simulated median line is off by default. Pass
#'    `plot_vpc_shown(sim_median_line = TRUE)` to enable it.
#'
#' @param style A [ggstylekit::style_spec()] controlling plot aesthetics
#'    (defaults to [style_vpc()]). The cens builder reads the same four keys
#'    listed above (`obs_point`, `obs_median_line`, `sim_median_line`,
#'    `sim_median_ci`); other keys are ignored.
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
#' @param irep_name Name of replicate variable in `data`. Accepts bare names
#'    or strings. Default is `SIM`.
#'
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
#' simout <- df_mrgsim_replicate(data = data_sad_pk, model = model,
#'                               replicates = 100, dv_var = ODV,
#'                               carry_out = c("LLOQ", "WTBL", "FOOD"),
#'                               recover = c("USUBJID", "PART"),
#'                               irep_name = SIM)
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
                          irep_name = "SIM",
                          min_bin_count = 1,
                          show_rep = TRUE,
                          shown = NULL,
                          style = NULL,
                          ci = 0.90) {

  ## Precomputed-stats path: caller passed the container returned by
  ## df_vpcstats(). Skip preprocess + compute, validate that the container
  ## carries the cens columns, and delegate to plot_build_vpc(type = "cens").
  ## Pipeline args (strat_var, loq, ci, column-name args) cannot be honored
  ## here because the pipeline doesn't re-run.
  if (inherits(data, "vpc_stats")) {
    check_pipeline_args_dropped(
      call           = match.call(),
      plot_only_args = c("data", "min_bin_count", "show_rep", "shown", "style"),
      fn_name        = "plot_vpc_cens"
    )
    return(plot_build_vpc(
      data,
      type          = "cens",
      min_bin_count = min_bin_count,
      show_rep      = show_rep,
      shown         = shown,
      style         = style,
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
    data       = data,
    time_var   = time_var_str,
    ntime_var  = ntime_var_str,
    pred_var   = pred_var_str,
    sim_dv_var = sim_dv_var_str,
    obs_dv_var = obs_dv_var_str,
    strat_var  = strat_var_str,
    irep_name  = irep_name_str,
    loq        = loq,
    ci         = ci
  )

  plot_build_vpc(
    out,
    type          = "cens",
    min_bin_count = min_bin_count,
    show_rep      = show_rep,
    shown         = shown,
    style         = style,
    bin_var       = BIN_MID_VAR
  )
}
