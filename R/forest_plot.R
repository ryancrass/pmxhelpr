#' Plot a forest plot of test and reference comparisons
#'
#' @description
#' Dual-mode wrapper that delegates to [df_forest()] for aggregation and
#' [plot_build_forest()] for rendering. Accepts either:
#'
#' * raw replicate-draws data plus column-name arguments — the common
#'   one-shot mode; or
#' * a precomputed `forest_stats` object returned by [df_forest()] — skip
#'   the aggregation and replot with different `style` / `ref` / `ref_band`
#'   / `ref_band_type` / `ref_layout` settings.
#'
#' On the precomputed path, pipeline arguments (`metric_name_var`,
#' `metric_value_var`, `cov_name_var`, `cov_level_var`, `ci`,
#' `sigdigits`, `cov_name_ref`, `cov_level_ref`) cannot be honored because
#' the aggregation does not run again — passing any of them aborts with a
#' message pointing the caller at [df_forest()]. Only `style`, `ref`,
#' `ref_band`, `ref_band_type`, `ref_layout`, `show_rep`, and `metric` are
#' accepted on both paths.
#'
#' @param data Either raw observation/draws data (data.frame) or a
#'    `forest_stats` object returned by [df_forest()].
#' @inheritParams df_forest
#' @inheritParams plot_build_forest
#'
#' @family forest plot
#' @return a `ggplot` plot object
#' @export plot_forest
#'
#' @examples
#' # Raw-data path: one metric per call
#' plot_forest(
#'   dplyr::filter(data_sad_pkforest, metric == "AUCRATIO"),
#'   cov_level_ref = attr(data_sad_pkforest, "cov_level_ref")
#' )
#'
#' # Precomputed path: compute once, render different metrics from the same stats
#' stats <- df_forest(
#'   data_sad_pkforest,
#'   cov_level_ref = attr(data_sad_pkforest, "cov_level_ref")
#' )
#' plot_forest(stats, metric = "AUCRATIO")
#' plot_forest(stats, metric = "CMAXRATIO", ref_band = c(0.8, 1.25))
#'
#' # Same stats, reference panel instead of a reference row in each covariate panel
#' plot_forest(stats, metric = "AUCRATIO", ref_layout = "consolidated")
#'
#' # Raw exposure from the same stats: reference line at the reference
#' # exposure, band at 0.8 to 1.25 times it (log x-axis keeps it symmetric)
#' plot_forest(stats, metric = "AUC", ref_band = c(0.8, 1.25),
#'             style = style_forest(logx = TRUE))
#'
#' # Per-covariate coloring via the style's `colors` map
#' plot_forest(
#'   stats, metric = "AUCRATIO",
#'   style = style_forest(colors = c(FOOD = "darkgreen", WTBL = "darkblue"))
#' )

plot_forest <- function(
  data,
  metric = NULL,
  metric_name_var = "metric",
  metric_value_var = "value",
  cov_name_var = "cov_name",
  cov_level_var = "cov_level",
  ci = 0.9,
  sigdigits = 3,
  cov_name_ref = "Reference",
  cov_level_ref = NULL,
  ref = "reference",
  ref_band = NULL,
  ref_band_type = c("multiplicative", "additive", "absolute"),
  ref_layout = NULL,
  show_rep = FALSE,
  style = NULL
) {
  if (inherits(data, "forest_stats")) {
    check_pipeline_args_dropped(
      call = match.call(),
      plot_only_args = c(
        "data", "style", "ref", "ref_band", "ref_band_type", "ref_layout",
        "show_rep", "metric"
      ),
      fn_name = "plot_forest",
      stats_fn = "df_forest"
    )
    stats <- data
  } else {
    metric_name_var_str <- resolve_var(rlang::enquo(metric_name_var))
    metric_value_var_str <- resolve_var(rlang::enquo(metric_value_var))
    cov_name_var_str <- resolve_var(rlang::enquo(cov_name_var))
    cov_level_var_str <- resolve_var(rlang::enquo(cov_level_var))
    stats <- df_forest(
      data,
      metric_name_var = metric_name_var_str,
      metric_value_var = metric_value_var_str,
      cov_name_var = cov_name_var_str,
      cov_level_var = cov_level_var_str,
      ci = ci,
      sigdigits = sigdigits,
      cov_name_ref = cov_name_ref,
      cov_level_ref = cov_level_ref
    )
  }

  plot_build_forest(
    stats,
    style = style,
    ref = ref,
    ref_band = ref_band,
    ref_band_type = ref_band_type,
    metric = metric,
    ref_layout = ref_layout,
    show_rep = show_rep
  )
}
