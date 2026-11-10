#' Plot a dose-proportionality assessment via power law (log-log) regression
#'
#' @description
#' Dual-mode wrapper that delegates to [df_doseprop()] for computation and
#' [plot_build_doseprop()] for rendering. Accepts either:
#'
#' * raw observation data (e.g. PKNCA output) plus a `metrics` vector — the
#'   common one-shot mode; or
#' * a precomputed `doseprop_stats` object returned by [df_doseprop()] — skip
#'   the regression refit and replot with different `style` / `se` settings.
#'
#' On the precomputed path, pipeline arguments (`metrics`, `metric_name_var`,
#' `metric_value_var`, `dose_var`, `method`, `ci`, `sigdigits`) cannot be honored
#' because the regression does not run again — passing any of them aborts
#' with a message pointing the caller at [df_doseprop()]. Only `style` and
#' `se` are accepted on both paths.
#'
#' @param data Either raw observation data (data.frame, default expected
#'    format is output from `PKNCA::pk.nca()`) or a `doseprop_stats` object
#'    returned by [df_doseprop()].
#' @param metrics character vector of exposure metrics in `data` to plot.
#'    Required on the raw-data path; must not be passed with a
#'    `doseprop_stats` object.
#' @param se logical to display confidence interval around regression.
#'    Default is `TRUE`.
#' @param style A [ggstylekit::style_spec()] controlling plot aesthetics.
#'    Defaults to [style_doseprop()]; view the defaults by running
#'    `style_doseprop()` with no arguments. Customize by passing
#'    `style = style_doseprop(...)`. The per-metric facet layout is
#'    taken from the style's `facet_scales` (default `"free"`), `facet_nrow`,
#'    and `facet_ncol`; the `facet`, `logx`, and `logy` fields are ignored
#'    because the builder sets the facet and log-log axes itself.
#' @inheritParams df_doseprop
#'
#' @family dose proportionality
#' @return a `ggplot` plot object
#' @export plot_doseprop
#'
#' @examples
#' # Raw-data path
#' plot_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
#'                metrics = c("aucinf.obs", "cmax"))
#'
#' # Precomputed path: compute once, replot many times
#' stats <- df_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
#'                       metrics = c("aucinf.obs", "cmax"))
#' plot_doseprop(stats)
#' plot_doseprop(stats, se = FALSE)

plot_doseprop <- function(data,
                          metrics = NULL,
                          metric_name_var = "PPTESTCD",
                          metric_value_var = "PPORRES",
                          dose_var = "DOSE",
                          method = "normal",
                          ci = 0.9,
                          sigdigits = 3,
                          se = TRUE,
                          style = NULL) {

  if (inherits(data, "doseprop_stats")) {
    check_pipeline_args_dropped(
      mc             = match.call(),
      plot_only_args = c("data", "style", "se"),
      fn_name        = "plot_doseprop",
      stats_fn       = "df_doseprop"
    )
    return(plot_build_doseprop(data, style = style, se = se))
  }

  metric_name_var_str <- resolve_var(rlang::enquo(metric_name_var))
  metric_value_var_str    <- resolve_var(rlang::enquo(metric_value_var))
  dose_var_str   <- resolve_var(rlang::enquo(dose_var))

  stats <- df_doseprop(data, metrics,
                       metric_name_var = metric_name_var_str,
                       metric_value_var = metric_value_var_str,
                       dose_var = dose_var_str,
                       method = method, ci = ci, sigdigits = sigdigits)

  plot_build_doseprop(stats, style = style, se = se)
}
