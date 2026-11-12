#' Build a dose-proportionality ggplot from a `doseprop_stats` object
#'
#' @description
#' Constructs a log-log regression scatter plot from a [df_doseprop()] result
#' (or any object satisfying the `doseprop_stats` contract). Recovers the
#' observation rows and column names from the object's attributes, builds the
#' faceting label from the per-metric `PowerCI` text, and renders the scatter
#' + trend layers.
#'
#' Most users will reach this function indirectly via [plot_doseprop()].
#' Call `plot_build_doseprop()` directly when working from a manually-
#' constructed or cached `doseprop_stats` object — for example, plotting a
#' precomputed result from disk or a custom pipeline that produces compatible
#' columns and attributes.
#'
#' @param stats A `doseprop_stats` object (typically the output of
#'    [df_doseprop()]). Must contain `$obs` and `$config` slots with
#'    `metric_name_var`, `metric_value_var`, `dose_var`, and `ci`. Validated by
#'    `validate_doseprop_stats()` at entry.
#' @param style A [ggstylekit::style_spec()] controlling plot aesthetics.
#'    Defaults to [style_doseprop()]; view the defaults by running
#'    `style_doseprop()` with no arguments. Customize by passing
#'    `style = style_doseprop(...)`. The per-metric facet layout is
#'    taken from the style's `facet_scales` (default `"free"`), `facet_nrow`,
#'    and `facet_ncol`; the `facet`, `logx`, and `logy` fields are ignored
#'    because the builder sets the facet and log-log axes itself.
#' @param se logical to display confidence interval around regression. Default
#'    is `TRUE`.
#'
#' @family dose proportionality
#' @seealso [autoplot()][autoplot.pmx_stats], which draws a container with its builder.
#' @return a `ggplot` plot object
#' @export plot_build_doseprop
#'
#' @examples
#' stats <- df_doseprop(dplyr::filter(data_sad_nca, PART == "Part 1-SAD"),
#'                       metrics = c("aucinf.obs", "cmax"))
#' plot_build_doseprop(stats)
#' plot_build_doseprop(stats, se = FALSE)

plot_build_doseprop <- function(stats,
                                style = NULL,
                                se = TRUE) {

  check_boolean(se, "se")
  validate_doseprop_stats(stats)

  metric_name_var_str <- stats$config$metric_name_var
  metric_value_var_str    <- stats$config$metric_value_var
  dose_var_str   <- stats$config$dose_var
  ci             <- stats$config$ci
  obs            <- stats$obs

  plotstyle <- resolve_style(style, style_doseprop)

  tab <- stats$stats
  tab$label <- paste0(tab[[metric_name_var_str]], "\n", tab$PowerCI)

  plot_data <- dplyr::left_join(obs, tab, by = metric_name_var_str)

  base <- ggplot2::ggplot(plot_data, ggplot2::aes(
    x = .data[[dose_var_str]], y = .data[[metric_value_var_str]]))

  plot <- add_obs_layers_style(base, id_var_str = NULL)
  plot <- add_trend_layers_style(plot, method = "lm", show = TRUE, se = se,
                                 series = "linear",
                                 col_var_str = NULL, col_trend = FALSE,
                                 formula = y ~ x, level = ci)
  plot <- plot +
    ggplot2::labs(x = "Dose", y = "Exposure") +
    ggplot2::scale_x_log10(
      guide  = "axis_logticks",
      limits = function(x) c(10^floor(log10(x[1])), 10^ceiling(log10(x[2]))),
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ggplot2::scale_y_log10(
      guide  = "axis_logticks",
      limits = function(x) c(10^floor(log10(x[1])), 10^ceiling(log10(x[2]))),
      expand = ggplot2::expansion(mult = 0.02)
    ) +
    ## The per-metric facet is built here (its label carries the fitted power
    ## text), so ggstylekit's own facet path is bypassed; the style's facet
    ## layout fields are forwarded explicitly, as in plot_build_vpc().
    ggplot2::facet_wrap(~label,
                        nrow   = plotstyle$facet_nrow,
                        ncol   = plotstyle$facet_ncol,
                        scales = plotstyle$facet_scales %||% "free")

  ggstylekit::style_plot(plot, plotstyle)
}
