# ===========================================================================
# ggstylekit-based layer builders
#
# These `*_style()` helpers emit bare geoms tagged with
# `ggstylekit::series_layer()` so `style_plot()` fills their role-keyed fixed
# aesthetics (points, lines, error bars). The LLOQ line reads its aesthetics
# inline via `series_aes()` because it maps a label to a manual linetype scale.
# Series names are the plot role keys.
# ===========================================================================


#' Internal helper: add a horizontal reference line (style pattern)
#'
#' @param plot ggplot object to modify.
#' @param ref Numeric y-intercept, or `NULL` for no line.
#'
#' @return The (possibly modified) ggplot object, with the reference line tagged
#'   as the `"ref_line"` series for [ggstylekit::style_plot()].
#' @keywords internal
add_ref_layer_style <- function(plot, ref) {
  if (is.null(ref)) return(plot)
  plot + ggstylekit::series_layer(
    ggplot2::geom_hline(yintercept = as.numeric(ref)), "ref_line")
}


#' Internal helper: add observed points and spaghetti lines (style pattern)
#'
#' Color may be inherited from the plot's global `aes()` (as [plot_dvtime()]
#' does, leaving `col_var_str = NULL`) or mapped at the layer level via
#' `col_var_str` (as [plot_dvconc()] does, so only the points are colored while
#' trend lines stay unstratified). The `obs_point`/`obs_line` series aesthetics
#' are filled by [ggstylekit::style_plot()].
#'
#' @param plot ggplot object.
#' @param id_var_str Column name for spaghetti grouping, or `NULL` for no lines.
#' @param col_var_str Column name to map to color at the layer level, or `NULL`
#'   to inherit color from the plot's global mapping.
#'
#' @return Modified ggplot object.
#' @keywords internal
add_obs_layers_style <- function(plot, id_var_str, col_var_str = NULL) {
  point <- if (is.null(col_var_str)) {
    ggplot2::geom_point()
  } else {
    ggplot2::geom_point(ggplot2::aes(color = .data[[col_var_str]]))
  }
  plot <- plot + ggstylekit::series_layer(point, "obs_point")

  if (!is.null(id_var_str)) {
    line_mapping <- if (is.null(col_var_str)) {
      ggplot2::aes(group = .data[[id_var_str]])
    } else {
      ggplot2::aes(color = .data[[col_var_str]], group = .data[[id_var_str]])
    }
    plot <- plot + ggstylekit::series_layer(
      ggplot2::geom_line(line_mapping), "obs_line")
  }
  plot
}


#' Internal helper: add a trend line layer (style pattern)
#'
#' Adds a `geom_smooth` for `method`, tagged as the `series` name (`"loess"` or
#' `"linear"`) so [ggstylekit::style_plot()] fills its color, linewidth,
#' linetype, SE-ribbon fill (base `line_fill`), and SE-ribbon alpha (the
#' series `alphas` entry). When `col_trend` is `TRUE` and a color variable is
#' supplied, color and fill are mapped to it instead.
#'
#' @param plot ggplot object.
#' @param method Smoothing method (`"loess"` or `"lm"`).
#' @param show Logical; whether to add the layer.
#' @param se Logical; whether to draw the SE ribbon.
#' @param series Series/role name for the style (e.g. `"loess"`, `"linear"`).
#' @param col_var_str Color variable name, or `NULL`.
#' @param col_trend Logical; stratify the trend by `col_var_str`.
#' @param ... Passed to [ggplot2::geom_smooth()] (e.g. `formula`, `level`, `span`).
#'
#' @return Modified ggplot object.
#' @keywords internal
add_trend_layers_style <- function(plot, method, show, se, series,
                                    col_var_str, col_trend, ...) {
  if (!isTRUE(show)) return(plot)
  smooth <- if (isTRUE(col_trend) && !is.null(col_var_str)) {
    ggplot2::geom_smooth(
      ggplot2::aes(color = .data[[col_var_str]], fill = .data[[col_var_str]]),
      method = method, se = se, ...)
  } else {
    ggplot2::geom_smooth(method = method, se = se, ...)
  }
  plot + ggstylekit::series_layer(smooth, series)
}


#' Internal helper: add LOQ reference line and BLQ caption (style pattern)
#'
#' Draws the LLOQ line (suppressed under `dosenorm`) and appends the BLQ
#' imputation caption. The line's fixed aesthetics come from the `"loq_line"`
#' series of `style`. When `show_legend` is `TRUE`, the linetype is mapped to a
#' `"LLOQ = <value>"` label with a manual scale so the line joins the legend;
#' `style_plot()` preserves this manual scale.
#'
#' @param plot ggplot object.
#' @param caption Current caption string.
#' @param loq_method Numeric BLQ method (0/1/2).
#' @param loq Numeric LLOQ value.
#' @param dosenorm Logical; whether dose normalization is active.
#' @param style A `ggstylekit_style_spec` supplying the `loq_line` aesthetics.
#' @param show_legend Logical; whether to add LLOQ to the linetype legend.
#'
#' @return A named list with `plot` (modified ggplot) and `caption` (modified
#'   string).
#' @keywords internal
add_loq_layer_style <- function(plot, caption, loq_method, loq, dosenorm, style,
                                show_legend = FALSE) {
  if (!loq_method %in% c(1, 2)) {
    return(list(plot = plot, caption = caption))
  }

  if (!isTRUE(dosenorm)) {
    el <- series_aes(style, "loq_line")
    if (isTRUE(show_legend)) {
      loq_lab <- paste0(loq)
      hargs <- compact(list(
        mapping = ggplot2::aes(yintercept = loq, linetype = loq_lab),
        linewidth = el$linewidth, alpha = el$alpha, color = el$colour))
      plot <- plot +
        do.call(ggplot2::geom_hline, hargs) +
        ggplot2::scale_linetype_manual(
          name = "LLOQ",
          values = stats::setNames(el$linetype, loq_lab)) +
        ggplot2::guides(color = ggplot2::guide_legend(order = 1),
                        linetype = ggplot2::guide_legend(order = 2))
    } else {
      hargs <- compact(list(
        yintercept = loq, linewidth = el$linewidth,
        linetype = el$linetype, alpha = el$alpha, color = el$colour))
      plot <- plot + do.call(ggplot2::geom_hline, hargs)
    }
  }

  blq_captions <- c(`1` = "Post-dose BLQ observations are imputed to 1/2 LLOQ",
                    `2` = "All BLQ observations are imputed to 1/2 LLOQ")
  caption <- paste0(caption, "\n", blq_captions[[as.character(loq_method)]])

  list(plot = plot, caption = caption)
}


#' Internal helper: default the error bar cap width from the data
#'
#' Shared by [plot_dvtime()] and [plot_gof()]. The cap width is the
#' `errorbar_width` field of [ggstylekit::style_spec()], set through
#' `style_dvtime(errorbar_width = ...)` / `style_gof(errorbar_width = ...)`. The
#' presets leave it unset because the useful default is a data-scale quantity:
#' when `style$errorbar_width` is `NULL`, this back-fills 2.5% of the maximum
#' `NTIME` in `data`. A width set in the style always wins. When `NTIME` is
#' absent or all `NA` the field stays unset and ggplot2's default width applies
#' (`NA` is not a valid `errorbar_width`).
#'
#' @param style A `ggstylekit_style_spec`.
#' @param data The plot data, checked for an `NTIME` column.
#'
#' @return `style`, with `errorbar_width` filled when it was unset.
#' @keywords internal
style_errorbar_width <- function(style, data) {
  if (!is.null(style$errorbar_width)) return(style)
  if ("NTIME" %in% names(data) && any(!is.na(data$NTIME))) {
    ggstylekit::set_style(style,
                          errorbar_width = max(data$NTIME, na.rm = TRUE) * 0.025)
  } else style
}


#' Internal helper: error bar layers for a central-tendency summary
#'
#' Adds the `stat_summary` error bar layer(s) for `cent`, each tagged as the
#' `"cent_errorbar"` series so [ggstylekit::style_plot()] fills their
#' role-keyed fixed aesthetics and the style's `errorbar_width` (see
#' [style_errorbar_width()]). `"mean_sdl"` draws mean +/- SD, `"median_iqr"`
#' the 25th-75th percentiles, and `"mean_sdl_upper"` an upper-only bar (cap at
#' mean + SD plus a `geom_linerange` from the mean). Other `cent` values add
#' nothing.
#'
#' @param plot ggplot object.
#' @param cent Central tendency measure (see [plot_dvtime()]).
#' @param mapping The `aes()` shared with the point/line summary layers.
#'
#' @return Modified ggplot object.
#' @keywords internal
add_errorbar_layers_style <- function(plot, cent, mapping) {
  errorbar <- function(...) {
    ggstylekit::series_layer(
      ggplot2::stat_summary(mapping, geom = "errorbar", ...), "cent_errorbar")
  }
  if (cent == "mean_sdl") {
    plot <- plot + errorbar(fun.data = "mean_sdl", fun.args = list(mult = 1))
  }
  if (cent == "mean_sdl_upper") {
    upper <- function(x) mean(x) + stats::sd(x)
    plot <- plot + errorbar(fun.max = upper, fun.min = function(x) NA_real_)
    plot <- plot + ggstylekit::series_layer(
      ggplot2::stat_summary(mapping, geom = "linerange", show.legend = FALSE,
                            fun.max = upper, fun.min = function(x) mean(x)),
      "cent_errorbar")
  }
  if (cent == "median_iqr") {
    plot <- plot + errorbar(fun.max = function(x) stats::quantile(x, 0.75),
                            fun.min = function(x) stats::quantile(x, 0.25))
  }
  plot
}


#' Internal helper: add central tendency layers (style pattern)
#'
#' Central-tendency points, lines, and error bars are `stat_summary` layers
#' tagged as the `"cent_point"`/`"cent_line"`/`"cent_errorbar"` series for
#' [ggstylekit::style_plot()], which fills their role-keyed fixed aesthetics.
#' Error bars are ggstylekit's errorbar entity (`GeomErrorbar`/`GeomLinerange`),
#' so the cap width comes from the style's `errorbar_width` field (see
#' [style_errorbar_width()]). Color is inherited from the plot's global `aes()`
#' when mapped.
#'
#' @param plot ggplot object.
#' @param cent Central tendency measure (see [plot_dvtime()]).
#' @param y_var Y variable name (e.g. `"DV"`).
#'
#' @return Modified ggplot object.
#' @keywords internal
add_cent_layers_style <- function(plot, cent, y_var) {
  if (cent == "none") return(plot)

  mapping <- ggplot2::aes(x = .data$NTIME, y = .data[[y_var]])
  stat_fun <- if (cent %in% c("mean", "mean_sdl", "mean_sdl_upper")) "mean" else "median"

  plot <- plot + ggstylekit::series_layer(
    ggplot2::stat_summary(mapping, fun = stat_fun, geom = "point"), "cent_point")
  plot <- plot + ggstylekit::series_layer(
    ggplot2::stat_summary(mapping, fun = stat_fun, geom = "line"), "cent_line")

  add_errorbar_layers_style(plot, cent, mapping)
}


#' Internal helper: observed layers for a GOF overlay (style pattern)
#'
#' GOF overlays route color through a manual scale keyed by a literal label
#' (`"OBS"`), so color is mapped via `aes()` while the layer is tagged with
#' [ggstylekit::series_layer()] under its role name. [ggstylekit::style_plot()]
#' then fills the role-keyed fixed aesthetics (shape/size/linewidth/linetype/
#' alpha) and skips the mapped color channel, leaving it to the caller's
#' `scale_color_manual()`. Mirrors the single-series [add_obs_layers_style()]
#' but with a literal-label color contract.
#'
#' @param plot ggplot object.
#' @param id_var_str Column name for spaghetti grouping, or `NULL`.
#' @param color_aes Literal color label (e.g. `"OBS"`).
#'
#' @return Modified ggplot object.
#' @keywords internal
add_obs_layers_gof_style <- function(plot, id_var_str, color_aes) {
  plot <- plot + ggstylekit::series_layer(
    ggplot2::geom_point(ggplot2::aes(color = color_aes)), "obs_point")
  if (!is.null(id_var_str)) {
    plot <- plot + ggstylekit::series_layer(
      ggplot2::geom_line(ggplot2::aes(x = .data$TIME, y = .data$DV,
                                      color = color_aes,
                                      group = .data[[id_var_str]])), "obs_line")
  }
  plot
}


#' Internal helper: central tendency layers for a GOF overlay (style pattern)
#'
#' Like [add_cent_layers_style()] but for the GOF overlay: color is mapped to a
#' literal label (`"DV"`/`"PRED"`/`"IPRED"`) for the manual color legend. The
#' point, line, and error bar layers are tagged with
#' [ggstylekit::series_layer()] so [ggstylekit::style_plot()] fills their
#' role-keyed fixed aesthetics while leaving the mapped color channel to the
#' caller's scale.
#'
#' @inheritParams add_cent_layers_style
#' @param color_aes Literal color label.
#' @param show_errorbars Logical; add error bars (DV only in GOF).
#'
#' @return Modified ggplot object.
#' @keywords internal
add_cent_layers_gof_style <- function(plot, cent, y_var, color_aes,
                                      show_errorbars = TRUE) {
  if (cent == "none") return(plot)

  mapping <- ggplot2::aes(x = .data$NTIME, y = .data[[y_var]], color = color_aes)
  stat_fun <- if (cent %in% c("mean", "mean_sdl", "mean_sdl_upper")) "mean" else "median"

  plot <- plot + ggstylekit::series_layer(
    ggplot2::stat_summary(mapping, fun = stat_fun, geom = "point"), "cent_point")
  plot <- plot + ggstylekit::series_layer(
    ggplot2::stat_summary(mapping, fun = stat_fun, geom = "line"), "cent_line")

  if (isTRUE(show_errorbars)) {
    plot <- add_errorbar_layers_style(plot, cent, mapping)
  }

  plot
}
