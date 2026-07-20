# ===========================================================================
# ggstylekit-based layer builders
#
# These `*_style()` helpers emit bare geoms tagged with
# `ggstylekit::series_layer()` (for entity geoms styled by `style_plot()`) or
# with inline aesthetics read from the style via `series_aes()` (for non-entity
# geoms like error bars). Series names are the plot role keys. They replace the
# `pmx_element`-reading helpers above as each family migrates.
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


#' Internal helper: add central tendency layers (style pattern)
#'
#' Central-tendency points and lines are `stat_summary` layers tagged as the
#' `"cent_point"`/`"cent_line"` series for [ggstylekit::style_plot()]. Error
#' bars use `GeomErrorbar`/`GeomLinerange`, which are outside ggstylekit's
#' entity registry, so their aesthetics are set inline from the
#' `"cent_errorbar"` series via [series_aes()]. Color is inherited from the
#' plot's global `aes()` when mapped.
#'
#' @param plot ggplot object.
#' @param cent Central tendency measure (see [plot_dvtime()]).
#' @param y_var Y variable name (e.g. `"DV"`).
#' @param style A `ggstylekit_style_spec`.
#' @param width Numeric error bar cap width.
#'
#' @return Modified ggplot object.
#' @keywords internal
add_cent_layers_style <- function(plot, cent, y_var, style, width) {
  if (cent == "none") return(plot)

  mapping <- ggplot2::aes(x = .data$NTIME, y = .data[[y_var]])
  stat_fun <- if (cent %in% c("mean", "mean_sdl", "mean_sdl_upper")) "mean" else "median"

  plot <- plot + ggstylekit::series_layer(
    ggplot2::stat_summary(mapping, fun = stat_fun, geom = "point"), "cent_point")
  plot <- plot + ggstylekit::series_layer(
    ggplot2::stat_summary(mapping, fun = stat_fun, geom = "line"), "cent_line")

  eb <- series_aes(style, "cent_errorbar")
  eb_bar <- c(list(mapping = mapping, geom = "errorbar", width = width), eb)

  if (cent == "mean_sdl") {
    plot <- plot + do.call(ggplot2::stat_summary,
      c(list(fun.data = "mean_sdl", fun.args = list(mult = 1)), eb_bar))
  }
  if (cent == "mean_sdl_upper") {
    plot <- plot + do.call(ggplot2::stat_summary,
      c(list(fun.max = function(x) mean(x) + stats::sd(x),
             fun.min = function(x) NA_real_), eb_bar))
    plot <- plot + do.call(ggplot2::stat_summary,
      c(list(mapping = mapping, geom = "linerange", show.legend = FALSE,
             fun.max = function(x) mean(x) + stats::sd(x),
             fun.min = function(x) mean(x)), eb))
  }
  if (cent == "median_iqr") {
    plot <- plot + do.call(ggplot2::stat_summary,
      c(list(fun.max = function(x) stats::quantile(x, 0.75),
             fun.min = function(x) stats::quantile(x, 0.25)), eb_bar))
  }

  plot
}


#' Internal helper: observed layers for a GOF overlay (style pattern)
#'
#' GOF overlays route color through a manual scale keyed by a literal label
#' (`"OBS"`), so color is mapped via `aes()` rather than styled by
#' [ggstylekit::style_plot()]. The remaining fixed aesthetics are read inline
#' from the `obs_point`/`obs_line` series of `style` (no `series_layer()` tag,
#' so `style_plot()` leaves the color channel to the caller's
#' `scale_color_manual()`). Mirrors the single-series [add_obs_layers_style()]
#' but with a literal-label color contract.
#'
#' @param plot ggplot object.
#' @param id_var_str Column name for spaghetti grouping, or `NULL`.
#' @param style A `ggstylekit_style_spec`.
#' @param color_aes Literal color label (e.g. `"OBS"`).
#'
#' @return Modified ggplot object.
#' @keywords internal
add_obs_layers_gof_style <- function(plot, id_var_str, style, color_aes) {
  plot <- plot + do.call(ggplot2::geom_point,
    c(list(mapping = ggplot2::aes(color = color_aes)),
      series_aes(style, "obs_point")))
  if (!is.null(id_var_str)) {
    plot <- plot + do.call(ggplot2::geom_line,
      c(list(mapping = ggplot2::aes(x = .data$TIME, y = .data$DV,
                                    color = color_aes,
                                    group = .data[[id_var_str]])),
        series_aes(style, "obs_line")))
  }
  plot
}


#' Internal helper: central tendency layers for a GOF overlay (style pattern)
#'
#' Like [add_cent_layers_style()] but for the GOF overlay: color is mapped to a
#' literal label (`"DV"`/`"PRED"`/`"IPRED"`) for the manual color legend, and
#' all fixed aesthetics are read inline from `style` (no `series_layer()` tag).
#'
#' @inheritParams add_cent_layers_style
#' @param color_aes Literal color label.
#' @param show_errorbars Logical; add error bars (DV only in GOF).
#'
#' @return Modified ggplot object.
#' @keywords internal
add_cent_layers_gof_style <- function(plot, cent, y_var, style, width,
                                      color_aes, show_errorbars = TRUE) {
  if (cent == "none") return(plot)

  mapping <- ggplot2::aes(x = .data$NTIME, y = .data[[y_var]], color = color_aes)
  stat_fun <- if (cent %in% c("mean", "mean_sdl", "mean_sdl_upper")) "mean" else "median"

  plot <- plot + do.call(ggplot2::stat_summary,
    c(list(mapping = mapping, fun = stat_fun, geom = "point"),
      series_aes(style, "cent_point")))
  plot <- plot + do.call(ggplot2::stat_summary,
    c(list(mapping = mapping, fun = stat_fun, geom = "line"),
      series_aes(style, "cent_line")))

  if (isTRUE(show_errorbars)) {
    eb <- series_aes(style, "cent_errorbar")
    eb_bar <- c(list(mapping = mapping, geom = "errorbar", width = width), eb)
    if (cent == "mean_sdl") {
      plot <- plot + do.call(ggplot2::stat_summary,
        c(list(fun.data = "mean_sdl", fun.args = list(mult = 1)), eb_bar))
    }
    if (cent == "mean_sdl_upper") {
      plot <- plot + do.call(ggplot2::stat_summary,
        c(list(fun.max = function(x) mean(x) + stats::sd(x),
               fun.min = function(x) NA_real_), eb_bar))
      plot <- plot + do.call(ggplot2::stat_summary,
        c(list(mapping = mapping, geom = "linerange", show.legend = FALSE,
               fun.max = function(x) mean(x) + stats::sd(x),
               fun.min = function(x) mean(x)), eb))
    }
    if (cent == "median_iqr") {
      plot <- plot + do.call(ggplot2::stat_summary,
        c(list(fun.max = function(x) stats::quantile(x, 0.75),
               fun.min = function(x) stats::quantile(x, 0.25)), eb_bar))
    }
  }

  plot
}
