
# ---------------------------------------------------------------------------
# Plot families and shared helpers
#
# pmxhelpr exposes three plot families. They legitimately diverge in
# preprocessing and layer construction; the panel theme is applied
# uniformly via apply_panel_theme(), parameterized per family.
#
# 1. Continuous family — plot_dvtime, plot_gof, plot_dvconc
#    Preprocess: df_prep_dvtime (renames TIME/NTIME/DV, BLQ, dose-norm).
#    Init:       init_plot (theme_bw + apply_panel_theme()).
#    Layers:     add_ref_layers, add_blq_layers, add_obs_layers[_manual],
#                add_cent_layers[_manual], add_trend_layers.
#    Env:        prep_plot_env (caption + merged theme + errorbar width).
#
# 2. Regression family — plot_doseprop
#    Preprocess: df_doseprop (per-metric log-log fits + obs subset attached
#                as `obs` attribute on a `doseprop_stats`-classed data.frame).
#    Build:      plot_build_doseprop (ggplot from a doseprop_stats object).
#    Layers:     add_obs_layers (no col_var/id_var), add_trend_layers (lm).
#    Panel:      apply_panel_theme(keep_major_x = TRUE) — keeps major.x for
#                log-log readability.
#
# 3. VPC family — plot_vpc_cont
#    Preprocess: df_vpcpreprocess (validate, EVID=0, BLQ encode).
#    Compute:    df_vpccompute (pred-correction, quantile aggregation).
#    Build:      plot_build_vpc (ribbons/lines + obs overlay from compute output).
#    Panel:      apply_panel_theme(white_panel = TRUE) — white background.
#
# Shared across families: resolve_var (R/utils.R), merge_theme /
# merge_element (R/utils_theme.R), pmx_* element constructors, build_layer,
# apply_panel_theme.
# ---------------------------------------------------------------------------


#' Internal helper: Apply unified panel theme to a ggplot
#'
#' All three plot families (continuous, regression, VPC) blank
#' `panel.grid.minor` and differ only in two switches:
#' `keep_major_x` (regression keeps it for log-log readability) and
#' `white_panel` (VPC uses a white panel rather than `theme_bw()`'s grey).
#' This helper centralizes the panel theme application so each family picks
#' its variant via parameters rather than re-stating an inline `theme()`
#' block.
#'
#' @param plot A ggplot2 object.
#' @param keep_major_x Logical. When `FALSE` (default), `panel.grid.major.x`
#'    is blanked. Set `TRUE` for the regression family (log-log axes benefit
#'    from the major-x gridline).
#' @param white_panel Logical. When `TRUE`, sets `panel.background` to a
#'    white rectangle with a thin black border. Default is `FALSE`. Used by
#'    the VPC family.
#'
#' @return A ggplot2 object with the unified panel theme applied.
#' @keywords internal
apply_panel_theme <- function(plot, keep_major_x = FALSE, white_panel = FALSE) {
  args <- list(panel.grid.minor = ggplot2::element_blank())
  if (!isTRUE(keep_major_x)) {
    args$panel.grid.major.x <- ggplot2::element_blank()
  }
  if (isTRUE(white_panel)) {
    args$panel.background <- ggplot2::element_rect(fill = "white",
                                                   linewidth = 0.5,
                                                   color = "black")
  }
  plot + do.call(ggplot2::theme, args)
}


#' Internal helper: Initialize a ggplot with standard theme
#'
#' Creates a ggplot object with the base theme (`theme_bw`) and the unified
#' panel theme via [apply_panel_theme()] (continuous-family defaults: minor
#' and major.x gridlines blanked).
#'
#' @param data data.frame to plot
#' @param x_var String name of the x-axis variable
#' @param y_var String name of the y-axis variable
#' @param col_var_str String name of the color variable, or `NULL`
#'
#' @return A ggplot object with base theme applied
#' @keywords internal
init_plot <- function(data, x_var, y_var, col_var_str = NULL) {
  if (is.null(col_var_str)) {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))
  } else {
    plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]],
                                                color = .data[[col_var_str]]))
  }
  apply_panel_theme(plot + ggplot2::theme_bw())
}


#' Internal helper: Build a ggplot2 layer with conditional color
#'
#' Wraps `do.call()` and includes `color` in the argument list only when
#' color is not mapped via aes and a non-NULL value is provided.
#'
#' @param layer_fn The geom/stat function (e.g., `ggplot2::geom_point`)
#' @param args Named list of arguments to pass
#' @param color Fixed color value, or `NULL`
#' @param color_mapped Logical: is color already mapped via aes?
#'
#' @return A ggplot2 layer object
#' @keywords internal
build_layer <- function(layer_fn, args, color = NULL, color_mapped = FALSE) {
  if (!isTRUE(color_mapped) && !is.null(color)) {
    args$color <- color
  }
  do.call(layer_fn, args)
}


#' Internal helper: Add central tendency point, line, and error bar layers
#'
#' Use this variant when color is either fixed (from theme) or inherited from
#' a global `col_var` aesthetic set in [init_plot()]. The layers do not
#' contribute to a `scale_color_manual` legend.
#'
#' For GOF-style overlay plots — multiple cent layers with different y-vars
#' (DV / PRED / IPRED) sharing a manual color legend — use
#' [add_cent_layers_manual()] instead. The two helpers implement different
#' `aes()` contracts (inherited/fixed color vs. literal-label color routed
#' through `scale_color_manual()`) and intentionally are not unified.
#'
#' @param plot ggplot object to modify.
#' @param cent Character string specifying the central tendency measure.
#'    One of `"mean"`, `"mean_sdl"`, `"mean_sdl_upper"`, `"median"`, `"median_iqr"`, or `"none"`.
#' @param y_var Character string specifying the y variable name (e.g., `"DV"`).
#' @param point_el A `pmx_point` element with point aesthetics.
#' @param line_el A `pmx_line` element with line aesthetics.
#' @param eb_el A `pmx_errorbar` element with error bar aesthetics.
#' @param width Numeric error bar cap width.
#' @param color_mapped Logical indicating whether color is mapped via
#'    a global aesthetic (e.g., `col_var`). When `TRUE`, fixed theme colors
#'    are suppressed to allow inheritance.
#' @param show_errorbars Logical indicating whether to add error bar layers.
#'
#' @return A modified ggplot object with central tendency layers added
#' @keywords internal
#' @examples
#' #
#'
add_cent_layers <- function(plot, cent, y_var, point_el, line_el, eb_el, width,
                            color_mapped = FALSE, show_errorbars = TRUE) {

  if (cent == "none") return(plot)

  mapping <- ggplot2::aes(x = NTIME, y = .data[[y_var]])

  is_mean <- cent %in% c("mean", "mean_sdl", "mean_sdl_upper")
  stat_fun <- if (is_mean) "mean" else "median"

  # Central Tendency Points
  plot <- plot + build_layer(ggplot2::stat_summary,
    args = list(mapping = mapping, fun = stat_fun, geom = "point",
                size = point_el$size, shape = point_el$shape, alpha = point_el$alpha),
    color = point_el$color, color_mapped = color_mapped)

  # Central Tendency Lines
  plot <- plot + build_layer(ggplot2::stat_summary,
    args = list(mapping = mapping, fun = stat_fun, geom = "line",
                linewidth = line_el$linewidth, linetype = line_el$linetype,
                alpha = line_el$alpha),
    color = line_el$color, color_mapped = color_mapped)

  # Error Bars
  if (show_errorbars) {
    eb_args <- list(mapping = mapping, geom = "errorbar",
                    linewidth = eb_el$linewidth, linetype = eb_el$linetype,
                    alpha = eb_el$alpha, width = width)

    if (cent == "mean_sdl") {
      plot <- plot + build_layer(ggplot2::stat_summary,
        args = c(list(fun.data = "mean_sdl", fun.args = list(mult = 1)), eb_args),
        color = eb_el$color, color_mapped = color_mapped)
    }

    if (cent == "mean_sdl_upper") {
      plot <- plot + build_layer(ggplot2::stat_summary,
        args = c(list(fun.max = function(x){mean(x) + stats::sd(x)},
                      fun.min = function(x){NA_real_}), eb_args),
        color = eb_el$color, color_mapped = color_mapped)
      plot <- plot + build_layer(ggplot2::stat_summary,
        args = list(mapping = mapping, geom = "linerange",
                    fun.max = function(x){mean(x) + stats::sd(x)},
                    fun.min = function(x){mean(x)},
                    linewidth = eb_el$linewidth, linetype = eb_el$linetype,
                    alpha = eb_el$alpha, show.legend = FALSE),
        color = eb_el$color, color_mapped = color_mapped)
    }

    if (cent == "median_iqr") {
      plot <- plot + build_layer(ggplot2::stat_summary,
        args = c(list(fun.max = function(x){stats::quantile(x, 0.75)},
                      fun.min = function(x){stats::quantile(x, 0.25)}), eb_args),
        color = eb_el$color, color_mapped = color_mapped)
    }
  }

  return(plot)
}


#' Add central tendency layers for GOF overlay plots
#'
#' Use this variant for GOF-style overlay plots where multiple cent layers
#' with different y-vars (DV / PRED / IPRED) share a single manual color
#' legend. The layer's `color` is set to the literal string `color_aes`
#' inside `aes()` so ggplot treats it as a discrete level, then
#' `scale_color_manual()` (added by the calling plot function) maps each
#' level to a color. Fixed theme colors are never applied.
#'
#' For single-y plots where color is inherited from a global `col_var`
#' aesthetic or fixed from the theme, use [add_cent_layers()]. The two
#' helpers implement different `aes()` contracts and intentionally are not
#' unified.
#'
#' @inheritParams add_cent_layers
#' @param color_aes String label for color aesthetic (e.g., `"DV"`, `"PRED"`).
#'
#' @return A modified ggplot object with central tendency layers added
#' @keywords internal
add_cent_layers_manual <- function(plot, cent, y_var, point_el, line_el, eb_el,
                                   width, color_aes, show_errorbars = TRUE) {

  if (cent == "none") return(plot)

  mapping <- ggplot2::aes(x = NTIME, y = .data[[y_var]], color = color_aes)

  is_mean <- cent %in% c("mean", "mean_sdl", "mean_sdl_upper")
  stat_fun <- if (is_mean) "mean" else "median"

  # Central Tendency Points
  plot <- plot + ggplot2::stat_summary(mapping,
    fun = stat_fun, geom = "point",
    size = point_el$size, shape = point_el$shape, alpha = point_el$alpha)

  # Central Tendency Lines
  plot <- plot + ggplot2::stat_summary(mapping,
    fun = stat_fun, geom = "line",
    linewidth = line_el$linewidth, linetype = line_el$linetype,
    alpha = line_el$alpha)

  # Error Bars
  if (show_errorbars) {
    if (cent == "mean_sdl") {
      plot <- plot + ggplot2::stat_summary(mapping,
        fun.data = "mean_sdl", fun.args = list(mult = 1), geom = "errorbar",
        linewidth = eb_el$linewidth, linetype = eb_el$linetype,
        alpha = eb_el$alpha, width = width)
    }

    if (cent == "mean_sdl_upper") {
      plot <- plot + ggplot2::stat_summary(mapping,
        fun.max = function(x){mean(x) + stats::sd(x)},
        fun.min = function(x){NA_real_}, geom = "errorbar",
        linewidth = eb_el$linewidth, linetype = eb_el$linetype,
        alpha = eb_el$alpha, width = width)
      plot <- plot + ggplot2::stat_summary(mapping,
        fun.max = function(x){mean(x) + stats::sd(x)},
        fun.min = function(x){mean(x)}, geom = "linerange",
        linewidth = eb_el$linewidth, linetype = eb_el$linetype,
        alpha = eb_el$alpha, show.legend = FALSE)
    }

    if (cent == "median_iqr") {
      plot <- plot + ggplot2::stat_summary(mapping,
        fun.max = function(x){stats::quantile(x, 0.75)},
        fun.min = function(x){stats::quantile(x, 0.25)}, geom = "errorbar",
        linewidth = eb_el$linewidth, linetype = eb_el$linetype,
        alpha = eb_el$alpha, width = width)
    }
  }

  return(plot)
}


#' Add observed data point and spaghetti line layers to a plot
#'
#' Adds a \code{geom_point} layer for observed data (visibility controlled by
#' theme alpha) and optionally a \code{geom_line} layer connecting observations
#' within groups when `id_var_str` is specified. Color is mapped to a data
#' column when `col_var_str` is provided.
#'
#' Use this variant when color is column-mapped, inherited from a global
#' aesthetic, or fixed from the theme. For GOF-style overlay plots that
#' route color through `scale_color_manual()` via a literal label, use
#' [add_obs_layers_manual()] instead. The two helpers implement different
#' `aes()` contracts and intentionally are not unified.
#'
#' @param plot ggplot object
#' @param id_var_str character column name for spaghetti line grouping, or `NULL` for no lines
#' @param point_el A `pmx_point` element with point aesthetics.
#' @param line_el A `pmx_line` element with line aesthetics.
#' @param col_var_str optional string column name for color aesthetic, or `NULL`
#'
#' @return modified ggplot object
#' @keywords internal
add_obs_layers <- function(plot, id_var_str, point_el, line_el,
                           col_var_str = NULL) {

  color_mapped <- !is.null(col_var_str)
  point_mapping <- if (color_mapped) ggplot2::aes(color = .data[[col_var_str]]) else NULL

  plot <- plot + build_layer(ggplot2::geom_point,
    args = compact(list(mapping = point_mapping,
                        shape = point_el$shape, size = point_el$size,
                        alpha = point_el$alpha)),
    color = point_el$color, color_mapped = color_mapped)

  if (!is.null(id_var_str)) {
    line_mapping <- if (color_mapped) {
      ggplot2::aes(x = TIME, y = DV, color = .data[[col_var_str]],
                   group = .data[[id_var_str]])
    } else {
      ggplot2::aes(x = TIME, y = DV, group = .data[[id_var_str]])
    }
    plot <- plot + build_layer(ggplot2::geom_line,
      args = list(mapping = line_mapping,
                  linewidth = line_el$linewidth, linetype = line_el$linetype,
                  alpha = line_el$alpha),
      color = line_el$color, color_mapped = color_mapped)
  }

  plot
}

#' Add observed data layers for GOF plots with manual legend
#'
#' Use this variant for GOF-style overlay plots where the obs layer
#' participates in a manual color legend alongside cent layers for DV /
#' PRED / IPRED. The layer's `color` is set to the literal string
#' `color_aes` inside `aes()`, which ggplot treats as a discrete level
#' that `scale_color_manual()` then maps to a color.
#'
#' For column-mapped, inherited, or fixed-from-theme color, use
#' [add_obs_layers()]. The two helpers implement different `aes()`
#' contracts and intentionally are not unified.
#'
#' @param plot ggplot object
#' @param id_var_str character column name for spaghetti line grouping, or `NULL` for no lines
#' @param point_el A `pmx_point` element with point aesthetics.
#' @param line_el A `pmx_line` element with line aesthetics.
#' @param color_aes string label for color aesthetic (e.g., "OBS")
#'
#' @return modified ggplot object
#' @keywords internal
add_obs_layers_manual <- function(plot, id_var_str, point_el, line_el, color_aes) {

  plot <- plot + ggplot2::geom_point(
    mapping = ggplot2::aes(color = color_aes),
    shape   = point_el$shape,
    size    = point_el$size,
    alpha   = point_el$alpha
  )

  if (!is.null(id_var_str)) {
    line_aes <- ggplot2::aes(x = TIME, y = DV, color = color_aes,
                             group = .data[[id_var_str]])
    plot <- plot + ggplot2::geom_line(
      mapping   = line_aes,
      linewidth = line_el$linewidth,
      linetype  = line_el$linetype,
      alpha     = line_el$alpha
    )
  }

  plot
}


#' Add LOQ reference line and caption to a plot
#'
#' Conditionally adds an horizontal reference line at the LLOQ and appends
#' BLQ imputation method text to the plot caption.
#'
#' @param plot ggplot object
#' @param caption character, current caption string
#' @param loq_method numeric, 0/1/2
#' @param loq numeric, lower limit of quantification value
#' @param dosenorm logical, whether dose normalization is active
#' @param loq_el A `pmx_line` element with LOQ line aesthetics.
#' @param show_legend logical, whether to add LLOQ to the linetype legend
#'
#' @return A named list with elements `plot` (modified ggplot) and `caption` (modified string)
#' @keywords internal
add_blq_layers <- function(plot, caption, loq_method, loq, dosenorm, loq_el,
                           show_legend = FALSE) {

  if (!loq_method %in% c(1, 2) || isTRUE(dosenorm)) {
    return(list(plot = plot, caption = caption))
  }

  if (isTRUE(show_legend)) {
    loq_lab <- paste0(loq)
    plot <- plot +
      build_layer(ggplot2::geom_hline,
        args = list(mapping = ggplot2::aes(yintercept = loq, linetype = loq_lab),
                    linewidth = loq_el$linewidth, alpha = loq_el$alpha),
        color = loq_el$color) +
      ggplot2::scale_linetype_manual(
        name = "LLOQ",
        values = stats::setNames(c(loq_el$linetype), loq_lab)) +
      ggplot2::guides(color = ggplot2::guide_legend(order = 1),
                      linetype = ggplot2::guide_legend(order = 2))
  } else {
    plot <- plot +
      build_layer(ggplot2::geom_hline,
        args = list(yintercept = loq, linewidth = loq_el$linewidth,
                    linetype = loq_el$linetype, alpha = loq_el$alpha),
        color = loq_el$color)
  }

  blq_captions <- c(`1` = "Post-dose BLQ observations are imputed to 1/2 LLOQ",
                     `2` = "All BLQ observations are imputed to 1/2 LLOQ")
  caption <- paste0(caption, "\n", blq_captions[[as.character(loq_method)]])

  list(plot = plot, caption = caption)
}


#' Internal helper: Add horizontal reference line
#'
#' Conditionally adds a horizontal reference line at the specified y-intercept.
#' Draws the line when `ref` is non-NULL.
#'
#' @param plot ggplot object to modify
#' @param ref Numeric y-intercept for the reference line, or `NULL` for no line.
#' @param ref_el A `pmx_line` element with reference line aesthetics.
#'
#' @return The (possibly modified) ggplot object
#' @keywords internal
add_ref_layers <- function(plot, ref, ref_el) {
  if (is.null(ref)) return(plot)
  plot + build_layer(ggplot2::geom_hline,
    args = list(yintercept = as.numeric(ref), linewidth = ref_el$linewidth,
                linetype = ref_el$linetype, alpha = ref_el$alpha),
    color = ref_el$color)
}


#' Internal helper: Add a trend line layer to a DV-vs-concentration plot
#'
#' Adds a `geom_smooth` layer for the specified `method`. When `col_var_str`
#' is non-NULL and `col_trend` is TRUE, the color and fill aesthetics are
#' mapped to the grouping variable; otherwise, fixed colors from `plottheme`
#' are used.
#'
#' @param plot ggplot object to modify
#' @param method Character smoothing method (`"loess"` or `"lm"`)
#' @param show Logical indicating whether to add this trend layer
#' @param se Logical indicating whether to show the standard error ribbon
#' @param plottheme Named list of theme aesthetics (must contain element named by `theme_key`)
#' @param col_var_str String name of the color variable, or `NULL`
#' @param col_trend Logical indicating if trends should be colored by group
#' @param ... Additional arguments passed to `geom_smooth` (e.g., `span`)
#' @param theme_key Character key to look up in `plottheme`. Defaults to `method`.
#'
#' @return The (possibly modified) ggplot object
#' @keywords internal
add_trend_layers <- function(plot, method, show, se, plottheme,
                              col_var_str, col_trend, ...,
                              theme_key = method) {
  if (!isTRUE(show)) return(plot)
  theme_el <- plottheme[[theme_key]]
  if (isTRUE(col_trend) && !is.null(col_var_str)) {
    plot + ggplot2::geom_smooth(
      ggplot2::aes(color = .data[[col_var_str]], fill = .data[[col_var_str]]),
      method = method, se = se,
      linewidth = theme_el$linewidth, linetype = theme_el$linetype,
      alpha = theme_el$se_alpha, ...)
  } else {
    plot + ggplot2::geom_smooth(
      method = method, se = se,
      linewidth = theme_el$linewidth, linetype = theme_el$linetype,
      color = theme_el$color, fill = theme_el$se_color,
      alpha = theme_el$se_alpha, ...)
  }
}




#' Internal helper: Prepare the plot environment for DV vs time family plots
#'
#' Generates caption text, merges theme overrides with defaults,
#' and computes error bar width.
#'
#' @param data data.frame containing processed data with `NTIME` column.
#' @param cent Character string specifying central tendency measure.
#' @param log_y Logical indicating log-scale y-axis.
#' @param theme User-supplied theme overrides (named list), or `NULL`.
#' @param theme_fn Theme factory function (e.g., `plot_dvtime_theme`).
#'
#' @return A named list with elements `caption`, `plottheme`, and `width`.
#' @keywords internal
#' @examples
#' data <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1,2)), DV = ODV)
#' env <- pmxhelpr:::prep_plot_env(data, cent = "mean", log_y = FALSE,
#'   theme = NULL, theme_fn = plot_dvtime_theme)
#'
prep_plot_env <- function(data, cent, log_y, theme, theme_fn) {
  caption   <- caption_dvtime(cent, log_y)
  plottheme <- merge_theme(theme, theme_fn())
  width     <- errorbar_width(plottheme, data)
  list(caption = caption, plottheme = plottheme, width = width)
}


#' Internal helper: Compute error bar width
#'
#' Returns the user-specified error bar width from the theme, or defaults
#' to 2.5 percent of the maximum `NTIME` value.
#'
#' @param plottheme Named list of theme elements containing `$cent_errorbar$width`.
#' @param data data.frame containing `NTIME` column.
#'
#' @return Numeric error bar width value
#' @keywords internal
#' @examples
#' theme <- plot_dvtime_theme()
#' data <- data.frame(NTIME = c(0, 1, 2, 4, 8, 24))
#' pmxhelpr:::errorbar_width(theme, data)
#'
errorbar_width <- function(plottheme, data) {
  if(is.numeric(plottheme$cent_errorbar$width)) return(plottheme$cent_errorbar$width)
  if (!"NTIME" %in% colnames(data) || nrow(data) == 0L || all(is.na(data$NTIME))) {
    rlang::warn("cannot compute default errorbar width: `NTIME` is empty or all NA. Returning NA; set `width` via `pmx_errorbar(width = ...)` to suppress this warning.")
    return(NA_real_)
  }
  max(data$NTIME, na.rm = TRUE) * 0.025
}


#' Determine axis breaks automatically for time variables
#'
#' @param x Numeric vector of times from which to determine breaks
#' @param unit Character string for time units.
#'    Options include:
#'    + "hours" (default), "hrs", "hour", "hr", "h"
#'    + "days", "dys", "day", "dy", "d"
#'    + "weeks", "wks", "week", "wk", "w"
#'    + "months", "mons", "mos", "month", "mo", "m"
#'
#' @param n Ideal number of axis breaks requested (default = 8). Passed to `labeling::extended()`
#'
#' @return A numeric vector of breaks
#' @keywords internal
#' @examples
#' ntimes <- sort(unique(data_sad$NTIME))
#' breaks <- pmxhelpr:::var_timebreaks(ntimes)

var_timebreaks <- function(x, unit = "hours", n = 8) {
  check_numeric(x, "x")
  check_timeu(unit)
  check_integer(n, "n")

  x <- as.numeric(x)
  if (length(x) == 0L || all(is.na(x))) {
    rlang::abort("argument `x` must contain at least one non-NA numeric value")
  }
  rng <- range(x, na.rm = TRUE)

  if (unit %in% c("hours", "hrs", "hour", "hr", "h")) {
    scale <- 24
  } else if (unit %in% c("days", "dys", "day", "dy", "d")) {
    scale <- 7
  } else if (unit %in% c("weeks", "wks", "week", "wk", "w")) {
    scale <- 1
  } else if (unit %in% c("months", "mons", "mos", "month", "mo", "m")) {
    scale <- 1
  }

  rng <- rng / scale

  if(max(rng, na.rm = TRUE) <= 1) {
    if(unit %in% c("hours", "hrs", "hour", "hr", "h")) Ql <- c(4/24, 8/24, 12/24, 1)
    if(unit %in% c("days", "dys", "day", "dy", "d")) Ql <- c(1/7, 1)
    if(unit %in% c("weeks", "wks", "week", "wk", "w",
                   "months", "mons", "mos", "month", "mo", "m")) Ql <- c(0.5, 1)

    breaks <- labeling::extended(
      rng[1], rng[2], n,
      Q = Ql,
      only.loose = FALSE) * scale

    breaks <- breaks[breaks <= max(x, na.rm = TRUE)]
  } else {
    breaks <- labeling::extended(
      rng[1], rng[2], n,
      Q = c(1, 2, 4, 7),
      only.loose = FALSE) * scale

    breaks <- breaks[breaks <= max(x, na.rm = TRUE)]
  }

  breaks
}
