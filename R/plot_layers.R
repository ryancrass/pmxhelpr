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
