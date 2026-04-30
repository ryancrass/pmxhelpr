
#' Internal helper: Initialize a ggplot with standard theme
#'
#' Creates a ggplot object with the base theme (`theme_bw`) and removes minor
#' grid lines and major x-axis grid lines.
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
  plot +
    ggplot2::theme_bw() +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major.x = ggplot2::element_blank())
}


#' Internal helper: Add central tendency point, line, and error bar layers to a plot
#'
#' @param plot ggplot object to modify.
#' @param cent Character string specifying the central tendency measure.
#'    One of `"mean"`, `"mean_sdl"`, `"mean_sdl_upper"`, `"median"`, `"median_iqr"`, or `"none"`.
#' @param y_var Character string specifying the y variable name (e.g., `"DV"`, `"IPRED"`, `"PRED"`).
#' @param point_el A `pmx_point` element with point aesthetics.
#' @param line_el A `pmx_line` element with line aesthetics.
#' @param eb_el A `pmx_errorbar` element with error bar aesthetics.
#' @param width Numeric error bar cap width.
#' @param color_aes Optional character string for color aesthetic (e.g., `"DV"` for popgof legend).
#' @param show_errorbars Logical indicating whether to add error bar layers.
#'
#' @return A modified ggplot object with central tendency layers added
#' @keywords internal
#' @examples
#' #
#'
add_cent_layers <- function(plot, cent, y_var, point_el, line_el, eb_el, width,
                            color_aes = NULL,
                            show_errorbars = TRUE) {

  if (cent == "none") return(plot)

  mapping <- if (is.null(color_aes)) {
    ggplot2::aes(x = NTIME, y = .data[[y_var]])
  } else {
    ggplot2::aes(x = NTIME, y = .data[[y_var]], color = color_aes)
  }

  # Determine stat function
  is_mean <- cent %in% c("mean", "mean_sdl", "mean_sdl_upper")
  stat_fun <- if (is_mean) "mean" else "median"

  # Central Tendency Points
  plot <- plot + ggplot2::stat_summary(mapping,
                                       fun = stat_fun, geom = "point",
                                       size  = point_el$size,
                                       shape = point_el$shape,
                                       alpha = point_el$alpha)

  # Central Tendency Lines
  plot <- plot + ggplot2::stat_summary(mapping,
                                       fun = stat_fun, geom = "line",
                                       linewidth = line_el$linewidth,
                                       linetype  = line_el$linetype,
                                       alpha     = line_el$alpha)

  # Error Bars
  if (show_errorbars) {
    if (cent == "mean_sdl") {
      plot <- plot + ggplot2::stat_summary(mapping,
                                           fun.data = "mean_sdl",
                                           fun.args = list(mult = 1), geom = "errorbar",
                                           linewidth = eb_el$linewidth,
                                           linetype = eb_el$linetype,
                                           alpha = eb_el$alpha,
                                           width = width)
    }

    if (cent == "mean_sdl_upper") {
      plot <- plot + ggplot2::stat_summary(mapping,
                                           fun.max = function(x){mean(x) + stats::sd(x)},
                                           fun.min = function(x){NA_real_},
                                           geom = "errorbar",
                                           linewidth = eb_el$linewidth,
                                           linetype = eb_el$linetype,
                                           alpha = eb_el$alpha,
                                           width = width) +
                    ggplot2::stat_summary(mapping,
                                         fun.max = function(x){mean(x) + stats::sd(x)},
                                         fun.min = function(x){mean(x)},
                                         geom = "linerange",
                                         linewidth = eb_el$linewidth,
                                         linetype = eb_el$linetype,
                                         alpha = eb_el$alpha,
                                         show.legend = FALSE)
    }

    if (cent == "median_iqr") {
      plot <- plot + ggplot2::stat_summary(mapping,
                                           fun.max = function(x){stats::quantile(x, 0.75)},
                                           fun.min = function(x){stats::quantile(x, 0.25)},
                                           geom = "errorbar",
                                           linewidth = eb_el$linewidth,
                                           linetype = eb_el$linetype,
                                           alpha = eb_el$alpha,
                                           width = width)
    }
  }

  return(plot)
}


#' Add observed data point and spaghetti line layers to a plot
#'
#' Conditionally adds a \code{geom_point} layer for observed data and a
#' \code{geom_line} layer connecting observations within groups.
#'
#' @param plot ggplot object
#' @param obs_dv logical, whether to show observed data points
#' @param grp_dv logical, whether to connect observations within groups
#' @param grp_var_str character, column name for the grouping variable
#' @param point_el A `pmx_point` element with point aesthetics.
#' @param line_el A `pmx_line` element with line aesthetics.
#' @param color_aes optional string for color aesthetic (e.g., "OBS" for popgof legend)
#'
#' @return modified ggplot object
#' @keywords internal
add_obs_layers <- function(plot, obs_dv, grp_dv, grp_var_str, point_el, line_el,
                           color_aes = NULL) {

  if (isTRUE(obs_dv)) {
    point_aes <- if (!is.null(color_aes)) ggplot2::aes(color = color_aes) else NULL
    plot <- plot + ggplot2::geom_point(
      mapping = point_aes,
      shape   = point_el$shape,
      size    = point_el$size,
      alpha   = point_el$alpha
    )
  }

  if (isTRUE(grp_dv)) {
    if (!is.null(color_aes)) {
      line_aes <- ggplot2::aes(x = TIME, y = DV, color = color_aes,
                               group = .data[[grp_var_str]])
    } else {
      line_aes <- ggplot2::aes(x = TIME, y = DV,
                               group = .data[[grp_var_str]])
    }
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
      ggplot2::geom_hline(ggplot2::aes(yintercept = loq, linetype = loq_lab),
                          linewidth = loq_el$linewidth,
                          alpha = loq_el$alpha) +
      ggplot2::scale_linetype_manual(
        name = "LLOQ",
        values = stats::setNames(c(loq_el$linetype), loq_lab)) +
      ggplot2::guides(color = ggplot2::guide_legend(order = 1),
                      linetype = ggplot2::guide_legend(order = 2))
  } else {
    plot <- plot +
      ggplot2::geom_hline(yintercept = loq,
                          linewidth = loq_el$linewidth,
                          linetype = loq_el$linetype,
                          alpha = loq_el$alpha)
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
  plot + ggplot2::geom_hline(yintercept = as.numeric(ref),
                             linewidth = ref_el$linewidth,
                             linetype = ref_el$linetype,
                             alpha = ref_el$alpha)
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


#' Internal helper: Add observation point layer to a DV-vs-concentration plot
#'
#' @param plot ggplot object to modify
#' @param point_el A `pmx_point` element with point aesthetics.
#' @param col_var_str String name of the color variable, or `NULL`
#'
#' @return The modified ggplot object
#' @keywords internal
add_obs_point_layer <- function(plot, point_el, col_var_str) {
  if (is.null(col_var_str)) {
    plot + ggplot2::geom_point(shape = point_el$shape,
                               size = point_el$size,
                               alpha = point_el$alpha)
  } else {
    plot + ggplot2::geom_point(ggplot2::aes(color = .data[[col_var_str]]),
                               shape = point_el$shape,
                               size = point_el$size,
                               alpha = point_el$alpha)
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
#' @param obs_dv Logical indicating if observed data points are shown.
#' @param grp_dv Logical indicating if observations are connected within groups.
#' @param theme User-supplied theme overrides (named list), or `NULL`.
#' @param theme_fn Theme factory function (e.g., `plot_dvtime_theme`).
#'
#' @return A named list with elements `caption`, `plottheme`, and `width`.
#' @keywords internal
#' @examples
#' data <- dplyr::rename(dplyr::filter(data_sad, CMT %in% c(1,2)), DV = ODV)
#' env <- pmxhelpr:::prep_plot_env(data, cent = "mean", log_y = FALSE,
#'   obs_dv = TRUE, grp_dv = FALSE,
#'   theme = NULL, theme_fn = plot_dvtime_theme)
#'
prep_plot_env <- function(data, cent, log_y, obs_dv, grp_dv, theme, theme_fn) {
  caption   <- caption_dvtime(cent, log_y, obs_dv, grp_dv)
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
  if(is.numeric(plottheme$cent_errorbar$width)) plottheme$cent_errorbar$width
  else max(data$NTIME, na.rm = TRUE) * 0.025
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
