
# Internal helper: build aes for central tendency layers
build_cent_aes <- function(y_var, color_aes = NULL) {
  if (is.null(color_aes)) {
    ggplot2::aes(x = NTIME, y = .data[[y_var]])
  } else {
    ggplot2::aes(x = NTIME, y = .data[[y_var]], color = color_aes)
  }
}

# Internal helper: add central tendency point, line, and errorbar layers to a plot
#
# @param plot ggplot object
# @param cent character, one of "mean", "mean_sdl", "mean_sdl_upper", "median", "median_iqr", "none"
# @param y_var character, the y variable name (e.g., "DV", "IPRED", "PRED")
# @param plottheme named list of theme elements (must contain $cent and $errorbar)
# @param width numeric, errorbar cap width
# @param color_aes optional string for color aesthetic (e.g., "DV" for popgof legend)
# @param line_element optional element for line aesthetics; defaults to plottheme$cent
# @param show_errorbars logical, whether to add errorbar layers
add_cent_layers <- function(plot, cent, y_var, plottheme, width,
                            color_aes = NULL,
                            line_element = NULL,
                            show_errorbars = TRUE) {

  if (cent == "none") return(plot)

  mapping <- build_cent_aes(y_var, color_aes)

  # Determine stat function
  is_mean <- cent %in% c("mean", "mean_sdl", "mean_sdl_upper")
  stat_fun <- if (is_mean) "mean" else "median"

  # Resolve line element
  le <- if (!is.null(line_element)) line_element else plottheme$cent

  # Central Tendency Points
  plot <- plot + ggplot2::stat_summary(mapping,
                                       fun = stat_fun, geom = "point",
                                       size  = plottheme$cent$size,
                                       shape = plottheme$cent$shape,
                                       alpha = plottheme$cent$alpha)

  # Central Tendency Lines
  plot <- plot + ggplot2::stat_summary(mapping,
                                       fun = stat_fun, geom = "line",
                                       linewidth = le$linewidth,
                                       linetype  = le$linetype,
                                       alpha     = le$line_alpha)

  # Error Bars
  if (show_errorbars) {
    eb <- plottheme$errorbar

    if (cent == "mean_sdl") {
      plot <- plot + ggplot2::stat_summary(mapping,
                                           fun.data = "mean_sdl",
                                           fun.args = list(mult = 1), geom = "errorbar",
                                           linewidth = eb$linewidth,
                                           linetype = eb$linetype,
                                           alpha = eb$alpha,
                                           width = width)
    }

    if (cent == "mean_sdl_upper") {
      plot <- plot + ggplot2::stat_summary(mapping,
                                           fun.max = function(x){mean(x) + stats::sd(x)},
                                           fun.min = function(x){NA_real_},
                                           geom = "errorbar",
                                           linewidth = eb$linewidth,
                                           linetype = eb$linetype,
                                           alpha = eb$alpha,
                                           width = width) +
                    ggplot2::stat_summary(mapping,
                                         fun.max = function(x){mean(x) + stats::sd(x)},
                                         fun.min = function(x){mean(x)},
                                         geom = "linerange",
                                         linewidth = eb$linewidth,
                                         linetype = eb$linetype,
                                         alpha = eb$alpha,
                                         show.legend = FALSE)
    }

    if (cent == "median_iqr") {
      plot <- plot + ggplot2::stat_summary(mapping,
                                           fun.max = function(x){stats::quantile(x, 0.75)},
                                           fun.min = function(x){stats::quantile(x, 0.25)},
                                           geom = "errorbar",
                                           linewidth = eb$linewidth,
                                           linetype = eb$linetype,
                                           alpha = eb$alpha,
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
#' @param plottheme named list of theme aesthetics
#' @param color_aes optional string for color aesthetic (e.g., "OBS" for popgof legend)
#'
#' @return modified ggplot object
#' @keywords internal
add_obs_layers <- function(plot, obs_dv, grp_dv, grp_var_str, plottheme,
                           color_aes = NULL) {

  if (isTRUE(obs_dv)) {
    point_aes <- if (!is.null(color_aes)) ggplot2::aes(color = color_aes) else NULL
    plot <- plot + ggplot2::geom_point(
      mapping = point_aes,
      shape   = plottheme$obs$shape,
      size    = plottheme$obs$size,
      alpha   = plottheme$obs$alpha
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
      linewidth = plottheme$obs$linewidth,
      linetype  = plottheme$obs$linetype,
      alpha     = plottheme$obs$line_alpha
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
#' @param plottheme named list of theme aesthetics
#' @param show_legend logical, whether to add LLOQ to the linetype legend
#'
#' @return A named list with elements `plot` (modified ggplot) and `caption` (modified string)
#' @keywords internal
add_blq_layers <- function(plot, caption, loq_method, loq, dosenorm, plottheme,
                           show_legend = FALSE) {

  if (!loq_method %in% c(1, 2) || isTRUE(dosenorm)) {
    return(list(plot = plot, caption = caption))
  }

  if (isTRUE(show_legend)) {
    loq_lab <- paste0(loq)
    plot <- plot +
      ggplot2::geom_hline(ggplot2::aes(yintercept = loq, linetype = loq_lab),
                          linewidth = plottheme$ref$linewidth,
                          alpha = plottheme$ref$alpha) +
      ggplot2::scale_linetype_manual(
        name = "LLOQ",
        values = stats::setNames(c(plottheme$ref$linetype), loq_lab)) +
      ggplot2::guides(color = ggplot2::guide_legend(order = 1),
                      linetype = ggplot2::guide_legend(order = 2))
  } else {
    plot <- plot +
      ggplot2::geom_hline(yintercept = loq,
                          linewidth = plottheme$ref$linewidth,
                          linetype = plottheme$ref$linetype,
                          alpha = plottheme$ref$alpha)
  }

  blq_captions <- c(`1` = "Post-dose BLQ observations are imputed to 1/2 LLOQ",
                     `2` = "All BLQ observations are imputed to 1/2 LLOQ")
  caption <- paste0(caption, "\n", blq_captions[[as.character(loq_method)]])

  list(plot = plot, caption = caption)
}


prep_plot_env <- function(data, cent, log_y, obs_dv, grp_dv,
                          timeu, n_breaks, theme, theme_fn) {
  caption  <- caption_dvtime(cent, log_y, obs_dv, grp_dv)
  xbreaks  <- var_timebreaks(x = sort(unique(data$NTIME)), unit = timeu, n = n_breaks)
  plottheme <- merge_theme(theme, theme_fn())
  width    <- errorbar_width(plottheme, data)
  list(caption = caption, xbreaks = xbreaks, plottheme = plottheme, width = width)
}


errorbar_width <- function(plottheme, data) {
  if(is.numeric(plottheme$errorbar$width)) plottheme$errorbar$width
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
