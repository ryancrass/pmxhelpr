# Shared base theme constants for reference lines, error bars, and observations
.base_ref_theme <- list(
  linewidth_ref = 0.5,
  linetype_ref = 2,
  alpha_line_ref = 1
)

.base_errorbar_theme <- list(
  linewidth_errorbar = 0.75,
  linetype_errorbar = 1,
  alpha_errorbar = 1,
  width_errorbar = NULL
)

# Internal helper: build aes for central tendency layers
build_cent_aes <- function(y_var, color_aes = NULL) {
  if (is.null(color_aes)) {
    ggplot2::aes(x = NTIME, y = !!rlang::sym(y_var))
  } else {
    ggplot2::aes(x = NTIME, y = !!rlang::sym(y_var), color = !!color_aes)
  }
}

# Internal helper: add central tendency point, line, and errorbar layers to a plot
#
# @param plot ggplot object
# @param cent character, one of "mean", "mean_sdl", "mean_sdl_upper", "median", "median_iqr", "none"
# @param y_var character, the y variable name (e.g., "DV", "IPRED", "PRED")
# @param plottheme named list of theme aesthetics
# @param width numeric, errorbar cap width
# @param color_aes optional string for color aesthetic (e.g., "DV" for popgof legend)
# @param line_prefix character prefix for line theme keys ("cent" or "dv")
# @param show_errorbars logical, whether to add errorbar layers
add_cent_layers <- function(plot, cent, y_var, plottheme, width,
                            color_aes = NULL,
                            line_prefix = "cent",
                            show_errorbars = TRUE) {

  if (cent == "none") return(plot)

  mapping <- build_cent_aes(y_var, color_aes)

  # Determine stat function
  is_mean <- cent %in% c("mean", "mean_sdl", "mean_sdl_upper")
  stat_fun <- if (is_mean) "mean" else "median"

  # Line theme keys
  lw_key    <- paste0("linewidth_", line_prefix)
  lt_key    <- paste0("linetype_", line_prefix)
  alpha_key <- paste0("alpha_line_", line_prefix)

  # Central Tendency Points
  if (cent != "none") {
    plot <- plot + ggplot2::stat_summary(mapping,
                                         fun = stat_fun, geom = "point",
                                         size = plottheme$size_point_cent,
                                         shape = plottheme$shape_point_cent,
                                         alpha = plottheme$alpha_point_cent)
  }

  # Central Tendency Lines
  plot <- plot + ggplot2::stat_summary(mapping,
                                       fun = stat_fun, geom = "line",
                                       linewidth = plottheme[[lw_key]],
                                       linetype = plottheme[[lt_key]],
                                       alpha = plottheme[[alpha_key]])

  # Error Bars
  if (show_errorbars) {
    if (cent == "mean_sdl") {
      plot <- plot + ggplot2::stat_summary(mapping,
                                           fun.data = "mean_sdl",
                                           fun.args = list(mult = 1), geom = "errorbar",
                                           linewidth = plottheme$linewidth_errorbar,
                                           linetype = plottheme$linetype_errorbar,
                                           alpha = plottheme$alpha_errorbar,
                                           width = width)
    }

    if (cent == "mean_sdl_upper") {
      plot <- plot + ggplot2::stat_summary(mapping,
                                           fun.max = function(x){mean(x) + stats::sd(x)},
                                           fun.min = function(x){NA_real_},
                                           geom = "errorbar",
                                           linewidth = plottheme$linewidth_errorbar,
                                           linetype = plottheme$linetype_errorbar,
                                           alpha = plottheme$alpha_errorbar,
                                           width = width) +
                    ggplot2::stat_summary(mapping,
                                         fun.max = function(x){mean(x) + stats::sd(x)},
                                         fun.min = function(x){mean(x)},
                                         geom = "linerange",
                                         linewidth = plottheme$linewidth_errorbar,
                                         linetype = plottheme$linetype_errorbar,
                                         alpha = plottheme$alpha_errorbar,
                                         show.legend = FALSE)
    }

    if (cent == "median_iqr") {
      plot <- plot + ggplot2::stat_summary(mapping,
                                           fun.max = function(x){stats::quantile(x, 0.75)},
                                           fun.min = function(x){stats::quantile(x, 0.25)},
                                           geom = "errorbar",
                                           linewidth = plottheme$linewidth_errorbar,
                                           linetype = plottheme$linetype_errorbar,
                                           alpha = plottheme$alpha_errorbar,
                                           width = width)
    }
  }

  return(plot)
}
