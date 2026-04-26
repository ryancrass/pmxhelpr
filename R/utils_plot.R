
# Full default theme lists for each plot type
.dvtime_defaults <- list(
  linewidth_ref = 0.5,
  linetype_ref = 2,
  alpha_line_ref = 1,
  shape_point_obs = 1,
  size_point_obs = 0.75,
  alpha_point_obs = 0.5,
  linewidth_obs = 0.5,
  linetype_obs = 1,
  alpha_line_obs = 0.5,
  shape_point_cent = 16,
  size_point_cent = 1.25,
  alpha_point_cent = 1,
  linewidth_cent = 0.75,
  linetype_cent = 1,
  alpha_line_cent = 1,
  linewidth_errorbar = 0.75,
  linetype_errorbar = 1,
  alpha_errorbar = 1,
  width_errorbar = NULL
)

.popgof_defaults <- list(
  linewidth_ref = 0.5,
  linetype_ref = 2,
  alpha_line_ref = 1,
  shape_point_obs = 1,
  size_point_obs = 0.75,
  alpha_point_obs = 0.5,
  linewidth_obs = 0.5,
  linetype_obs = 1,
  alpha_line_obs = 0.75,
  linewidth_dv = 1,
  linetype_dv = 1,
  alpha_line_dv = 1,
  shape_point_cent = 1,
  size_point_cent = 1.25,
  alpha_point_cent = 1,
  linewidth_cent = 0.75,
  linetype_cent = 1,
  alpha_line_cent = 1,
  linewidth_errorbar = 0.75,
  linetype_errorbar = 1,
  alpha_errorbar = 1,
  width_errorbar = NULL
)

.dvconc_defaults <- list(
  linewidth_ref = 0.5,
  linetype_ref = 2,
  alpha_line_ref = 1,
  shape_point_obs = 1,
  size_point_obs = 1.25,
  alpha_point_obs = 0.5,
  linewidth_loess = 1,
  linetype_loess = 1,
  linewidth_linear = 1,
  linetype_linear = 2,
  color_loess = "black",
  color_linear = "black",
  color_se_loess = "lightgrey",
  color_se_linear = "lightgrey",
  alpha_se_loess = 0.4,
  alpha_se_linear = 0.4
)

.vpc_defaults <- list(
  obs_color = "#0000FF",
  obs_size = 1,
  obs_shape = 1,
  obs_alpha = 0.7,
  obs_median_color = "#FF0000",
  obs_median_linetype = "solid",
  obs_median_size = 1,
  obs_ci_color = "#0000FF",
  obs_ci_linetype = "dashed",
  obs_ci_size = 0.5,
  sim_pi_fill = "#0000FF",
  sim_pi_alpha = 0.15,
  sim_pi_color = "#000000",
  sim_pi_linetype = "dotted",
  sim_pi_size = 1,
  sim_median_fill = "#FF0000",
  sim_median_alpha = 0.3,
  sim_median_color = "#000000",
  sim_median_linetype = "dashed",
  sim_median_size = 1,
  bin_separators_color = "#000000",
  loq_color = "#990000",
  loq_linetype = "dashed"
)

.vpc_shown_defaults <- list(
  obs_dv = TRUE,
  obs_ci = TRUE,
  pi = FALSE,
  pi_as_area = FALSE,
  pi_ci = TRUE,
  obs_median = TRUE,
  sim_median = FALSE,
  sim_median_ci = TRUE)


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


# Internal helper: vectorized dose normalization
var_dosenorm <- function(dv_var, dose_var) {
  dv_var / dose_var
}

# Internal helper: vectorized prediction correction
  # Applies the standard PC-VPC formula to numeric vectors.
  # Assumes the vectors are already scoped to a single bin/group.
var_pc <- function(dv_var, pred_var, lower_bound = 0) {
  predbin <- stats::median(pred_var)
  lower_bound + (dv_var - lower_bound) * ((predbin - lower_bound) / (pred_var - lower_bound))
}
