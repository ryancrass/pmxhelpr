
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
      shape   = plottheme$shape_point_obs,
      size    = plottheme$size_point_obs,
      alpha   = plottheme$alpha_point_obs
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
      linewidth = plottheme$linewidth_obs,
      linetype  = plottheme$linetype_obs,
      alpha     = plottheme$alpha_line_obs
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
                          linewidth = plottheme$linewidth_ref,
                          alpha = plottheme$alpha_line_ref) +
      ggplot2::scale_linetype_manual(
        name = "LLOQ",
        values = stats::setNames(c(plottheme$linetype_ref), loq_lab)) +
      ggplot2::guides(color = ggplot2::guide_legend(order = 1),
                      linetype = ggplot2::guide_legend(order = 2))
  } else {
    plot <- plot +
      ggplot2::geom_hline(yintercept = loq,
                          linewidth = plottheme$linewidth_ref,
                          linetype = plottheme$linetype_ref,
                          alpha = plottheme$alpha_line_ref)
  }

  blq_captions <- c(`1` = "Post-dose BLQ observations are imputed to 1/2 LLOQ",
                     `2` = "All BLQ observations are imputed to 1/2 LLOQ")
  caption <- paste0(caption, "\n", blq_captions[[as.character(loq_method)]])

  list(plot = plot, caption = caption)
}



#' Determine left-censoring for quantiles at the lower limit of quantification
#'
#' @param x Vector containing the variable to be censored
#' @param p Quantile for computation
#' @param loq Numeric value of the lower limit of quantification (LLOQ) for the assay
#'
#' @return Replaces values below loq (including NA) with -Inf, then computes
#   the quantile. Returns NA if the result is -Inf.
#' @keywords internal
#' @examples
#' data <- data_sad |>
#'   dplyr::group_by(CMT, NTIME, DOSE) |>
#'   dplyr::summarize(P05 = pmxhelpr:::var_loqcens(ODV, 0.05, loq = LLOQ),
#'                    P50 = pmxhelpr:::var_loqcens(ODV, 0.5, loq = LLOQ),
#'                    P95 = pmxhelpr:::var_loqcens(ODV, 0.05, loq = LLOQ), .groups = "drop")

var_loqcens <- function(x, p, loq) {
  x[is.na(x)] <- -Inf
  x[x < loq] <- -Inf
  q <- stats::quantile(x, probs = p, na.rm = TRUE)
  if (is.infinite(q) && q < 0) NA_real_ else as.numeric(q)
}





#' Determine dose-normalization
#'
#' @param dv_var Vector containing the dependent variable (DV)
#' @param dose_var Vector containing dose
#'
#' @return A numeric vector of dose-normalized values of `dv_var`
#' @keywords internal
#' @examples
#' data <- dplyr::mutate(data_sad, DNDV = pmxhelpr:::var_dosenorm(ODV, DOSE))

var_dosenorm <- function(dv_var, dose_var) {
  dv_var / dose_var
}





#' Determine prediction correction
#'
#' @param dv_var Vector containing the dependent variable (DV)
#' @param pred_var Vector containing population predictions (PRED)
#' @param lower_bound Lower bound for prediction correction formula.
#'
#' @return A numeric vector of prediction-corrected values of `dv_var`
#' @keywords internal
#' @examples
#' pkmodel <- model_mread_load(model = "pkmodel")
#' data <- df_addpred(data = dplyr::filter(data_sad, CMT != 3), model = pkmodel)
#' data <- dplyr::mutate(data, PCDV = pmxhelpr:::var_pc(ODV, PRED))
#'
var_pc <- function(dv_var, pred_var, lower_bound = 0) {
  predbin <- stats::median(pred_var)
  lower_bound + (dv_var - lower_bound) * ((predbin - lower_bound) / (pred_var - lower_bound))
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
