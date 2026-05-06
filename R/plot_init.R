
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
# merge_element (R/theme_internals.R), pmx_* element constructors,
# build_layer, apply_panel_theme.
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
