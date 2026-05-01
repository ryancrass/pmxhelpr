#' Build a VPC ggplot from summary statistics
#'
#' @description
#' `vpc_build_plot()` constructs a ggplot2 VPC plot from pre-computed quantile
#'    summary statistics.
#'
#' @param vpcstats Data.frame of simulated quantile statistics from `df_vpcstats()`.
#' @param bin_var String. Binning variable name. Default is `"BIN_MID"`.
#' @param strat_var_str String or `NULL`. Stratification variable name.
#' @param shown Named list of logicals specifying which layers to include.
#' @param theme Named list of aesthetic parameters (colors, sizes, etc.).
#' @param loq Numeric value for LLOQ reference line or `NULL`.
#'
#' @return A ggplot2 object.
#' @keywords internal

vpc_build_plot <- function(vpcstats,
                     bin_var = "BIN_MID",
                     strat_var_str = NULL,
                     shown = NULL,
                     theme = NULL,
                     loq = NULL) {

  ##Set vpc aesthetics and theme
  shown <- merge_element(shown, plot_vpc_shown())
  vpctheme <- merge_theme(theme, plot_vpc_theme())

  ###Generate Base Plot
  plot <- ggplot2::ggplot(vpcstats, ggplot2::aes(x = .data[[bin_var]]))

  ## Simulated prediction interval as area
  if (isTRUE(shown$pi_as_area)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q5_med, ymax = q95_med),
        fill = vpctheme$sim_pi$fill,
        alpha = vpctheme$sim_pi$alpha
      )
  }

  ## Simulated prediction interval CI ribbons
  if (isTRUE(shown$pi_ci)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q5_low, ymax = q5_hi),
        fill = vpctheme$sim_pi$fill,
        alpha = vpctheme$sim_pi$alpha
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q95_low, ymax = q95_hi),
        fill = vpctheme$sim_pi$fill,
        alpha = vpctheme$sim_pi$alpha
      )
  }

  ## Simulated prediction interval lines
  if (isTRUE(shown$pi)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(y = q5_med),
        color = vpctheme$sim_pi$color,
        linetype = vpctheme$sim_pi$linetype,
        linewidth = vpctheme$sim_pi$linewidth
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = q95_med),
        color = vpctheme$sim_pi$color,
        linetype = vpctheme$sim_pi$linetype,
        linewidth = vpctheme$sim_pi$linewidth
      )
  }

  ## Simulated median CI ribbon
  if (isTRUE(shown$sim_median_ci)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q50_low, ymax = q50_hi),
        fill = vpctheme$sim_median$fill,
        alpha = vpctheme$sim_median$alpha
      )
  }

  ## Simulated median line
  if (isTRUE(shown$sim_median)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(y = q50_med),
        color = vpctheme$sim_median$color,
        linetype = vpctheme$sim_median$linetype,
        linewidth = vpctheme$sim_median$linewidth
      )
  }

  ## Observed median line
  if (isTRUE(shown$obs_median)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var]], y = obs50),
        inherit.aes = FALSE,
        color = vpctheme$obs_median$color,
        linetype = vpctheme$obs_median$linetype,
        linewidth = vpctheme$obs_median$linewidth
      )
  }

  ## Observed CI lines (lower and upper quantiles)
  if (isTRUE(shown$obs_ci)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var]], y = obs5),
        inherit.aes = FALSE,
        color = vpctheme$obs_ci$color,
        linetype = vpctheme$obs_ci$linetype,
        linewidth = vpctheme$obs_ci$linewidth
      ) +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var]], y = obs95),
        inherit.aes = FALSE,
        color = vpctheme$obs_ci$color,
        linetype = vpctheme$obs_ci$linetype,
        linewidth = vpctheme$obs_ci$linewidth
      )
  }

  ## LOQ reference line
  if (!is.null(loq)) {
    plot <- plot +
      ggplot2::geom_hline(
        yintercept = loq,
        color = vpctheme$loq_line$color,
        linetype = vpctheme$loq_line$linetype,
        linewidth = vpctheme$loq_line$linewidth
      )
  }

  ## Faceting by stratification variable
  if (!is.null(strat_var_str)) {
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(.data[[strat_var_str]]))
  }

  return(plot)
}
