#' Build a VPC ggplot from summary statistics
#'
#' @description
#' `plot_vpc()` constructs a ggplot2 VPC plot from pre-computed quantile
#'    summary statistics.
#'
#' @param vpcstats Data.frame of simulated quantile statistics from `df_vpcstats()`.
#' @param bin_var_str String. Binning variable name. Default is `"NTIME"`.
#' @param strat_var_str String or `NULL`. Stratification variable name.
#' @param shown Named list of logicals specifying which layers to include.
#' @param vpc_theme Named list of aesthetic parameters (colors, sizes, etc.).
#' @param loq Numeric value for LLOQ reference line or `NULL`.
#'
#' @return A ggplot2 object.
#' @keywords internal

plot_vpc <- function(vpcstats,
                     bin_var_str = "NTIME",
                     strat_var_str = NULL,
                     shown = NULL,
                     vpc_theme = NULL,
                     loq = NULL) {

  ##Set vpc aesthetics and theme
  shown <- list_update(shown, plot_vpc_shown())
  vpc_theme <- merge_theme(vpc_theme, plot_vpc_theme())

  ###Generate Base Plot
  plot <- ggplot2::ggplot(vpcstats, ggplot2::aes(x = .data[[bin_var_str]]))

  ## Simulated prediction interval as area
  if (isTRUE(shown$pi_as_area)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q5_med, ymax = q95_med),
        fill = vpc_theme$sim_pi$fill,
        alpha = vpc_theme$sim_pi$alpha
      )
  }

  ## Simulated prediction interval CI ribbons
  if (isTRUE(shown$pi_ci)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q5_low, ymax = q5_hi),
        fill = vpc_theme$sim_pi$fill,
        alpha = vpc_theme$sim_pi$alpha
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q95_low, ymax = q95_hi),
        fill = vpc_theme$sim_pi$fill,
        alpha = vpc_theme$sim_pi$alpha
      )
  }

  ## Simulated prediction interval lines
  if (isTRUE(shown$pi)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(y = q5_med),
        color = vpc_theme$sim_pi$color,
        linetype = vpc_theme$sim_pi$linetype,
        linewidth = vpc_theme$sim_pi$size
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = q95_med),
        color = vpc_theme$sim_pi$color,
        linetype = vpc_theme$sim_pi$linetype,
        linewidth = vpc_theme$sim_pi$size
      )
  }

  ## Simulated median CI ribbon
  if (isTRUE(shown$sim_median_ci)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q50_low, ymax = q50_hi),
        fill = vpc_theme$sim_median$fill,
        alpha = vpc_theme$sim_median$alpha
      )
  }

  ## Simulated median line
  if (isTRUE(shown$sim_median)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(y = q50_med),
        color = vpc_theme$sim_median$color,
        linetype = vpc_theme$sim_median$linetype,
        linewidth = vpc_theme$sim_median$size
      )
  }

  ## Observed median line
  if (isTRUE(shown$obs_median)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var_str]], y = obs50),
        inherit.aes = FALSE,
        color = vpc_theme$obs_median$color,
        linetype = vpc_theme$obs_median$linetype,
        linewidth = vpc_theme$obs_median$size
      )
  }

  ## Observed CI lines (lower and upper quantiles)
  if (isTRUE(shown$obs_ci)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var_str]], y = obs5),
        inherit.aes = FALSE,
        color = vpc_theme$obs_ci$color,
        linetype = vpc_theme$obs_ci$linetype,
        linewidth = vpc_theme$obs_ci$size
      ) +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var_str]], y = obs95),
        inherit.aes = FALSE,
        color = vpc_theme$obs_ci$color,
        linetype = vpc_theme$obs_ci$linetype,
        linewidth = vpc_theme$obs_ci$size
      )
  }

  ## LOQ reference line
  if (!is.null(loq)) {
    plot <- plot +
      ggplot2::geom_hline(
        yintercept = loq,
        color = vpc_theme$loq$color,
        linetype = vpc_theme$loq$linetype
      )
  }

  ## Faceting by stratification variable
  if (!is.null(strat_var_str)) {
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(.data[[strat_var_str]]))
  }

  return(plot)
}
