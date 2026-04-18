#' Build a VPC ggplot from summary statistics
#'
#' @description
#' `plot_vpc()` constructs a ggplot2 VPC plot from pre-computed quantile
#'    summary statistics.
#'
#' @param sim_quant Data.frame of simulated quantile statistics from `df_vpcstats()`.
#' @param obs_quant Data.frame of observed quantile statistics from `df_vpcstats()`.
#' @param strat_var Character string of stratification variable name, or `NULL`.
#' @param show Named list of logicals specifying which layers to include.
#' @param vpc_theme Named list of aesthetic parameters (colors, sizes, etc.).
#' @param lloq Numeric value for LLOQ reference line, or `NULL`.
#'
#' @return A ggplot2 object.
#' @keywords internal

plot_vpc <- function(sim_quant,
                     obs_quant,
                     strat_var = NULL,
                     show,
                     vpc_theme,
                     lloq = NULL) {

  plot <- ggplot2::ggplot(sim_quant, ggplot2::aes(x = NTIME))

  ## Simulated prediction interval as area
  if (isTRUE(show$pi_as_area)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q5_med, ymax = q95_med),
        fill = vpc_theme$sim_pi_fill,
        alpha = vpc_theme$sim_pi_alpha
      )
  }

  ## Simulated prediction interval CI ribbons
  if (isTRUE(show$pi_ci)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q5_low, ymax = q5_hi),
        fill = vpc_theme$sim_pi_fill,
        alpha = vpc_theme$sim_pi_alpha
      ) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q95_low, ymax = q95_hi),
        fill = vpc_theme$sim_pi_fill,
        alpha = vpc_theme$sim_pi_alpha
      )
  }

  ## Simulated prediction interval lines
  if (isTRUE(show$pi)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(y = q5_med),
        color = vpc_theme$sim_pi_color,
        linetype = vpc_theme$sim_pi_linetype,
        linewidth = vpc_theme$sim_pi_size
      ) +
      ggplot2::geom_line(
        ggplot2::aes(y = q95_med),
        color = vpc_theme$sim_pi_color,
        linetype = vpc_theme$sim_pi_linetype,
        linewidth = vpc_theme$sim_pi_size
      )
  }

  ## Simulated median CI ribbon
  if (isTRUE(show$sim_median_ci)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q50_low, ymax = q50_hi),
        fill = vpc_theme$sim_median_fill,
        alpha = vpc_theme$sim_median_alpha
      )
  }

  ## Simulated median line
  if (isTRUE(show$sim_median)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(y = q50_med),
        color = vpc_theme$sim_median_color,
        linetype = vpc_theme$sim_median_linetype,
        linewidth = vpc_theme$sim_median_size
      )
  }

  ## Observed median line
  if (isTRUE(show$obs_median)) {
    plot <- plot +
      ggplot2::geom_line(
        data = obs_quant,
        ggplot2::aes(x = NTIME, y = obs50),
        inherit.aes = FALSE,
        color = vpc_theme$obs_median_color,
        linetype = vpc_theme$obs_median_linetype,
        linewidth = vpc_theme$obs_median_size
      )
  }

  ## Observed CI lines (lower and upper quantiles)
  if (isTRUE(show$obs_ci)) {
    plot <- plot +
      ggplot2::geom_line(
        data = obs_quant,
        ggplot2::aes(x = NTIME, y = obs5),
        inherit.aes = FALSE,
        color = vpc_theme$obs_ci_color,
        linetype = vpc_theme$obs_ci_linetype,
        linewidth = vpc_theme$obs_ci_size
      ) +
      ggplot2::geom_line(
        data = obs_quant,
        ggplot2::aes(x = NTIME, y = obs95),
        inherit.aes = FALSE,
        color = vpc_theme$obs_ci_color,
        linetype = vpc_theme$obs_ci_linetype,
        linewidth = vpc_theme$obs_ci_size
      )
  }

  ## LOQ reference line
  if (!is.null(lloq)) {
    plot <- plot +
      ggplot2::geom_hline(
        yintercept = lloq,
        color = vpc_theme$loq_color,
        linetype = "dashed"
      )
  }

  ## Faceting by stratification variable
  if (!is.null(strat_var)) {
    plot <- plot +
      ggplot2::facet_wrap(stats::as.formula(paste("~", strat_var)))
  }

  plot
}
