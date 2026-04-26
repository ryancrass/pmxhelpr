#' Build a VPC ggplot from summary statistics
#'
#' @description
#' `plot_vpc()` constructs a ggplot2 VPC plot from pre-computed quantile
#'    summary statistics.
#'
#' @param vpcstats Data.frame of simulated quantile statistics from `df_vpcstats()`.
#' @param bin_var Binning variable name. Default is `NTIME`. Accepts bare names or strings.
#' @param strat_var Stratification variable name, or `NULL`. Accepts bare names or strings.
#' @param shown Named list of logicals specifying which layers to include.
#' @param vpc_theme Named list of aesthetic parameters (colors, sizes, etc.).
#' @param loq Numeric value for LLOQ reference line or `NULL`.
#'
#' @return A ggplot2 object.
#' @keywords internal

plot_vpc <- function(vpcstats,
                     bin_var = NTIME,
                     strat_var = NULL,
                     shown = NULL,
                     vpc_theme = NULL,
                     loq = NULL) {

  bin_var   <- rlang::as_name(rlang::ensym(bin_var))
  strat_var <- capture_col(rlang::enquo(strat_var))

  ##Set vpc aesthetics and theme
  shown <- list_update(shown, plot_vpc_shown())
  vpc_theme <- list_update(vpc_theme, plot_vpc_theme())

  plot <- ggplot2::ggplot(vpcstats, ggplot2::aes(x = .data[[bin_var]]))

  ## Simulated prediction interval as area
  if (isTRUE(shown$pi_as_area)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q5_med, ymax = q95_med),
        fill = vpc_theme$sim_pi_fill,
        alpha = vpc_theme$sim_pi_alpha
      )
  }

  ## Simulated prediction interval CI ribbons
  if (isTRUE(shown$pi_ci)) {
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
  if (isTRUE(shown$pi)) {
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
  if (isTRUE(shown$sim_median_ci)) {
    plot <- plot +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = q50_low, ymax = q50_hi),
        fill = vpc_theme$sim_median_fill,
        alpha = vpc_theme$sim_median_alpha
      )
  }

  ## Simulated median line
  if (isTRUE(shown$sim_median)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(y = q50_med),
        color = vpc_theme$sim_median_color,
        linetype = vpc_theme$sim_median_linetype,
        linewidth = vpc_theme$sim_median_size
      )
  }

  ## Observed median line
  if (isTRUE(shown$obs_median)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var]], y = obs50),
        inherit.aes = FALSE,
        color = vpc_theme$obs_median_color,
        linetype = vpc_theme$obs_median_linetype,
        linewidth = vpc_theme$obs_median_size
      )
  }

  ## Observed CI lines (lower and upper quantiles)
  if (isTRUE(shown$obs_ci)) {
    plot <- plot +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var]], y = obs5),
        inherit.aes = FALSE,
        color = vpc_theme$obs_ci_color,
        linetype = vpc_theme$obs_ci_linetype,
        linewidth = vpc_theme$obs_ci_size
      ) +
      ggplot2::geom_line(
        ggplot2::aes(x = .data[[bin_var]], y = obs95),
        inherit.aes = FALSE,
        color = vpc_theme$obs_ci_color,
        linetype = vpc_theme$obs_ci_linetype,
        linewidth = vpc_theme$obs_ci_size
      )
  }

  ## LOQ reference line
  if (!is.null(loq)) {
    plot <- plot +
      ggplot2::geom_hline(
        yintercept = loq,
        color = vpc_theme$loq_color,
        linetype = "dashed"
      )
  }

  ## Faceting by stratification variable
  if (!is.null(strat_var)) {
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(.data[[strat_var]]))
  }

  return(plot)
}
