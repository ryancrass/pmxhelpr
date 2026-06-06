#' Concentration-time plot theme
#'
#' Constructor and factory for `plot_dvtime` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#' Use role-level shortcuts `obs` and `cent` with [pmx_style()] to set shared
#' aesthetics (e.g., color, alpha) for both point and line elements at once.
#'
#' @param obs_point Observed data point aesthetics. See [pmx_point()].
#' @param obs_line Observed data line aesthetics (spaghetti). See [pmx_line()].
#' @param cent_point Central tendency point aesthetics. See [pmx_point()].
#' @param cent_line Central tendency line aesthetics. See [pmx_line()].
#' @param cent_errorbar Central tendency error bar aesthetics. See [pmx_errorbar()].
#' @param ref_line Reference line aesthetics (e.g., change-from-baseline). See [pmx_line()].
#' @param loq_line LOQ reference line aesthetics. See [pmx_line()].
#' @param obs Shortcut: apply shared aesthetics to both `obs_point` and `obs_line`.
#'   See [pmx_style()].
#' @param cent Shortcut: apply shared aesthetics to both `cent_point` and `cent_line`.
#'   See [pmx_style()].
#'
#' @family exploratory analysis
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_dvtime_theme()
#' plot_dvtime_theme(obs_point = pmx_point(size = 2), ref_line = pmx_line(linetype = 3))
#' plot_dvtime_theme(obs = pmx_style(alpha = 0.3))
plot_dvtime_theme <- function(obs_point = NULL, obs_line = NULL,
                              cent_point = NULL, cent_line = NULL,
                              cent_errorbar = NULL, ref_line = NULL, loq_line = NULL,
                              obs = NULL, cent = NULL) {
  defaults <- list(
    obs_point     = pmx_point(shape = 1, size = 0.75, alpha = 0.5),
    obs_line      = pmx_line(linewidth = 0.5, linetype = 1, alpha = 0.5),
    cent_point    = pmx_point(shape = 16, size = 1.25, alpha = 0),
    cent_line     = pmx_line(linewidth = 0.75, linetype = 1, alpha = 1),
    cent_errorbar = pmx_errorbar(linewidth = 0.75, linetype = 1, alpha = 1),
    ref_line      = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1),
    loq_line      = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1)
  )
  user <- compact(list(
    obs_point = obs_point, obs_line = obs_line,
    cent_point = cent_point, cent_line = cent_line,
    cent_errorbar = cent_errorbar, ref_line = ref_line, loq_line = loq_line,
    obs = obs, cent = cent
  ))
  pmx_theme(merge_theme(user, defaults), subclass = "plot_dvtime_theme")
}


#' Population overlay GOF plot theme
#'
#' Constructor and factory for `plot_gof` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#' Use role-level shortcuts `obs` and `cent` with [pmx_style()] to set shared
#' aesthetics for both point and line elements at once.
#' Override overlay colors with `cent_color = pmx_color()`.
#'
#' @param obs_point Observed data point aesthetics. See [pmx_point()].
#' @param obs_line Observed data line aesthetics. See [pmx_line()].
#' @param cent_point Shared central tendency point aesthetics for DV, PRED, and IPRED.
#'   See [pmx_point()].
#' @param cent_line Shared central tendency line aesthetics for DV, PRED, and IPRED.
#'   See [pmx_line()].
#' @param cent_errorbar Central tendency error bar aesthetics. See [pmx_errorbar()].
#' @param ref_line Reference line aesthetics. See [pmx_line()].
#' @param loq_line LOQ reference line aesthetics. See [pmx_line()].
#' @param obs Shortcut: apply shared aesthetics to both `obs_point` and `obs_line`.
#'   See [pmx_style()].
#' @param cent Shortcut: apply shared aesthetics to both `cent_point` and `cent_line`.
#'   See [pmx_style()].
#' @param cent_color Overlay color mapping for DV, PRED, and IPRED. See [pmx_color()].
#'
#' @family goodness-of-fit
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_gof_theme()
#' plot_gof_theme(cent_color = pmx_color(pred = "purple"))
#' plot_gof_theme(cent = pmx_style(alpha = 0.8))
plot_gof_theme <- function(obs_point = NULL, obs_line = NULL,
                           cent_point = NULL, cent_line = NULL,
                           cent_errorbar = NULL, ref_line = NULL, loq_line = NULL,
                           obs = NULL, cent = NULL, cent_color = NULL) {
  defaults <- list(
    obs_point     = pmx_point(shape = 1, size = 0.75, alpha = 0.5, color = "darkgrey"),
    obs_line      = pmx_line(linewidth = 0.5, linetype = 1, alpha = 0.75, color = "darkgrey"),
    cent_point    = pmx_point(shape = 16, size = 1.25, alpha = 0),
    cent_line     = pmx_line(linewidth = 0.75, linetype = 1, alpha = 1),
    cent_errorbar = pmx_errorbar(linewidth = 0.75, linetype = 1, alpha = 1),
    ref_line      = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1),
    loq_line      = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1),
    cent_color    = pmx_color(dv = "blue", pred = "red", ipred = "green")
  )
  user <- compact(list(
    obs_point = obs_point, obs_line = obs_line,
    cent_point = cent_point, cent_line = cent_line,
    cent_errorbar = cent_errorbar, ref_line = ref_line, loq_line = loq_line,
    obs = obs, cent = cent, cent_color = cent_color
  ))
  pmx_theme(merge_theme(user, defaults), subclass = "plot_gof_theme")
}


#' Response versus concentration plot theme
#'
#' Constructor and factory for `plot_dvconc` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#'
#' @param obs_point Observed data point aesthetics. See [pmx_point()].
#' @param ref_line Reference line aesthetics. See [pmx_line()].
#' @param loess LOESS trend line aesthetics. See [pmx_trend()].
#' @param linear Linear trend line aesthetics. See [pmx_trend()].
#' @param obs Shortcut: apply shared aesthetics to `obs_point`. See [pmx_style()].
#'
#' @family exploratory analysis
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_dvconc_theme()
#' plot_dvconc_theme(loess = pmx_trend(color = "red"))
#' plot_dvconc_theme(obs = pmx_style(alpha = 0.3))
plot_dvconc_theme <- function(obs_point = NULL, ref_line = NULL, loess = NULL,
                              linear = NULL, obs = NULL) {
  defaults <- list(
    obs_point = pmx_point(shape = 1, size = 1.25, alpha = 0.5),
    ref_line = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1),
    loess  = pmx_trend(linewidth = 1, linetype = 1,
                       color = "black", se_color = "lightgrey", se_alpha = 0.4),
    linear = pmx_trend(linewidth = 1, linetype = 2,
                       color = "black", se_color = "lightgrey", se_alpha = 0.4)
  )
  user <- compact(list(obs_point = obs_point, ref_line = ref_line,
                       loess = loess, linear = linear, obs = obs))
  pmx_theme(merge_theme(user, defaults), subclass = "plot_dvconc_theme")
}


#' Dose-proportionality plot theme
#'
#' Constructor and factory for `plot_doseprop` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#'
#' @param obs_point Observed data point aesthetics. See [pmx_point()].
#' @param linear Linear regression line + SE ribbon aesthetics. See [pmx_trend()].
#' @param obs Shortcut: apply shared aesthetics to `obs_point`. See [pmx_style()].
#'
#' @family dose proportionality
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_doseprop_theme()
#' plot_doseprop_theme(linear = pmx_trend(color = "red"))
#' plot_doseprop_theme(obs = pmx_style(alpha = 0.3))
plot_doseprop_theme <- function(obs_point = NULL, linear = NULL, obs = NULL) {
  defaults <- list(
    obs_point = pmx_point(shape = 1, size = 2, alpha = 0.7),
    linear    = pmx_trend(linewidth = 1, linetype = 1,
                          color = "black",
                          se_color = "lightgrey", se_alpha = 0.4)
  )
  user <- compact(list(obs_point = obs_point, linear = linear, obs = obs))
  pmx_theme(merge_theme(user, defaults), subclass = "plot_doseprop_theme")
}


#' Forest plot theme
#'
#' Constructor and factory for `plot_forest` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#'
#' @param point Point aesthetics for the central estimate. See [pmx_point()].
#' @param errorbar Confidence-interval line aesthetics (rendered as a horizontal
#'   `geom_linerange`; the element's `width` field is unused). See [pmx_errorbar()].
#' @param ref_line Vertical no-effect reference line aesthetics. See [pmx_line()].
#' @param ref_band Shaded equivalence-interval ribbon aesthetics. See [pmx_ribbon()].
#' @param panel_color Optional per-covariate color mapping. See [pmx_color()].
#'   When supplied with at least one named entry, point and errorbar layers
#'   are colored by covariate (facet); the legend is suppressed since facet
#'   strips already label each covariate. Default is an empty [pmx_color()]
#'   (no per-covariate coloring), which keeps the single-color behavior from
#'   `point$color` / `errorbar$color`. Covariates absent from the palette
#'   render in grey50 and trigger a warning.
#'
#' @family forest plot
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_forest_theme()
#' plot_forest_theme(point = pmx_point(shape = 18, size = 3))
#' plot_forest_theme(ref_band = pmx_ribbon(fill = "skyblue", alpha = 0.2))
#' plot_forest_theme(panel_color = pmx_color(
#'   FOOD = "firebrick", WTBL = "steelblue", Reference = "grey20"
#' ))
plot_forest_theme <- function(point = NULL, errorbar = NULL,
                              ref_line = NULL, ref_band = NULL,
                              panel_color = NULL) {
  defaults <- list(
    point       = pmx_point(shape = 16, size = 2.5, alpha = 1, color = "black"),
    errorbar    = pmx_errorbar(linewidth = 0.7, linetype = 1, alpha = 1, color = "black"),
    ref_line    = pmx_line(linewidth = 0.5, linetype = 2, alpha = 1, color = "grey40"),
    ref_band    = pmx_ribbon(fill = "grey80", alpha = 0.3, color = NA),
    panel_color = pmx_color()
  )
  user <- compact(list(point = point, errorbar = errorbar,
                       ref_line = ref_line, ref_band = ref_band,
                       panel_color = panel_color))
  pmx_theme(merge_theme(user, defaults), subclass = "plot_forest_theme")
}


#' VPC plot theme
#'
#' Constructor and factory for `plot_vpc_cont` plot aesthetics.
#' Call with no arguments to view defaults. Pass element overrides to customize.
#' Each theme key maps 1:1 with a [plot_vpc_shown()] visibility toggle.
#'
#' @param obs_point Observed data point aesthetics. See [pmx_point()].
#' @param obs_median_line Observed median line aesthetics. See [pmx_line()].
#' @param obs_pi_line Observed quantile line aesthetics. See [pmx_line()].
#' @param sim_pi_line Simulated prediction interval line aesthetics. See [pmx_line()].
#' @param sim_pi_ci Simulated prediction interval CI ribbon aesthetics. See [pmx_ribbon()].
#' @param sim_pi_area Simulated prediction interval area ribbon aesthetics. See [pmx_ribbon()].
#' @param sim_median_line Simulated median line aesthetics. See [pmx_line()].
#' @param sim_median_ci Simulated median CI ribbon aesthetics. See [pmx_ribbon()].
#' @param loq_line LOQ reference line aesthetics. See [pmx_line()].
#'
#' @family vpc
#' @return A named list of theme elements
#' @export
#'
#' @examples
#' plot_vpc_theme()
#' plot_vpc_theme(obs_point = pmx_point(color = "#000000"))
plot_vpc_theme <- function(obs_point = NULL, obs_median_line = NULL, obs_pi_line = NULL,
                           sim_pi_line = NULL, sim_pi_ci = NULL, sim_pi_area = NULL,
                           sim_median_line = NULL, sim_median_ci = NULL,
                           loq_line = NULL) {
  defaults <- list(
    obs_point      = pmx_point(color = "#0000FF", size = 1, shape = 1, alpha = 0.7),
    obs_median_line    = pmx_line(color = "#FF0000", linetype = "solid", linewidth = 1),
    obs_pi_line     = pmx_line(color = "#0000FF", linetype = "dashed", linewidth = 0.5),
    sim_pi_line     = pmx_line(color = "#000000", linetype = "dotted", linewidth = 1),
    sim_pi_ci       = pmx_ribbon(fill = "#0000FF", alpha = 0.15),
    sim_pi_area     = pmx_ribbon(fill = "#0000FF", alpha = 0.15),
    sim_median_line = pmx_line(color = "#000000", linetype = "dashed", linewidth = 1),
    sim_median_ci   = pmx_ribbon(fill = "#FF0000", alpha = 0.3),
    loq_line        = pmx_line(color = "#990000", linetype = "dashed", linewidth = 0.5)
  )
  user <- compact(list(obs_point = obs_point, obs_median_line = obs_median_line,
                       obs_pi_line = obs_pi_line, sim_pi_line = sim_pi_line,
                       sim_pi_ci = sim_pi_ci, sim_pi_area = sim_pi_area,
                       sim_median_line = sim_median_line, sim_median_ci = sim_median_ci,
                       loq_line = loq_line))
  pmx_theme(merge_theme(user, defaults), subclass = "plot_vpc_theme")
}
