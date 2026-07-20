# ---------------------------------------------------------------------------
# Family style presets
#
# Each `style_*()` returns a `ggstylekit::style_spec` pre-filled with a plot
# family's pmxhelpr domain defaults, keyed by the family's role/series names.
# Pass any `style_spec()` field via `...` to override a default wholesale (a
# whole per-series map is replaced, matching `ggstylekit::set_style()`); for
# entry-wise tweaks on a finished plot use `ggstylekit::restyle_plot()`.
#
# Replaces the retired `plot_*_theme()` / `pmx_*` element system.
# ---------------------------------------------------------------------------


#' Concentration-time plot style (`plot_dvtime`)
#'
#' Default [ggstylekit::style_spec()] for [plot_dvtime()]. Series names are the
#' plot roles: `obs_point`, `obs_line` (spaghetti), `cent_point`, `cent_line`,
#' `cent_errorbar`, `ref_line`, `loq_line`.
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding the
#'   defaults below wholesale (e.g. `shapes = c(obs_point = 16)`, `title = "..."`).
#'
#' @family exploratory analysis
#' @return A `ggstylekit_style_spec` object.
#' @export
#' @examples
#' style_dvtime()
#' style_dvtime(shapes = c(obs_point = 16, cent_point = 16))
style_dvtime <- function(...) {
  defaults <- list(
    shapes     = c(obs_point = 1, cent_point = 16),
    sizes      = c(obs_point = 0.75, cent_point = 1.25),
    alphas     = c(obs_point = 0.5, obs_line = 0.5, cent_point = 0, cent_line = 1,
                   cent_errorbar = 1, ref_line = 1, loq_line = 1),
    linewidths = c(obs_line = 0.5, cent_line = 0.75, cent_errorbar = 0.75,
                   ref_line = 0.5, loq_line = 0.5),
    linetypes  = c(obs_line = "solid", cent_line = "solid",
                   ref_line = "dashed", loq_line = "dashed"),
    theme      = pmx_house_theme()
  )
  build_style(defaults, list(...))
}


#' Population overlay GOF plot style (`plot_gof`)
#'
#' Default [ggstylekit::style_spec()] for [plot_gof()]. The DV/PRED/IPRED (and
#' OBS) overlays are colour-mapped by label via the `colors` map; the remaining
#' fixed aesthetics use the role series names as in [style_dvtime()].
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding defaults
#'   wholesale (e.g. `colors = c(DV = "black")`).
#'
#' @family goodness-of-fit
#' @return A `ggstylekit_style_spec` object.
#' @export
#' @examples
#' style_gof()
#' style_gof(colors = c(OBS = "darkgrey", DV = "blue", PRED = "purple", IPRED = "green"))
style_gof <- function(...) {
  defaults <- list(
    colors     = c(OBS = "darkgrey", DV = "blue", PRED = "red", IPRED = "green"),
    shapes     = c(obs_point = 1, cent_point = 16),
    sizes      = c(obs_point = 0.75, cent_point = 1.25),
    alphas     = c(obs_point = 0.5, obs_line = 0.75, cent_point = 0, cent_line = 1,
                   cent_errorbar = 1, ref_line = 1, loq_line = 1),
    linewidths = c(obs_line = 0.5, cent_line = 0.75, cent_errorbar = 0.75,
                   ref_line = 0.5, loq_line = 0.5),
    linetypes  = c(obs_line = "solid", cent_line = "solid",
                   ref_line = "dashed", loq_line = "dashed"),
    theme      = pmx_house_theme()
  )
  build_style(defaults, list(...))
}


#' Response versus concentration plot style (`plot_dvconc`)
#'
#' Default [ggstylekit::style_spec()] for [plot_dvconc()]. Series names:
#' `obs_point`, `ref_line`, `loess`, `linear`. The `loess`/`linear` trend lines
#' are `geom_smooth` (line entity); their SE ribbon fill/alpha come from
#' `line_fill`/`fill_alpha`.
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding defaults
#'   wholesale.
#'
#' @family exploratory analysis
#' @return A `ggstylekit_style_spec` object.
#' @export
#' @examples
#' style_dvconc()
style_dvconc <- function(...) {
  defaults <- list(
    shapes     = c(obs_point = 1),
    sizes      = c(obs_point = 1.25),
    # loess/linear alpha is the SE-ribbon alpha (the fitted line stays opaque).
    alphas     = c(obs_point = 0.5, ref_line = 1, loess = 0.4, linear = 0.4),
    colors     = c(loess = "black", linear = "black"),
    linewidths = c(ref_line = 0.5, loess = 1, linear = 1),
    linetypes  = c(ref_line = "dashed", loess = "solid", linear = "dashed"),
    line_fill  = "lightgrey",
    theme      = pmx_house_theme()
  )
  build_style(defaults, list(...))
}


#' Dose-proportionality plot style (`plot_doseprop`)
#'
#' Default [ggstylekit::style_spec()] for [plot_doseprop()]. Series names:
#' `obs_point`, `linear`. Log-log axes and per-metric facets are set by the
#' builder via `logx`/`logy`/`facet` fields.
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding defaults
#'   wholesale.
#'
#' @family dose proportionality
#' @return A `ggstylekit_style_spec` object.
#' @export
#' @examples
#' style_doseprop()
style_doseprop <- function(...) {
  defaults <- list(
    shapes     = c(obs_point = 1),
    sizes      = c(obs_point = 2),
    # linear alpha is the SE-ribbon alpha (the fitted line stays opaque).
    alphas     = c(obs_point = 0.7, linear = 0.4),
    colors     = c(linear = "black"),
    linewidths = c(linear = 1),
    linetypes  = c(linear = "solid"),
    line_fill  = "lightgrey",
    theme      = pmx_house_theme()
  )
  build_style(defaults, list(...))
}


#' VPC plot style (`plot_vpc_cont` / `plot_vpc_cens`)
#'
#' Default [ggstylekit::style_spec()] for the VPC family. Line/point roles
#' (`obs_point`, `obs_median_line`, `obs_pi_line`, `sim_pi_line`,
#' `sim_median_line`, `loq_line`) are styled by `style_plot()`. The three ribbon
#' roles (`sim_pi_ci`, `sim_pi_area`, `sim_median_ci`) are `geom_ribbon`
#' (outside ggstylekit's entity registry), so their `fill`/`alpha` are set
#' inline by the builder, read from these maps via `series_aes()`.
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding defaults
#'   wholesale.
#'
#' @family vpc
#' @return A `ggstylekit_style_spec` object.
#' @export
#' @examples
#' style_vpc()
style_vpc <- function(...) {
  defaults <- list(
    colors     = c(obs_point = "#0000FF", obs_median_line = "#FF0000",
                   obs_pi_line = "#0000FF", sim_pi_line = "#000000",
                   sim_median_line = "#000000", loq_line = "#990000"),
    fill       = c(sim_pi_ci = "#0000FF", sim_pi_area = "#0000FF",
                   sim_median_ci = "#FF0000"),
    shapes     = c(obs_point = 1),
    sizes      = c(obs_point = 1),
    alphas     = c(obs_point = 0.7, sim_pi_ci = 0.15, sim_pi_area = 0.15,
                   sim_median_ci = 0.3),
    linewidths = c(obs_median_line = 1, obs_pi_line = 0.5, sim_pi_line = 1,
                   sim_median_line = 1, loq_line = 0.5),
    linetypes  = c(obs_median_line = "solid", obs_pi_line = "dashed",
                   sim_pi_line = "dotted", sim_median_line = "dashed",
                   loq_line = "dashed"),
    theme      = pmx_house_theme(white_panel = TRUE)
  )
  build_style(defaults, list(...))
}
