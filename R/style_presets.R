# ---------------------------------------------------------------------------
# Family style presets
#
# Each `style_*()` returns a `ggstylekit::style_spec` pre-filled with a plot
# family's pmxhelpr domain defaults, keyed by the family's role/series names.
# Pass any `style_spec()` field via `...` to override a default. Per-series maps
# (`colors`, `fill`, `shapes`, `sizes`, `linetypes`, `linewidths`, `alphas`)
# merge entry-wise onto the preset defaults, so setting one role leaves the
# others unchanged; all other fields replace their default wholesale.
#
# Replaces the retired `plot_*_theme()` / `pmx_*` element system.
# ---------------------------------------------------------------------------


#' Concentration-time plot style (`plot_dvtime`)
#'
#' Default [ggstylekit::style_spec()] for [plot_dvtime()]. Series names are the
#' plot roles: `obs_point`, `obs_line` (spaghetti), `cent_point`, `cent_line`,
#' `cent_errorbar`, `ref_line`, `loq_line`.
#'
#' Like every pmxhelpr preset, it places legend titles above the keys
#' (`legend.title.position = "top"`) and applies the house `theme_bw()`-derived
#' theme.
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding the
#'   preset defaults; call the preset with no arguments to view them. Per-series maps (`colors`, `fill`, `shapes`, `sizes`,
#'   `linetypes`, `linewidths`, `alphas`) merge entry-wise onto the defaults, so
#'   setting one role leaves the others unchanged (e.g. `shapes = c(obs_point =
#'   16)`); all other fields replace their default wholesale (e.g. `title = "..."`).
#'   A per-series map may instead be a palette `function(n)` (e.g.
#'   `function(n) grDevices::hcl.colors(n, "Viridis")`), which `ggstylekit` calls
#'   with the number of mapped groups; a palette has no entries to merge, so it
#'   replaces the default map.
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
    # Legend titles above the keys, the house look for bottom/top legends.
    legend.title.position = "top",
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
#' Like every pmxhelpr preset, it places legend titles above the keys
#' (`legend.title.position = "top"`) and applies the house `theme_bw()`-derived
#' theme.
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding the
#'   preset defaults; call the preset with no arguments to view them. Per-series maps merge entry-wise (e.g. `colors = c(DV =
#'   "black")` leaves the other overlay colors unchanged); all other fields
#'   replace their default wholesale.
#'   A per-series map may instead be a palette `function(n)` (e.g.
#'   `function(n) grDevices::hcl.colors(n, "Viridis")`), which `ggstylekit` calls
#'   with the number of mapped groups; a palette has no entries to merge, so it
#'   replaces the default map.
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
    # Legend titles above the keys, the house look for bottom/top legends.
    legend.title.position = "top",
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
#' Like every pmxhelpr preset, it places legend titles above the keys
#' (`legend.title.position = "top"`) and applies the house `theme_bw()`-derived
#' theme.
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding the
#'   preset defaults; call the preset with no arguments to view them. Per-series maps merge entry-wise onto the defaults; all
#'   other fields replace their default wholesale.
#'   A per-series map may instead be a palette `function(n)` (e.g.
#'   `function(n) grDevices::hcl.colors(n, "Viridis")`), which `ggstylekit` calls
#'   with the number of mapped groups; a palette has no entries to merge, so it
#'   replaces the default map.
#'
#' @family exploratory analysis
#' @return A `ggstylekit_style_spec` object.
#' @export
#' @examples
#' style_dvconc()
#' style_dvconc(colors = c(loess = "blue", linear = "red"))
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
    # Legend titles above the keys, the house look for bottom/top legends.
    legend.title.position = "top",
    theme      = pmx_house_theme()
  )
  build_style(defaults, list(...))
}


#' Dose-proportionality plot style (`plot_doseprop`)
#'
#' Default [ggstylekit::style_spec()] for [plot_doseprop()]. Series names:
#' `obs_point`, `linear`. The builder draws the log-log axes and the
#' per-metric facet itself, so the `logx`, `logy`, and `facet` fields are
#' ignored; the facet layout fields `facet_scales` (default `"free"`),
#' `facet_nrow`, and `facet_ncol` are honored.
#'
#' Like every pmxhelpr preset, it places legend titles above the keys
#' (`legend.title.position = "top"`) and applies the house `theme_bw()`-derived
#' theme.
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding the
#'   preset defaults; call the preset with no arguments to view them. Per-series maps merge entry-wise onto the defaults; all
#'   other fields replace their default wholesale.
#'   A per-series map may instead be a palette `function(n)` (e.g.
#'   `function(n) grDevices::hcl.colors(n, "Viridis")`), which `ggstylekit` calls
#'   with the number of mapped groups; a palette has no entries to merge, so it
#'   replaces the default map.
#'
#' @family dose proportionality
#' @return A `ggstylekit_style_spec` object.
#' @export
#' @examples
#' style_doseprop()
#' style_doseprop(colors = c(linear = "navy"), facet_ncol = 1)
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
    # Legend titles above the keys, the house look for bottom/top legends.
    legend.title.position = "top",
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
#' Stratification facets are built by the VPC plotting functions from
#' `strat_var` (not by `style_plot()`), but the `facet_scales`, `facet_nrow`,
#' and `facet_ncol` fields are honored; `facet` is ignored when `strat_var` is
#' supplied.
#'
#' Like every pmxhelpr preset, it places legend titles above the keys
#' (`legend.title.position = "top"`) and applies the house `theme_bw()`-derived
#' theme.
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding the
#'   preset defaults; call the preset with no arguments to view them. Per-series maps merge entry-wise onto the defaults; all
#'   other fields replace their default wholesale.
#'   A per-series map may instead be a palette `function(n)` (e.g.
#'   `function(n) grDevices::hcl.colors(n, "Viridis")`), which `ggstylekit` calls
#'   with the number of mapped groups; a palette has no entries to merge, so it
#'   replaces the default map.
#'
#' @family vpc
#' @return A `ggstylekit_style_spec` object.
#' @export
#' @examples
#' style_vpc()
#' style_vpc(fill = c(sim_pi_ci = "#3388cc"), facet_scales = "free_y")
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
    # Legend titles above the keys, the house look for bottom/top legends.
    legend.title.position = "top",
    theme      = pmx_house_theme(white_panel = TRUE)
  )
  build_style(defaults, list(...))
}
