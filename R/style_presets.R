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
#' The error bar cap width is the `errorbar_width` field (e.g.
#' `style_dvtime(errorbar_width = 10)`, in x-axis units). The preset leaves it
#' unset, and [plot_dvtime()] then defaults it to 2.5% of the maximum nominal
#' time so the caps scale with the time axis.
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
#' style_dvtime(errorbar_width = 10)
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
#' The error bar cap width is the `errorbar_width` field (e.g.
#' `style_gof(errorbar_width = 10)`, in x-axis units). The preset leaves it
#' unset, and [plot_gof()] then defaults it to 2.5% of the maximum nominal time.
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
#' roles (`sim_pi_ci`, `sim_pi_area`, `sim_median_ci`) are `geom_ribbon` layers
#' whose `fill`/`alpha` the VPC builder sets inline, read from these maps via
#' `series_aes()` (inline aesthetics take precedence over the `area_*` fields).
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


#' Forest plot style (`plot_forest`)
#'
#' Default [ggstylekit::style_spec()] for [plot_forest()] /
#' [plot_build_forest()]. Series names are the plot roles: `point` (the
#' estimate), `errorbar` (the CI line range), `ref_point` (the estimate of a
#' reference row: the distributed per-covariate reference label or the
#' standalone reference panel; an open circle by default), `ref_line` (the
#' vertical reference line drawn at `ref`), and `ref_band` (the shaded
#' equivalence band drawn at `ref_band`; a `geom_rect` annotation whose `fill`
#' and `alpha` come from the `fill` / `alphas` maps).
#'
#' Per-covariate coloring is opt-in through the same `colors` map: add entries
#' keyed by covariate name (the values of the `cov_name_var` column, e.g.
#' `colors = c(FOOD = "darkgreen", WTBL = "darkblue")`). When at least one
#' covariate present in the plotted data has an entry, the builder maps point
#' and error bar color to the covariate (facet) and suppresses the legend,
#' since the facet strips already label each covariate. Covariates without an
#' entry take colours from the house palette, as for `col_var` in
#' [plot_dvtime()]. The role
#' entries (`point`, `errorbar`, `ref_line`) are then ignored for the mapped
#' layers.
#'
#' The builder draws the covariate row facet itself (`facet_grid(rows =
#' cov_name, switch = "y")`), so the `facet*` fields are ignored. It labels
#' the x-axis with the rendered metric unless the `xlabel` field is supplied,
#' and draws no title unless the `title` field is supplied.
#' The house theme uses the forest panel variant of `theme_bw()`: a faint
#' vertical ruler, no horizontal gridlines, covariate panels stacked without
#' spacing into one frame, bold top-left covariate strips placed outside the y-axis,
#' and the estimate column as right-hand axis text under a bold header drawn
#' as the right-aligned subtitle on the top edge of the panels (style them
#' through `theme = ` with `axis.text.y.right` and `plot.subtitle`). Set
#' `logx = TRUE` for a log x-axis.
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding the
#'   preset defaults; call the preset with no arguments to view them. Per-series maps merge entry-wise onto the defaults (e.g.
#'   `shapes = c(point = 18)` leaves the other roles unchanged; `colors =
#'   c(FOOD = "red")` adds a covariate color while keeping the role colors);
#'   all other fields replace their default wholesale. A palette
#'   `function(n)` in `colors` colours every covariate (called with the
#'   number of covariates); it replaces the role entries, which then fall back
#'   to the ggplot2 defaults.
#'
#' @seealso [style_pmforest()] for the look of Metrum's `pmforest` package.
#' @family forest plot
#' @return A `ggstylekit_style_spec` object.
#' @export
#' @examples
#' style_forest()
#' style_forest(shapes = c(point = 18), sizes = c(point = 3))
#' style_forest(fill = c(ref_band = "skyblue"), alphas = c(ref_band = 0.2))
#' style_forest(shapes = c(ref_point = 16), colors = c(ref_point = "grey50"))
#' style_forest(colors = c(FOOD = "firebrick", WTBL = "steelblue", Reference = "grey20"))
#' style_forest(logx = TRUE)
style_forest <- function(...) {
  defaults <- list(
    colors     = c(point = "black", errorbar = "black", ref_point = "black",
                   ref_line = "grey30"),
    fill       = c(ref_point = "white", ref_band = "grey80"),
    shapes     = c(point = 16, ref_point = 21),
    # Compact markers; the thinner CI line keeps the estimate legible when the
    # interval is narrow.
    sizes      = c(point = 2.2, ref_point = 2.2),
    alphas     = c(point = 1, errorbar = 1, ref_point = 1, ref_line = 1,
                   ref_band = 0.3),
    linewidths = c(errorbar = 0.6, ref_line = 0.6),
    linetypes  = c(errorbar = "solid", ref_line = "dashed"),
    # The builder draws no legend; these place any a caller adds below the
    # panels, titles above the keys: the house look for bottom/top legends.
    legend.position = "bottom",
    legend.title.position = "top",
    theme      = pmx_house_theme(forest_panel = TRUE)
  )
  build_style(defaults, list(...))
}


#' pmforest-style forest plot style (`plot_forest`)
#'
#' An alternative [ggstylekit::style_spec()] for [plot_forest()] /
#' [plot_build_forest()] that reproduces the default look of Metrum Research
#' Group's [pmforest](https://github.com/metrumresearchgroup/pmforest/)
#' package: black-outlined filled diamonds (shape 23, size 3.5) and uncapped
#' interval lines coloured per covariate with the ggplot2 default hue
#' palette, a dot-dash reference line, a `grey35` equivalence band at alpha
#' 0.2, and a `theme_bw()` panel with grey major vertical gridlines, ~9.9 pt
#' text, a ~7.9 pt estimate column, and no legend.
#'
#' Series roles are those of [style_forest()]. The plot keeps the
#' `plot_forest()` layout: covariate facet strips (pmforest labels groups on
#' the y-axis), the `est (lo, hi)` estimate column (pmforest prints
#' `est [lo, hi]`), reference rows drawn as open diamonds (pmforest draws
#' no point for them), and no minor gridlines (add them through `theme = `
#' with `panel.grid.minor.x`).
#'
#' @param ... Fields passed to [ggstylekit::style_spec()], overriding the
#'   preset defaults; call the preset with no arguments to view them.
#'   Per-series maps merge entry-wise onto the defaults (e.g. `sizes =
#'   c(point = 3)`). The default `colors` is a hue palette `function(n)`, so
#'   passing `colors` replaces it wholesale: a different palette function, or
#'   entries keyed by covariate name (covariates without an entry take the
#'   house palette). The point fill follows the covariate colour while the
#'   point shape is a fill shape (21-25); the `point` colour is the outline.
#'
#' @family forest plot
#' @return A `ggstylekit_style_spec` object.
#' @export
#' @examples
#' style_pmforest()
#' style_pmforest(fill = c(ref_band = "skyblue"))
#' style_pmforest(logx = TRUE)
style_pmforest <- function(...) {
  defaults <- list(
    # ggplot2's default discrete hue palette (`scales::hue_pal()`).
    colors     = function(n) {
      grDevices::hcl(h = seq(15, 375, length.out = n + 1)[seq_len(n)], c = 100, l = 65)
    },
    fill       = c(ref_point = "white", ref_band = "grey35"),
    shapes     = c(point = 23, ref_point = 23),
    sizes      = c(point = 3.5, ref_point = 3.5),
    alphas     = c(point = 1, errorbar = 1, ref_point = 1, ref_line = 1,
                   ref_band = 0.2),
    linewidths = c(errorbar = 0.5, ref_line = 0.5),
    linetypes  = c(errorbar = "solid", ref_line = "dotdash"),
    legend.position = "none",
    # pmforest sizes text in mm (`text_size = 3.5`, table text 0.8x that);
    # 0.352777778 mm per pt.
    theme      = pmx_house_theme(forest_panel = TRUE) + ggplot2::theme(
      text               = ggplot2::element_text(size = 3.5 / 0.352777778),
      panel.background   = ggplot2::element_rect(fill = NA, colour = "grey20"),
      panel.grid.major.x = ggplot2::element_line(colour = "grey"),
      axis.text.y.right  = ggplot2::element_text(
        size = 2.8 / 0.352777778, hjust = 0, colour = "black",
        margin = ggplot2::margin(l = 8)
      )
    )
  )
  build_style(defaults, list(...))
}
