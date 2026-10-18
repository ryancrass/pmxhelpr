# pmxhelpr (development version)

## New features

### New functions

* `plot_forest()` generates a forest plot of test/reference comparisons
  for visualizing PK/PD relationships and covariate impact for a single
  PK parameter per call. Returns a plain `ggplot` object (composable
  with `+ geom_*()` / `+ theme()` / `+ coord_*()`); the numeric
  `est [lo, hi]` column is rendered as the right-hand secondary y-axis
  text under a header built from the statistic and CI width (e.g.
  `Median [90% CI]`, drawn as the plot tag) rather than a side table;
  the primary y-axis carries the covariate level only. Rows flagged
  `is_ref` (the dispersed per-covariate reference label, or the standalone
  Reference panel) draw as an open circle through the `ref_point` style
  role and show the estimate alone in the column. `show_legend = TRUE`
  adds a legend keying the reference line and shaded band (default
  `FALSE`). Covariate name is a grouping dimension (`facet_grid` row with
  left-side strip label). Rows draw top to bottom in the order of the
  `forest_stats` frame, and each row is its own y position, so covariate
  level labels may repeat across panels. Select which exposure metric to
  render via the `metric` argument; if `stats` carries exactly one metric,
  it is picked automatically. The reference line defaults to
  `ref = "reference"`, the estimate of the metric's reference row (`1` for
  a ratio, the reference exposure for a raw metric); `ref_band` is placed
  around it per `ref_band_type`: `"multiplicative"` (default,
  `c(0.8, 1.25)` is bioequivalence on ratio and exposure scales),
  `"additive"` (`c(-10, 10)` around a reference change for difference
  metrics), or `"absolute"`.
* `df_forest()` aggregates replicate draws into a plot-ready
  `forest_stats` / `pmx_stats` container: one row per plotted point with
  canonical `est`, `lo`, `hi`, `ci_label`, and `is_ref` columns, sorted
  into display order; `ci_label` pads every value to `sigdigits`
  significant digits (`1.00 [0.950, 1.05]`). The designated reference row
  (`cov_name_ref`, default `"Reference"`) sorts to the top as its own
  panel, or, given `cov_level_ref` (a named vector mapping covariate names
  to their per-covariate reference labels, e.g.
  `c(FOOD = "Fasted", WTBL = "70 kg")`), is dispersed into each named
  covariate panel under that label instead. Row order follows factor
  levels on the input columns when present, otherwise first appearance in
  the data; within a covariate, labels that start with a number sort
  ascending (`50 kg` above `90 kg`) and any other set keeps the reference
  label on top followed by the data order. Reorder the rows of
  `stats$stats` to change the plot.
* `plot_build_forest()` renders a forest ggplot from any `forest_stats`
  container; most users still go through `plot_forest()`.
* `style_forest()` is the `ggstylekit::style_spec()` preset for
  `plot_forest()` with roles `point`, `errorbar`, `ref_point`, `ref_line`,
  and `ref_band`; its forest panel theme styles the estimate column
  (`axis.text.y.right`) and its header (`plot.tag`).
* `is_forest_stats()` predicate for the new `forest_stats` subclass.

### New datasets

* `data_sad_pkforest` ships as long-form example data for `df_forest()` /
  `plot_forest()`: per-replicate bootstrap-PK draws of absolute and
  test/reference PK metrics across body-weight and food-effect covariate
  scenarios. Column defaults of `df_forest()` / `plot_forest()`
  (`metric`, `cov_var`, `cov_val`, `value`) match this dataset for
  drop-in use; pipe through `df_forest()` to produce a `forest_stats`
  container.

### Per-covariate coloring in forest plots

* `style_forest()` accepts `colors` entries keyed by covariate name (e.g.
  `style_forest(colors = c(FOOD = "darkgreen", WTBL = "darkblue"))`). When at
  least one covariate present in the plotted data is named, `plot_forest()`
  colors point and errorbar layers by covariate (facet) via
  `scale_color_manual()` and suppresses the legend, since facet strips
  already label each covariate. Covariates absent from the map render in
  `grey50` and trigger a warning. The default map names no covariates
  (single-color behavior).

### Dual-mode pipelines (replot without recompute)

* `plot_forest()` accepts the `forest_stats` container returned by
  `df_forest()` directly. Pass a precomputed result to skip aggregation
  and re-plot with different `style`, `ref`, `ref_band`, or
  `ref_band_type` settings.
  Pipeline arguments (`metric_name_var`, `cov_name_var`, `cov_level_var`,
  `metric_value_var`, `statistic`, `ci`, `sigdigits`,
  `cov_name_ref`, `cov_level_ref`) cannot be honored on the precomputed
  path and error with a clear message. The plot-only `metric` argument is
  accepted on both paths so a single `forest_stats` container can be
  re-rendered for different exposure metrics.

* `df_forest()` is draws-only: it aggregates every row of a
  `(metric, covariate, level)` group, so no replicate index column is
  needed. Users with pre-summarized data from external pipelines build
  a `forest_stats` container manually and render via
  `plot_build_forest()`.

# pmxhelpr 0.6.0

Plot styling is now powered by the `ggstylekit` package, replacing the
bespoke `pmx_*` element/theme system introduced in 0.5.0. 

## New features

* Plot aesthetics are controlled with `ggstylekit::style_spec()`. Each plot
  family has a `style_*()` preset — `style_dvtime()`, `style_gof()`,
  `style_dvconc()`, `style_doseprop()`, and `style_vpc()` — that returns a
  pre-filled style spec for the new `style` argument (replaces `theme`). 
* `restyle_plot()`, `reveal()`, `combine_styled_plots()`, and `legend_spec()`
  are re-exported from ggstylekit.
* Error bar cap width is a style field: `style_dvtime(errorbar_width = ...)`
  and `style_gof(errorbar_width = ...)`. When unset, `plot_dvtime()` and
  `plot_gof()` default it to 2.5% of the maximum nominal time.
* `plot_vpc_cont()`, `plot_vpc_cens()`, and `plot_doseprop()` take their facet
  layout from the style (`facet_scales`, `facet_nrow`, `facet_ncol`). These
  builders facet internally (by `strat_var` or by metric), so the style's
  `facet` field is ignored when they do.
* `plot_vpc_legend()` honors `legend.title.position` and `legend.title.hjust`
  from the style, so the standalone legend matches its plot.
* All `style_*()` presets place legend titles above the keys
  (`legend.title.position = "top"`).
* Plot functions abort early with an informative message when `style` is not
  a `ggstylekit::style_spec()` object.
* Character `col_var` values with numeric labels (e.g. `"5 mg"`, `"20 mg"`,
  `"100 mg"`) are ordered by value, not alphabetically, in legends and
  per-series color assignment (`plot_dvtime()`, `plot_dvconc()`).
* `ggstylekit (>= 0.4.0)` is a new dependency (Imports).

## Breaking changes

### Removed functions

* The `pmx_*` element constructors are removed: `pmx_point()`, `pmx_line()`,
  `pmx_ribbon()`, `pmx_errorbar()`, `pmx_trend()`, `pmx_style()`, and
  `pmx_color()`.
* The theme factories are removed: `plot_dvtime_theme()`,
  `plot_dvconc_theme()`, `plot_gof_theme()`, `plot_doseprop_theme()`, and
  `plot_vpc_theme()`. Use the corresponding `style_*()` preset instead.
* The theme class system is removed: `pmx_theme()`, `is_pmx_element()`,
  `is_pmx_theme()`, and the `+` / `print` methods for `pmx_element` and
  `pmx_theme`.

### Renamed and changed arguments

* The `theme` argument of `plot_dvtime()`, `plot_dvconc()`, `plot_gof()`,
  `plot_doseprop()`, `plot_vpc_cont()`, `plot_vpc_cens()`, `plot_build_vpc()`,
  `plot_build_doseprop()`, and `plot_vpc_legend()` is renamed to `style` and now
  takes a `ggstylekit::style_spec()` object, typically from a `style_*()` 
  preset.
* Error bar cap width moved out of the theme (`pmx_errorbar(width = ...)`)
  to the `errorbar_width` field of `style_dvtime()` and `style_gof()`.

## Migration

Replace `theme = plot_<fn>_theme(<role> = pmx_*(<field> = <value>))` with
`style = style_<fn>(<map> = c(<role> = <value>))`, where `<map>` is the
ggstylekit per-series map for that aesthetic (`colors`, `fill`, `shapes`,
`sizes`, `linetypes`, `linewidths`, `alphas`):

``` r
# before (0.5.x)
plot_dvtime(data, theme = plot_dvtime_theme(obs_point = pmx_point(alpha = 0)))

# after (0.6.0)
plot_dvtime(data, style = style_dvtime(alphas = c(obs_point = 0)))
```

See the *Plot Styling and Aesthetics* vignette for the role vocabulary; call
any `style_*()` preset with no arguments to view its defaults.

# pmxhelpr 0.5.1

* Bug fix in `plot_dvtime()` which was masking reference line, legend, and 
caption layers when character aliases where passed to argument `loq_method` in 
place of numeric values.
