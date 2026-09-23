# pmxhelpr (development version)

## New features

### New functions

* `plot_forest()` generates a forest plot of test/reference comparisons
  for visualizing PK/PD relationships and covariate impact for a single
  PK parameter per call. Returns a plain `ggplot` object (composable
  with `+ geom_*()` / `+ theme()` / `+ coord_*()`); numeric
  `est [lo, hi]` annotation is rendered as right-side secondary-axis
  text (via `geom_text` + `coord_cartesian(clip = "off")`) rather than
  a side table; the primary y-axis carries the covariate level only. Covariate name is a grouping dimension
  (`facet_grid` row with left-side strip label) and the designated
  reference row (controlled by the `cov_name_ref` argument, default
  `"Reference"`) always sorts to the top. Optionally, supply the `cov_level_ref`
  argument — a named vector mapping covariate names to their
  per-covariate reference labels (e.g.,
  `c(FOOD = "Fasted", WTBL = "70 kg")`) — to disperse the Reference row into
  each non-Reference panel under that label instead of rendering as a
  standalone facet. Covariates not named in `cov_level_ref` show no
  dispersed Reference row in their panel. Within each panel, the
  y-axis tick labels are auto-sorted: numeric-parseable labels (e.g.,
  `"50 kg"`, `"70 kg"`, `"90 kg"`) sort in ascending numerical order
  so the highest value lands at the top; non-numeric labels (e.g.,
  `"Fasted"`, `"Fed"`) keep the reverse data-encounter order (Reference row
  on top). Select which exposure
  metric to render via the `metric` argument; if `stats` carries
  exactly one metric, it is picked automatically.
* `df_forest()` aggregates replicate draws into per-group point and CI
  estimates. Returns a `forest_stats` / `pmx_stats` container with
  canonical `est`, `lo`, `hi`, and `ci_label` columns. The `cov_level_var`
  column drives the y-axis tick label (the covariate name lives in
  the facet strip; `ci_label` is rendered as a right-side secondary-axis
  text layer by `plot_build_forest()`).
* `plot_build_forest()` renders a forest ggplot from any `forest_stats`
  container; most users still go through `plot_forest()`.
* `style_forest()` is the `ggstylekit::style_spec()` preset for
  `plot_forest()` with roles `point`, `errorbar`, `ref_line`, and `ref_band`.
* `is_forest_stats()` predicate for the new `forest_stats` subclass.

### New datasets

* `data_sad_pkforest` ships as long-form example data for `df_forest()` /
  `plot_forest()`: per-replicate bootstrap-PK draws of absolute and
  test/reference PK metrics across body-weight and food-effect covariate
  scenarios. Column defaults of `df_forest()` / `plot_forest()`
  (`metric`, `cov_var`, `cov_val`, `value`) match this dataset for
  drop-in use; pipe through `df_forest(replicate_var = "SIM")` to
  produce a `forest_stats` container.

### Dual input for per-covariate reference labels

* `df_forest()` / `plot_forest()` now accept a `cov_ref` column on
  `data` as an alternative to the `cov_level_ref` named-vector argument.
  When the column is present, per-row reference labels are pulled from
  it directly (supporting heterogeneous within-cov_name labels that the
  named-vector form can't express); `cov_level_ref` is then ignored with
  an informational `message()`. Existing `cov_level_ref` callers are
  unaffected.

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
  and re-plot with different `style`, `ref`, or `ref_band` settings.
  Pipeline arguments (`metric_name_var`, `cov_name_var`, `cov_level_var`,
  `metric_value_var`, `replicate_var`, `statistic`, `ci`, `sigdigits`,
  `cov_name_ref`, `cov_level_ref`) cannot be honored on the precomputed
  path and error with a clear message. The plot-only `metric` argument is
  accepted on both paths so a single `forest_stats` container can be
  re-rendered for different exposure metrics.

* `df_forest()` is draws-only: the replicate-aggregation path is the
  function's sole purpose. Arguments for an externally pre-summarized
  input (`est_var`, `lo_var`, `hi_var`) are removed; `replicate_var` is
  required. Users with pre-summarized data from external pipelines build
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
