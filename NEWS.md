# pmxhelpr (development version)

## New features

### Forest plots

* `plot_forest()` draws a forest plot of test/reference comparisons for one
  exposure metric, faceted by covariate, with an `Estimate (CI)` text column
  on the right. 
  
  Key features:
  + Reference line (`ref`) and optional band (`ref_band`, `ref_band_type` of
    `"multiplicative"`, `"additive"`, or `"absolute"`).
  + Accepts raw draws or a precomputed `forest_stats` container, so a
    summary can be re-plotted without re-aggregating.
  + `ref_layout` draws the reference row in its own panel
    (`"consolidated"`) or in each covariate panel (`"distributed"`), so one
    `forest_stats` container renders either layout.
  + Returns a plain `ggplot` object.
  
* `df_forest()` summarizes replicate draws into a plot-ready `forest_stats`
  container (estimate, CI bounds, formatted labels, `is_ref` flag, `layout`) in display
  order. Reference rows are identified by name (`cov_name_ref`);
  `cov_level_ref` supplies the per-covariate reference labels that the
  distributed layout draws them under.
* `df_forest_preprocess()` reshapes wide replicate simulation output (one row
  per scenario per replicate) into the long draws `df_forest()` consumes,
  inferring each scenario's covariate from its difference to the reference
  (`cov_val_ref`) and optionally adding test/reference ratios (`ratio`).
* `plot_build_forest()` renders any `forest_stats` container, including
  manually built ones from external pipelines.
* `style_forest()` is the `ggstylekit` style preset for `plot_forest()`;
  `colors` keyed by covariate name color each covariate panel.
* `is_forest_stats()` tests for a `forest_stats` object.

### New arguments

* `plot_dvtime()` and `plot_dvconc()` gain `show_obs` to omit the observed
  point layer. Unlike `alphas = c(obs_point = 0)`, hidden points no longer
  affect the axis limits.

### New datasets

* `data_sad_pkforest_sim`: example raw replicate simulation output (500
  bootstrap replicates of steady-state `CMAX`, `CMIN`, `AUC`, `CAVG` under
  six covariate scenarios).
* `data_sad_pkforest`: `data_sad_pkforest_sim` processed by
  `df_forest_preprocess()`, ready for `df_forest()` / `plot_forest()`.

## Changed behavior

* Character `col_var` labels in `plot_dvconc()`, `plot_dvtime()`, and
  `plot_gof()` now order by a number anywhere in the label (e.g.
  `"Dose 5 mg"` before `"Dose 10 mg"`), not only a leading one. Label sets
  with any number-free value keep first-appearance order; factors are
  unaffected.
* `min_bin_count` in `plot_vpc_cont()`, `plot_vpc_cens()`, and
  `plot_build_vpc()` now defaults to `0` (was `1`), so no bins are filtered
  by default. Prediction-corrected simulated quantiles (`pc_sim_*`) are `NA`
  in bins with no quantifiable observations, so pcVPCs do not draw them.

## Bug fixes

* `df_doseprop()` / `plot_doseprop()` judge dose proportionality on unrounded
  confidence bounds, so bounds within rounding distance of 1 are no longer
  misclassified as "Dose-proportional".
* `df_doseprop()` `PowerCI` labels pad to `sigdigits` significant digits
  (e.g. `Power: 1.00 (90% CI 0.900-1.20)`) and no longer use scientific
  notation.

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
